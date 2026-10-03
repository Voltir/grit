package grit.eval.harness.pull

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError, Question}
import grit.core.id.{QuestionName, ShadowName, ShadowRef}
import grit.core.message.Usage
import grit.core.review.Considered
import grit.core.store.{Focus, Origin, Position, StoreError, Tx}
import grit.core.triage.{KnowledgeSources, ShadowAnswers, Shadowed, Tags, TriageStore}
import grit.dbos.engine.{Build, Reader}
import grit.eval.harness.corpus.{Case, CaseId, Digest, Failure}
import grit.eval.harness.label.{Rated, Verdicts}
import grit.eval.harness.log.{CacheKey, Footer, Header, Log, Outcome, Row, Suite, Weights}
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}

/** What a deployment's database kept of the heard messages tagged since a time, as run logs
  * the scorers read beside offline runs: live triage's tags ([[Pull.Kept]]), and each named
  * shadow's answers. Rows are of the corpus's cases alone, in its order, one a case, each with
  * the focus its message was said at, never cached; text-free.
  */
object Pull {

  /** The variant a pulled log of live triage's tags names: `kept`. */
  val Kept = "kept"

  /** The variant a pulled log of shadow `name`'s answers names: `shadow-<name>`. */
  def variant(name: ShadowName): String = s"shadow-${ShadowName.value(name)}"

  /** What [[apply]] read: live triage's log, its rows the corpus's cases triage answered and
    * whose question the corpus rebuilt, each answered row's answers under their names, the
    * question set's names those of its first answered row; `renamed`, the answered rows left
    * out because their names differ from those (a pull since before live triage changed its
    * question set: pull `since` the change); `unbuilt`, the cases left out because the corpus
    * could not rebuild their question; each shadow's log by the form its answers were kept in;
    * and the Slack messages tagged since that no corpus case is, for the next capture.
    */
  final case class Pulled(
      live: Log[VectorMap[QuestionName, Answer]],
      renamed: Int,
      shadows: Vector[Pulled.Shadow],
      uncaptured: Vector[CaseId],
      unbuilt: Int
  )

  object Pulled {

    /** One shadow's log; and of the messages tagged since with no row of its, how many its
      * shadow `ended` keeping nothing (DBOS has the shadow, finished), and how many are
      * `waiting` (not yet enqueued, or queued or running).
      */
    final case class Shadow(
        name: ShadowName,
        log: ShadowLog,
        ended: Int,
        waiting: Int
    )
  }

  /** The logs of what `reader`'s database kept of the heard messages tagged at or after
    * `since` and before `at`, over `cases` (the corpus `corpus`, its files' digest `corpusDigest`): live
    * triage's, and one for each of `names`. Each log's header has the variant
    * ([[Kept]], or [[variant]]), the model the first of its answered rows requested, no wording
    * ([[Header.wording]]), a question set's names for live's and a [[ShadowLog.Named]], the build every engine start since `since` ran (`Unknown` unless
    * they all ran one), one repeat, no cache, no rule, a cap of 0 (a shadow's cap is its
    * deployment's) and `at` as when it started; its footer what its rows' calls cost. A kept
    * row's request is the corpus's rebuilt digest, as live triage's own is not recorded, and
    * its latency 0. `Left` when the database cannot be read, naming what was being read.
    */
  def apply(
      reader: Reader^,
      corpus: String,
      corpusDigest: Digest,
      cases: Vector[Case],
      names: Vector[ShadowName],
      since: Instant,
      at: Instant
  ): Either[String, Pulled] = {
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]): Either[String, A] =
      reader.db.read(body).left.map(e => s"$what unread: ${e.getClass.getSimpleName}")
    val byId = cases.map(c => c.id -> c).toMap
    for {
      tagged <- read("tags")(reader.triage.tagged(since, at))
      ids <- each(tagged)(t =>
        read("conversation")(reader.conversations.get(t.triage.period.conversationId))
          .map(c =>
            t -> c.flatMap(c => CaseId.of(c.origin, t.entry).map(id => id -> focus(c.origin, id)))
          )
      )
      starts <- reader.starts().left.map(e => s"engine starts unread: ${e.getClass.getSimpleName}")
      inCorpus = cases.flatMap(c =>
        ids.collectFirst { case (t, Some((id, focus))) if id == c.id => (t, c, focus) }
      )
      built = inCorpus.flatMap((t, c, focus) => c.asked.map(a => (t, c, a.input.request, focus)))
      shadowed <- each(names)(n =>
        read("shadows")(reader.shadows.of(n, tagged.map(_.entry))).map(n -> _)
      )
    } yield {
      val build = starts.filterNot(_.at.isBefore(since)).map(_.build).distinct match {
        case Vector(one) => one
        case _ => Build.Unknown
      }
      def header(variant: String, model: String, questions: Option[Vector[QuestionName]]) =
        Header(
          corpus,
          corpusDigest,
          None,
          variant,
          None,
          model,
          None,
          None,
          questions,
          build,
          1,
          false,
          0,
          None,
          at
        )
      def log[A](
          variant: String,
          priced: Vector[Priced[A]],
          questions: Option[Vector[QuestionName]] = None
      ): Log[A] = {
        val rows: Vector[Row[A]] = priced.map((p: Priced[A]) => p.row)
        val model = priced.find(_.row.reported.isDefined).map(_.row.requested)
        Log(
          header(variant, model.getOrElse("unknown"), questions),
          rows,
          Some(Footer.of(priced.map((p: Priced[A]) => p.cost).sum, rows))
        )
      }
      def shadowLog(name: ShadowName, kept: Vector[Held]): ShadowLog = {
        val worded = kept.count {
          case Held(_, _, _, _, Form.Worded(_), _, _, _) => true
          case _ => false
        }
        val named = kept.collect { case Held(_, _, _, _, Form.Named(answers), _, _, _) => answers }
        (worded, named.headOption) match {
          case (0, Some(first)) =>
            ShadowLog.Named(
              log(variant(name), kept.flatMap(namedRow), Some(first.keys.toVector))
            )
          case (_, None) => ShadowLog.Worded(log(variant(name), kept.map(wordedRow)))
          case (_, Some(_)) => ShadowLog.Mixed(worded, named.size)
        }
      }
      val all = built.map((t, c, request, focus) => kept(c.id, request, t.tags, focus))
      val first = all.collectFirst { case (Some(names), _) => names }
      val live = all.collect {
        case (names, row) if names.isEmpty || names == first => row
      }
      Pulled(
        log(Kept, live, first),
        all.size - live.size,
        shadowed.map { (name, rows) =>
          val kept =
            inCorpus.flatMap((t, c, focus) => rows.get(t.entry).map(held(c.id, _, focus)))
          val missing = tagged.filterNot(t => rows.contains(t.entry))
          val ended = missing.count(t =>
            reader
              .workflow(ShadowRef(t.triage, name).workflowId)
              .exists(r => !Active.contains(r.status))
          )
          Pulled.Shadow(
            name,
            shadowLog(name, kept),
            ended,
            missing.size - ended
          )
        },
        ids.collect { case (_, Some((id, _))) if !byId.contains(id) => id }.distinct.sorted,
        inCorpus.size - built.size
      )
    }
  }

  /** What [[verdicts]] read: the verdicts standing, by case; and how many stand on a message
    * no case id names, one not heard in Slack.
    */
  final case class Standing(verdicts: Verdicts, unnamed: Int)

  /** The verdicts standing on the messages a review considered at or after `since`, each by
    * its case, rebuilt from its message's conversation ([[CaseId.of]]); one a case, the latest
    * given when two messages share one. `Left` when the database cannot be read, naming what
    * was being read.
    */
  def verdicts(reader: Reader^, since: Instant): Either[String, Standing] = {
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]): Either[String, A] =
      reader.db.read(body).left.map(e => s"$what unread: ${e.getClass.getSimpleName}")
    for {
      reviewed <- read("reviews")(reader.reviews.reviewed(since))
      rated = reviewed.flatMap(r =>
        (r.as, r.label) match {
          case (Considered.Picked(reason), Some(l)) =>
            Some(r -> Rated(r.shadow, reason, l.verdict, l.rater, l.at))
          case _ => None
        }
      )
      named <- each(rated)((r, rated) =>
        read("conversation")(reader.conversations.get(r.conversation))
          .map(c => (c.flatMap(c => CaseId.of(c.origin, r.entry)), rated))
      )
    } yield {
      val byCase = named.collect { case (Some(id), rated) => id -> rated }
      Standing(
        Verdicts(byCase.sortBy(_._2.at).toMap),
        named.count(_._1.isEmpty)
      )
    }
  }

  /** A pulled row, and what its call cost in USD (0 when unreported). */
  private final case class Priced[A](row: Row[A], cost: BigDecimal)

  /** What a shadow kept of case `id`, said at `focus`: its request's digest, the model
    * requested and the one that answered, its answers in their form, what it consumed, and how
    * long it took.
    */
  private final case class Held(
      id: CaseId,
      request: Digest,
      requested: String,
      reported: Option[String],
      form: Form,
      usage: Usage,
      latency: FiniteDuration,
      focus: Focus
  )

  /** A kept row's answers: a wording's, a question set's, or none, failed. */
  private enum Form {
    case Worded(answers: Vector[Answer])
    case Named(answers: VectorMap[QuestionName, Answer])
    case Failed(failure: Failure)
  }

  /** The statuses DBOS gives a workflow not yet finished. */
  private val Active = Set("PENDING", "ENQUEUED", "DELAYED")

  /** Triage's questions in the order a request asks them, whose keys every wording shares:
    * how an answer is put by position.
    */
  private val Questions: Vector[Question] =
    TriageQuestions.V1.request(TriageQuestion.State("", "", ""), KnowledgeSources.Empty).questions

  /** A kept row of case `id`, said at `focus` and asked as `request`, from live triage's
    * `tags`, and its cost; with the names it answered under, `None` when it failed.
    */
  private def kept(
      id: CaseId,
      request: Digest,
      tags: Tags,
      focus: Focus
  ): (Option[Vector[QuestionName]], Priced[VectorMap[QuestionName, Answer]]) =
    tags match {
      case Tags.Weighed(answers, model, usage) =>
        (
          Some(answers.keys.toVector),
          row(
            id,
            request,
            model,
            Some(model),
            Outcome.Answered(answers),
            usage,
            Duration.Zero,
            focus
          )
        )
      case Tags.Unanswered(why) =>
        (
          None,
          row(
            id,
            request,
            "unknown",
            None,
            Outcome.Failed(Failure.of(why)),
            Usage.Zero,
            Duration.Zero,
            focus
          )
        )
    }

  /** What a shadow kept of case `id`, said at `focus`. */
  private def held(id: CaseId, kept: Shadowed, focus: Focus): Held =
    kept match {
      case Shadowed.Answered(request, answers, usage, requested, answered, latency) =>
        val form = answers match {
          case ShadowAnswers.Worded(as) => Form.Worded(as)
          case ShadowAnswers.Named(as) => Form.Named(as)
        }
        Held(id, digest(request), requested, Some(answered), form, usage, latency, focus)
      case Shadowed.Failed(request, failure, latency) =>
        val kind = failure match {
          case ClassifierError.Kind.Unavailable => Failure.Unavailable
          case ClassifierError.Kind.Unreadable => Failure.Unreadable
        }
        Held(id, digest(request), "unknown", None, Form.Failed(kind), Usage.Zero, latency, focus)
    }

  /** `k` as a row of a log of triage's question, and its cost: a question set's answers, which
    * such a log cannot hold, and a wording's not of triage's questions, as unreadable.
    */
  private def wordedRow(k: Held): Priced[Vector[Weights]] = {
    val outcome: Outcome[Vector[Weights]] = k.form match {
      case Form.Worded(answers) =>
        val weights = answers.zip(Questions).flatMap((a, q) => Weights.of(q, a))
        if (weights.size == Questions.size && answers.size == Questions.size)
          Outcome.Answered(weights)
        else Outcome.Failed(Failure.Unreadable)
      case Form.Named(_) => Outcome.Failed(Failure.Unreadable)
      case Form.Failed(failure) => Outcome.Failed(failure)
    }
    row(k.id, k.request, k.requested, k.reported, outcome, k.usage, k.latency, k.focus)
  }

  /** `k` as a row of a question set's log, and its cost; `None` for a wording's answers. */
  private def namedRow(k: Held): Option[Priced[VectorMap[QuestionName, Answer]]] = {
    val outcome: Option[Outcome[VectorMap[QuestionName, Answer]]] = k.form match {
      case Form.Named(answers) => Some(Outcome.Answered(answers))
      case Form.Failed(failure) => Some(Outcome.Failed(failure))
      case Form.Worded(_) => None
    }
    outcome.map(o => row(k.id, k.request, k.requested, k.reported, o, k.usage, k.latency, k.focus))
  }

  private def row[A](
      id: CaseId,
      request: Digest,
      requested: String,
      reported: Option[String],
      outcome: Outcome[A],
      usage: Usage,
      latency: FiniteDuration,
      focus: Focus
  ): Priced[A] =
    Priced(
      Row(
        Suite.Triage,
        id,
        0,
        request,
        // Never a cache's: what the pulled call's key would have been is not recorded.
        CacheKey.of("pulled", request.hex, 0),
        requested,
        reported,
        outcome,
        usage,
        latency,
        false,
        Some(focus)
      ),
      usage.costUsd.getOrElse(BigDecimal(0))
    )

  /** The focus case `id` was said at, in a conversation of `origin`: its thread's opening
    * when `id` is the case that began it ([[CaseId.opening]]), else a reply.
    */
  private def focus(origin: Origin, id: CaseId): Focus =
    origin.focus(if (CaseId.opening(origin).contains(id)) Position.Opening else Position.Reply)

  /** A shadow row's request digest; the schema keeps 64 hex digits, so any other is the
    * digest of what it holds.
    */
  private def digest(request: String): Digest = Digest.read(request).getOrElse(Digest.text(request))

  private def each[A, B](xs: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    xs.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, x) =>
      acc.flatMap(done => f(x).map(done :+ _))
    )
}
