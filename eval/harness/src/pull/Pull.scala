package grit.eval.harness.pull

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError, Question}
import grit.core.id.{ShadowName, ShadowRef}
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Kind, Shadowed, Tags, TriageStore}
import grit.dbos.engine.{Build, Reader}
import grit.eval.harness.corpus.{Case, CaseId, Digest, Failure}
import grit.eval.harness.log.{CacheKey, Footer, Header, Log, Outcome, Row, Suite, Weights}
import grit.lifecycle.triage.TriageQuestion

/** What a deployment's database kept of the heard messages tagged since a time, as run logs
  * the scorers read beside offline runs: live triage's tags ([[Pull.Kept]]), and each named
  * shadow's answers. Rows are of the corpus's cases alone, in its order, one a case, never
  * cached; text-free.
  */
object Pull {

  /** The variant a pulled log of live triage's tags names: `kept`. Live triage keeps the
    * likeliest kind's probability, not every kind's: its row gives that kind its kept
    * probability and every other kind an even share of the rest, so a kind's comparison
    * against it reads the likeliest kind and its probability only.
    */
  val Kept = "kept"

  /** The variant a pulled log of shadow `name`'s answers names: `shadow-<name>`. */
  def variant(name: ShadowName): String = s"shadow-${ShadowName.value(name)}"

  /** What [[apply]] read: live triage's log (its rows the corpus's cases triage answered and
    * whose question the corpus rebuilt, `unbuilt` counting those it could not), each shadow's,
    * and the Slack messages tagged since that no corpus case is, for the next capture.
    */
  final case class Pulled(
      live: Log[Vector[Weights]],
      shadows: Vector[Pulled.Shadow],
      uncaptured: Vector[CaseId],
      unbuilt: Int
  )

  object Pulled {

    /** One shadow's log, and of the messages tagged since with no row of its: how many its
      * shadow `ended` keeping nothing (DBOS has the shadow, finished), and how many are
      * `waiting` (not yet enqueued, or queued or running).
      */
    final case class Shadow(name: ShadowName, log: Log[Vector[Weights]], ended: Int, waiting: Int)
  }

  /** The logs of what `reader`'s database kept of the heard messages tagged at or after
    * `since` and before `at`, over `cases` (the corpus `corpus`, its files' digest `corpusDigest`): live
    * triage's, and one for each of `names`. Each log's header has the variant
    * ([[Kept]], or [[variant]]), the model the first of its answered rows requested, no wording
    * ([[Header.wording]]), the build every engine start since `since` ran (`Unknown` unless
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
          .map(c => t -> c.flatMap(c => CaseId.of(c.origin, t.entry)))
      )
      starts <- reader.starts().left.map(e => s"engine starts unread: ${e.getClass.getSimpleName}")
      inCorpus = cases.flatMap(c => ids.collectFirst { case (t, Some(id)) if id == c.id => (t, c) })
      built = inCorpus.flatMap((t, c) => c.asked.map(a => (t, c, a.input.request)))
      shadowed <- each(names)(n =>
        read("shadows")(reader.shadows.of(n, tagged.map(_.entry))).map(n -> _)
      )
    } yield {
      val build = starts.filterNot(_.at.isBefore(since)).map(_.build).distinct match {
        case Vector(one) => one
        case _ => Build.Unknown
      }
      def header(variant: String, model: String) =
        Header(corpus, corpusDigest, None, variant, None, model, None, build, 1, false, 0, None, at)
      def log(variant: String, priced: Vector[Priced]): Log[Vector[Weights]] = {
        val rows: Vector[Row[Vector[Weights]]] = priced.map((p: Priced) => p.row)
        val model = priced.collectFirst {
          case p: Priced if p.row.reported.isDefined => p.row.requested
        }
        Log(
          header(variant, model.getOrElse("unknown")),
          rows,
          Some(Footer.of(priced.map((p: Priced) => p.cost).sum, rows))
        )
      }
      val live = built.map((t, c, request) => kept(c.id, request, t.tags))
      Pulled(
        log(Kept, live),
        shadowed.map { (name, rows) =>
          val mine = inCorpus.flatMap((t, c) => rows.get(t.entry).map(shadowRow(c.id, _)))
          val missing = tagged.filterNot(t => rows.contains(t.entry))
          val ended = missing.count(t =>
            reader
              .workflow(ShadowRef(t.triage, name).workflowId)
              .exists(r => !Active.contains(r.status))
          )
          Pulled.Shadow(name, log(variant(name), mine), ended, missing.size - ended)
        },
        ids.collect { case (_, Some(id)) if !byId.contains(id) => id }.distinct.sorted,
        inCorpus.size - live.size
      )
    }
  }

  /** A pulled row, and what its call cost in USD (0 when unreported). */
  private final case class Priced(row: Row[Vector[Weights]], cost: BigDecimal)

  /** The statuses DBOS gives a workflow not yet finished. */
  private val Active = Set("PENDING", "ENQUEUED", "DELAYED")

  /** Triage's questions in the order a request asks them, whose keys every wording shares:
    * how an answer is put by position.
    */
  private val Questions: Vector[Question] =
    TriageQuestion
      .request(TriageQuestion.Wording.Shipped, TriageQuestion.State("", "", ""))
      .fold(Vector.empty[Question])(_.questions)

  /** A kept row of case `id`, asked as `request`, from live triage's `tags`, and its cost. */
  private def kept(id: CaseId, request: Digest, tags: Tags): Priced =
    tags match {
      case Tags.Weighed(kind, kindP, waiting, durable, helps, model, usage) =>
        val p = Probability.value(kindP)
        val rest = (1 - p) / (Kind.values.size - 1)
        val ps = Kind.values.toVector.map(k => if (k == kind) p else rest)
        val weights = Vector(
          Weights.Choice(kind.ordinal, ps, Answer.confidence(ps)),
          Weights.YesNo(Probability.value(waiting)),
          Weights.YesNo(Probability.value(durable)),
          Weights.YesNo(Probability.value(helps))
        )
        row(id, request, model, Some(model), Outcome.Answered(weights), usage, Duration.Zero)
      case Tags.Unanswered(why) =>
        row(
          id,
          request,
          "unknown",
          None,
          Outcome.Failed(Failure.of(why)),
          Usage.Zero,
          Duration.Zero
        )
    }

  /** A shadow's row of case `id`, from what it kept, and its cost. */
  private def shadowRow(id: CaseId, kept: Shadowed): Priced =
    kept match {
      case Shadowed.Answered(request, answers, usage, requested, answered, latency) =>
        val weights = answers.zip(Questions).flatMap((a, q) => Weights.of(q, a))
        val outcome =
          if (weights.size == Questions.size && answers.size == Questions.size)
            Outcome.Answered(weights)
          else Outcome.Failed(Failure.Unreadable)
        row(id, digest(request), requested, Some(answered), outcome, usage, latency)
      case Shadowed.Failed(request, failure, latency) =>
        val kind = failure match {
          case ClassifierError.Kind.Unavailable => Failure.Unavailable
          case ClassifierError.Kind.Unreadable => Failure.Unreadable
        }
        row(id, digest(request), "unknown", None, Outcome.Failed(kind), Usage.Zero, latency)
    }

  private def row(
      id: CaseId,
      request: Digest,
      requested: String,
      reported: Option[String],
      outcome: Outcome[Vector[Weights]],
      usage: Usage,
      latency: FiniteDuration
  ): Priced =
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
        false
      ),
      usage.costUsd.getOrElse(BigDecimal(0))
    )

  /** A shadow row's request digest; the schema keeps 64 hex digits, so any other is the
    * digest of what it holds.
    */
  private def digest(request: String): Digest = Digest.read(request).getOrElse(Digest.text(request))

  private def each[A, B](xs: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    xs.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, x) =>
      acc.flatMap(done => f(x).map(done :+ _))
    )
}
