package grit.eval.harness.capture

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, ClassifierError}
import grit.core.id.{ConversationId, EntryId, QuestionName, WorkflowId}
import grit.core.period.Probability
import grit.core.stitch.{Offered, Tuning}
import grit.core.triage.{Kind, Tags}
import grit.dbos.engine.{Build, Reader}

/** One heard message of a capture, text-free: who it is (`id`), where it was in the database
  * captured (`entry`, `conversation`), when it was `tagged` and by which triage, what triage
  * and stitching made of it live, the inputs the shipped builders rebuild for it, and how it
  * clusters.
  *
  * @param asked
  *   triage's question rebuilt; `None` when the builder refuses it
  * @param author
  *   the digest of its author's enrolled name; `None` when the author was never enrolled
  * @param stitch
  *   its placement, when it was its thread's first message and was placed
  * @param tuning
  *   the tuning its placement was made under, when it is not the capture's
  */
final case class Case(
    id: CaseId,
    entry: EntryId,
    conversation: ConversationId,
    tagged: Instant,
    triage: Triaged,
    tags: Live,
    asked: Option[Asked],
    author: Option[Digest],
    clusters: Clusters,
    stitch: Option[Stitched],
    tuning: Option[Tuning]
)

/** The triage workflow that tagged a case, DBOS's record of it (`None` when DBOS no longer
  * knows it), and the build that ran it: the latest engine start at or before the workflow was
  * created, `Unknown` before the first recorded one.
  */
final case class Triaged(workflow: WorkflowId, recorded: Option[Reader.Recorded], build: Build)

object Triaged {

  /** The build of the latest of `starts` at or before `created`; `Unknown` when none is. */
  def buildAt(starts: Vector[Build.Started], created: Instant): Build =
    starts.filterNot(_.at.isAfter(created)).maxByOption(_.at).fold(Build.Unknown)(_.build)
}

/** What triage made of a heard message live. */
enum Live {

  /** v1's answers: the chosen kind and its weight, the probabilities of yes to waiting,
    * durable and helps, the model that weighed them and the call's cost.
    */
  case Weighed(
      kind: Kind,
      kindP: Probability,
      waiting: Probability,
      durable: Probability,
      helps: Probability,
      model: String,
      costUsd: Option[BigDecimal]
  )

  /** Another question set's answers, each under its question's name in the order asked (live
    * triage's since v2), the model that weighed them and the call's cost.
    */
  case Named(answers: VectorMap[QuestionName, Answer], model: String, costUsd: Option[BigDecimal])

  /** No answer, of this kind. */
  case Unanswered(failure: Failure)
}

object Live {

  /** What a case keeps of live triage's `tags`, less the usage but its cost: answers to v1's
    * names alone ([[Tags.V1]]), `kind` a choice of a [[Kind]] and the rest yes/nos, as
    * `Weighed`; any other answers as `Named`; no answer as `Unanswered`, of its kind.
    */
  def of(tags: Tags): Live = tags match {
    case Tags.Weighed(answers, model, usage) =>
      v1(answers, model, usage.costUsd).getOrElse(Live.Named(answers, model, usage.costUsd))
    case Tags.Unanswered(why) => Live.Unanswered(Failure.of(why))
  }

  private def v1(
      answers: VectorMap[QuestionName, Answer],
      model: String,
      costUsd: Option[BigDecimal]
  ): Option[Live.Weighed] = {
    def yes(name: QuestionName) =
      answers.get(name).collect { case Answer.YesNo(p) => Probability.clamped(p) }
    val names = Set(Tags.V1.kind, Tags.V1.waiting, Tags.V1.durable, Tags.V1.helps)
    for {
      _ <- Option.when(answers.keySet == names)(())
      (kind, kindP) <- answers.get(Tags.V1.kind).collect { case Answer.Choice(c, ws, _) =>
        (c, ws.find(_.key == c).fold(0.0)(_.probability))
      }
      k <- Kind.read(kind)
      waiting <- yes(Tags.V1.waiting)
      durable <- yes(Tags.V1.durable)
      helps <- yes(Tags.V1.helps)
    } yield Live.Weighed(k, Probability.clamped(kindP), waiting, durable, helps, model, costUsd)
  }
}

/** Why a classifier gave no answer, as a kind: never the words of why. */
enum Failure {

  /** It could not be reached, or refused the request. */
  case Unavailable

  /** It replied with no answer the question allows. */
  case Unreadable

  /** The question could not be asked: it repeats a key, or offers fewer than two options. */
  case Unasked

  /** Any other reason. */
  case Other
}

object Failure {

  /** `f`'s written name, as a capture and a run log keep it: `unavailable`, `unreadable`,
    * `unasked` or `other`.
    */
  def written(f: Failure): String = f.toString.toLowerCase

  /** The failure written `name`; `None` for no failure's. */
  def read(name: String): Option[Failure] = values.find(written(_) == name)

  /** The kind of `e`, never its words: they can quote the request. */
  def of(e: ClassifierError): Failure = e match {
    case ClassifierError.Unavailable(_) => Unavailable
    case ClassifierError.Unreadable(_) => Unreadable
  }

  /** The kind of failure `why` states, in the words triage and stitching keep: `unavailable:
    * …`, `unreadable: …`, `the question repeats …` or `fewer than two options`.
    */
  def of(why: String): Failure =
    if (why.startsWith("unavailable:")) Unavailable
    else if (why.startsWith("unreadable:")) Unreadable
    else if (why.startsWith("the question repeats") || why == "fewer than two options") Unasked
    else Other
}

/** An input as a shipped builder makes it: the digest of the state it shows the classifier,
  * and of the whole request ([[grit.core.classify.Request.digest]]).
  */
final case class Built(state: Digest, request: Digest)

/** Triage's question as rebuilt: its `input`, and the lengths in characters of the `message`
  * and of the `thread` it is shown.
  */
final case class Asked(input: Built, message: Int, thread: Int)

/** What a case clusters by: its `conversation` (the case that began its thread) and its
  * `exchange` (the case that began the exchange its thread follows, or its thread's own when
  * it follows none).
  */
final case class Clusters(conversation: CaseId, exchange: CaseId)

/** A first message's placement among its room's exchanges, live and rebuilt: what became of
  * it, the exchanges it was offered as it was shown live (most recently spoken in first), its
  * input as rebuilt now (`None` when nothing is offered on rebuild) and the slots it offers
  * then (most recently spoken in first; none when nothing is), the [[SeenCheck]] of
  * the two, and `drift`, how many exchanges offered lexically both live and on rebuild have
  * scores further apart than [[Stitched.Tolerance]].
  */
final case class Stitched(
    placed: Placement,
    offered: Vector[Offering],
    input: Option[Built],
    rebuilt: Vector[Slot],
    seen: SeenCheck,
    drift: Int
)

object Stitched {

  /** The most two BM25 scores of one exchange may differ by and not count as drift. */
  val Tolerance = 1e-6

  /** How many exchanges, by root, `live` and `rebuilt` both offer lexically with scores
    * further apart than [[Tolerance]].
    */
  def drift(live: Vector[Slot], rebuilt: Vector[Slot]): Int =
    live.count {
      case Slot(root, Offered.Lexical(was)) =>
        rebuilt.exists {
          case Slot(r, Offered.Lexical(now)) => r == root && math.abs(now - was) > Tolerance
          case _ => false
        }
      case _ => false
    }
}

/** What became of a first message put to the classifier live. */
enum Placement {

  /** It continues the exchange that `root` began, at `p`. */
  case Follows(root: CaseId, p: Probability)

  /** It begins something new; `p` is what the likeliest exchange was given. */
  case Begins(p: Probability)

  /** The classifier failed, of this kind. */
  case Unread(failure: Failure)
}

/** One exchange offered live: the case that began it, why it was offered, and the probability
  * it was given (`None` when the answer did not read).
  */
final case class Offering(root: CaseId, why: Offered, p: Option[Probability])

/** One exchange offered: the case that began it, and why it was offered. */
final case class Slot(root: CaseId, why: Offered)

/** Whether the state the shipped builder shows the classifier now is the one it was shown
  * live (`grit.core.stitch.Seen.state`), and when not, which kind of difference it is. A
  * recent slot is one offered as [[Offered.Recent]] with a rank no greater than the tuning's
  * `recent`; a slot ranked after them filled a lexical slot nothing matched, so it is
  * lexical. Only a lexical slot may differ without the rebuild reading rows from after the
  * message (BM25's statistics span every row).
  */
sealed trait SeenCheck

object SeenCheck {

  /** The same, field for field. */
  case object Match extends SeenCheck

  /** Nothing is offered on rebuild, or the room cannot be read. */
  case object Unbuilt extends SeenCheck

  /** The message or its author differs: these `fields`, never none. */
  final case class MessageDiffers private[capture] (fields: Set[Field]) extends SeenCheck

  /** The recent slots differ: the ranks, from 1, whose roots differ, never none. */
  final case class RecentDiffers private[capture] (ranks: Vector[Int]) extends SeenCheck

  /** An exchange both sides offer, by root, is shown differently: those `roots` and the
    * `fields` (`opening`, `latest`, `record`) that differ, neither ever none.
    */
  final case class SameRootDiffers private[capture] (roots: Vector[CaseId], fields: Set[Field])
      extends SeenCheck

  /** The recent slots are the same, in the same order, and only lexical slots differ, in
    * these `fields` of the state, never none.
    */
  final case class LexicalOnly private[capture] (fields: Set[Field]) extends SeenCheck

  /** How `rebuilt`, the stitch question's state offering `rebuiltSlots`, differs from `live`,
    * offering `liveSlots`, under a tuning of `recent` recent slots; each state's exchanges in
    * its slots' order. When more than one kind holds, the first of `MessageDiffers`,
    * `RecentDiffers`, `SameRootDiffers` and `LexicalOnly`. A field missing from either state
    * differs.
    */
  def compare(
      live: ujson.Value,
      liveSlots: Vector[Slot],
      rebuilt: ujson.Value,
      rebuiltSlots: Vector[Slot],
      recent: Int
  ): SeenCheck = {
    def at(v: ujson.Value, k: String): Option[ujson.Value] = v.objOpt.flatMap(_.get(k))
    def same(x: Option[ujson.Value], y: Option[ujson.Value]): Boolean = x.isDefined && x == y
    def exchanges(v: ujson.Value): Vector[ujson.Value] =
      at(v, "exchanges").flatMap(_.arrOpt).fold(Vector.empty[ujson.Value])(_.toVector)
    val (was, now) = (exchanges(live), exchanges(rebuilt))
    val shown =
      Vector(Field.Opening -> "opening", Field.Latest -> "latest", Field.Record -> "record")
    def differ(x: ujson.Value, y: ujson.Value): Set[Field] =
      shown.collect { case (f, k) if !same(at(x, k), at(y, k)) => f }.toSet
    val message = Set(
      Option.when(!same(at(live, "new_message"), at(rebuilt, "new_message")))(Field.NewMessage),
      Option.when(!same(at(live, "author"), at(rebuilt, "author")))(Field.Author)
    ).flatten
    def recents(s: Vector[Slot]): Map[Int, CaseId] =
      s.collect { case Slot(root, Offered.Recent(rank)) if rank <= recent => rank -> root }.toMap
    val (r1, r2) = (recents(liveSlots), recents(rebuiltSlots))
    val ranks = (r1.keySet ++ r2.keySet).toVector.sorted.filter(k => r1.get(k) != r2.get(k))
    val byRoot = rebuiltSlots.map(_.root).zip(now).toMap
    val sameRoot = liveSlots.map(_.root).zip(was).flatMap { (root, x) =>
      byRoot.get(root).map(y => root -> differ(x, y)).filter(_._2.nonEmpty)
    }
    val positional = Set(
      Option.when(was.isEmpty || was.size != now.size)(Field.Exchanges),
      Option.when(liveSlots.map(_.root) != rebuiltSlots.map(_.root))(Field.Roots)
    ).flatten ++ was.zip(now).flatMap(differ(_, _))
    if (message.nonEmpty) new MessageDiffers(message)
    else if (ranks.nonEmpty) new RecentDiffers(ranks)
    else if (sameRoot.nonEmpty)
      new SameRootDiffers(sameRoot.map(_._1), sameRoot.flatMap(_._2).toSet)
    else if (positional.nonEmpty) new LexicalOnly(positional)
    else Match
  }

  /** A field of the stitch question's state. */
  enum Field {

    /** How many exchanges are offered. */
    case Exchanges

    /** Which exchanges, by root, in which order. */
    case Roots

    /** An exchange's opening message. */
    case Opening

    /** An exchange's latest messages. */
    case Latest

    /** An exchange's record line. */
    case Record

    /** The message itself. */
    case NewMessage

    /** Who said it. */
    case Author
  }

  object Field {

    /** `field`'s written name, as the stitch question's state names it: `exchanges`, `roots`,
      * `opening`, `latest`, `record`, `new_message`, `author`.
      */
    def written(field: Field): String = field match {
      case NewMessage => "new_message"
      case f => f.toString.toLowerCase
    }

    /** The field written `name`; `None` for no field's. */
    def read(name: String): Option[Field] = values.find(written(_) == name)
  }
}
