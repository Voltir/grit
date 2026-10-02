package grit.eval.harness.corpus

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, WorkflowId}
import grit.core.period.Probability
import grit.core.stitch.{Offered, Tuning}
import grit.core.triage.Kind
import grit.dbos.engine.{Build, Reader}

/** One heard message of a corpus, text-free: who it is (`id`), where it was in the database
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
  *   the tuning its placement was made under, when it is not the corpus's
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

/** What triage made of a heard message live. */
enum Live {

  /** Its tags as `grit.triage` keeps them, less the usage but its cost. */
  case Weighed(
      kind: Kind,
      kindP: Probability,
      waiting: Probability,
      durable: Probability,
      helps: Probability,
      model: String,
      costUsd: Option[BigDecimal]
  )

  /** No answer, of this kind. */
  case Unanswered(failure: Failure)
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
  * input as rebuilt now (`None` when nothing is offered on rebuild), the [[SeenCheck]] of
  * the two, and `drift`, how many exchanges offered lexically both live and on rebuild have
  * scores further apart than [[Stitched.Tolerance]].
  */
final case class Stitched(
    placed: Placement,
    offered: Vector[Offering],
    input: Option[Built],
    seen: SeenCheck,
    drift: Int
)

object Stitched {

  /** The most two BM25 scores of one exchange may differ by and not count as drift. */
  val Tolerance = 1e-6
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

/** Whether the state the shipped builder shows the classifier now is the one it was shown
  * live (`grit.core.stitch.Seen.state`).
  */
sealed trait SeenCheck

object SeenCheck {

  /** The same, field for field. */
  case object Match extends SeenCheck

  /** These fields differ; never none. */
  final case class Differs private[SeenCheck] (fields: Set[Field]) extends SeenCheck

  /** Nothing is offered on rebuild, or the room cannot be read. */
  case object Unbuilt extends SeenCheck

  /** `Match` when no field differs, else `Differs`. */
  def of(fields: Set[Field]): SeenCheck = if (fields.isEmpty) Match else new Differs(fields)

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
