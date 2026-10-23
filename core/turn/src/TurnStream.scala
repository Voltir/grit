package grit.turn

import grit.act.phase.{Heard as Hearer, Hearing}
import grit.core.clock.{Clock, Fresh}
import grit.core.durable.StreamWriter
import grit.core.provider.Delta

/** What a turn tells edges while its model answers: the stream [[Key]] of its workflow,
  * chunks of reasoning and text, and the name of each tool call as it begins, each tagged
  * with the attempt at the `ask:n` step that wrote it. The stream is at least once (ADR
  * 0006): a step cut short by a crash leaves its chunks, and the rerun writes its own after
  * them, so a reader keeps only the latest attempt's ([[Heard]]). The recorded reply, not
  * the stream, is the turn's answer.
  */
object TurnStream {

  /** The turn workflow's stream of its reply. */
  val Key = "reply"

  /** A chunk of the reply, written by the `ask:n` run `attempt`. */
  final case class Chunk(attempt: String, delta: Delta)

  def encode(chunk: Chunk): String = {
    val (kind, text) = chunk.delta match {
      case Delta.Text(t) => ("text", t)
      case Delta.Reasoning(t) => ("reasoning", t)
      case Delta.Calling(name) => ("calling", name)
    }
    ujson.write(ujson.Obj("attempt" -> chunk.attempt, "kind" -> kind, "text" -> text))
  }

  /** The chunk `raw` encodes, or why it encodes none. */
  def decode(raw: String): Either[String, Chunk] =
    scala.util.Try(ujson.read(raw)).toOption.flatMap(_.objOpt) match {
      case None => Left("a chunk is not a JSON object")
      case Some(o) =>
        val field = (k: String) => o.get(k).flatMap(_.strOpt)
        (field("attempt"), field("kind"), field("text")) match {
          case (Some(a), Some("text"), Some(t)) => Right(Chunk(a, Delta.Text(t)))
          case (Some(a), Some("reasoning"), Some(t)) => Right(Chunk(a, Delta.Reasoning(t)))
          case (Some(a), Some("calling"), Some(n)) => Right(Chunk(a, Delta.Calling(n)))
          case _ => Left(s"not a chunk: ${raw.take(80)}")
        }
    }

  /** Deltas written to `out` as chunks of `attempt`, gathered so a reply is tens of rows
    * rather than one per token: a chunk of text or reasoning is written when the kind
    * changes, when it reaches [[MaxChars]], or when [[MaxMs]] have passed since the last was
    * written, as `now` (milliseconds) tells; a call's name is written at once, after what
    * was gathered before it. [[flush]] writes what is left. For one run of one step.
    */
  final class Writer(out: StreamWriter, attempt: String, now: () => Long) {

    // Private, each an immutable value replaced whole. A writer serves one attempt, whose
    // deltas a provider tells in order on the thread running the step (Provider.stream calls
    // onDelta before it returns), then flushes: no two of its calls overlap.
    @caps.unsafe.untrackedCaptures
    private var pending: Option[Delta] = None

    @caps.unsafe.untrackedCaptures
    private var since = now()

    def tell(delta: Delta): Unit = {
      pending = (pending, delta) match {
        case (Some(Delta.Text(a)), Delta.Text(b)) => Some(Delta.Text(a + b))
        case (Some(Delta.Reasoning(a)), Delta.Reasoning(b)) => Some(Delta.Reasoning(a + b))
        case (Some(other), _) => write(other); Some(delta)
        case (None, _) => Some(delta)
      }
      pending.foreach { p =>
        if (length(p) >= MaxChars || now() - since >= MaxMs) { write(p); pending = None }
      }
    }

    def flush(): Unit = {
      pending.foreach(write)
      pending = None
    }

    private def write(delta: Delta): Unit = {
      out.write(encode(Chunk(attempt, delta)))
      since = now()
    }

    /** How far `delta` is from being written: a call is never kept waiting. */
    private def length(delta: Delta): Int = delta match {
      case Delta.Text(t) => t.length
      case Delta.Reasoning(t) => t.length
      case Delta.Calling(_) => MaxChars
    }
  }

  /** A model call's attempts told to `out`: each a [[Writer]] of its own attempt, tagged by
    * `fresh` and paced by `clock`, flushed when the attempt is done.
    */
  def hearing(out: StreamWriter^, fresh: Fresh^, clock: Clock^): Hearing^ =
    new Hearing {
      def attempt(): Hearer^ = {
        val writer = new Writer(out, fresh.nonce(), () => clock.millis())
        new Hearer {
          def tell(delta: Delta): Unit = writer.tell(delta)
          def done(): Unit = writer.flush()
        }
      }
    }

  /** How long a chunk may wait to be written. */
  val MaxMs = 100L

  /** How long a chunk may grow before it is written. */
  val MaxChars = 200

  /** What an edge has heard of the reply so far: the latest attempt's reasoning and text,
    * and the names of the calls it has begun, in order.
    */
  final case class Heard(
      attempt: Option[String],
      reasoning: String,
      text: String,
      calling: Vector[String]
  ) {

    /** `chunk` heard: added to its attempt's, or, from a new attempt, starting over. */
    def +(chunk: Chunk): Heard = {
      val base =
        if (attempt.contains(chunk.attempt)) this else Heard(Some(chunk.attempt), "", "", Vector())
      chunk.delta match {
        case Delta.Text(t) => base.copy(text = base.text + t)
        case Delta.Reasoning(t) => base.copy(reasoning = base.reasoning + t)
        case Delta.Calling(name) => base.copy(calling = base.calling :+ name)
      }
    }
  }

  object Heard {
    val nothing: Heard = Heard(None, "", "", Vector())
  }
}
