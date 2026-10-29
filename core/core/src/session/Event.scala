package grit.core.session

import java.nio.charset.StandardCharsets
import java.time.Instant

/** One thing a working session did, in grit's words: `n` its source's own ordinal for it, unique
  * within the session (a hook's nanoseconds, a turn's entry seq), so an event recorded again is
  * known; `shape` what it was, never its content.
  */
final case class Event private (n: Long, at: Instant, kind: Event.Kind, shape: Event.Shape)

object Event {

  /** The event, or why not: `n` not above 0, or a read of fewer than 0 bytes. */
  def of(n: Long, at: Instant, kind: Kind, shape: Shape): Either[String, Event] =
    if (n <= 0) Left(s"an event's ordinal is above 0: $n")
    else
      kind match {
        case Kind.Read(bytes) if bytes < 0 => Left(s"a read is of 0 bytes or more: $bytes")
        case _ => Right(new Event(n, at, kind, shape))
      }

  /** What an event was. A read's bytes go to its session's counters, and a closing's summary
    * to its session's row, not to its log.
    */
  enum Kind {
    case Started
    case Read(bytes: Long)
    case Edited
    case Ran
    case Commit
    case Closing(summary: Session.Summary)
    case Ended

    /** Its stored name. */
    def name: String = this match {
      case Started => "started"
      case Read(_) => "read"
      case Edited => "edited"
      case Ran => "ran"
      case Commit => "commit"
      case Closing(_) => "closing"
      case Ended => "ended"
    }
  }

  /** What an event was, never its content (a path, an exit code, not a file's text): a JSON
    * object of at most [[Shape.Longest]] UTF-8 bytes as written.
    */
  opaque type Shape = String

  object Shape {

    /** 300: the most UTF-8 bytes a shape takes, written compactly. */
    val Longest: Int = 300

    val Empty: Shape = "{}"

    /** `o` as a shape, or why not: over [[Longest]] bytes, written compactly. */
    def of(o: ujson.Obj): Either[String, Shape] = {
      val text = ujson.write(o)
      val bytes = text.getBytes(StandardCharsets.UTF_8).length
      Either.cond(bytes <= Longest, text, s"an event's shape is $bytes bytes, over $Longest")
    }

    def value(s: Shape): String = s
  }
}
