package grit.core.session

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.SessionId
import grit.core.identity.Principal
import grit.core.place.Place
import grit.core.visibility.Label

/** A working session's record, as a transaction reads it: who ran it, at what label, where it
  * worked (`at`, as its [[Session.Header]] said), when it started and last did something, when
  * it ended if it said so, its summary, and its counters. `full` when its log reached [[Log.HardCap]]: what it did since is not logged or counted.
  */
final case class Session(
    id: SessionId,
    parent: Option[SessionId],
    by: Principal.Person,
    label: Label,
    at: Place,
    started: Instant,
    ended: Option[Instant],
    lastEvent: Instant,
    summary: Option[Session.Summary],
    counters: Session.Counters,
    full: Boolean
) {

  /** When it counts as ended: `ended`, or [[Session.Unended]] after its last event. */
  def ends: Instant = ended.getOrElse(lastEvent.plusMillis(Session.Unended.toMillis))
}

object Session {

  /** How long after its last event a session that never said it ended counts as ended. */
  val Unended: FiniteDuration = 24.hours

  /** How long after it ends a session's log is kept. */
  val LogKept: FiniteDuration = 30.days

  /** How long after it ends a session's row is kept. */
  val Kept: FiniteDuration = 180.days

  /** When a session that had ended at `ended` has ended once `events` are logged on it, taken
    * in their order: at an `ended` event's time, until a `started` event after that time.
    */
  def ending(ended: Option[Instant], events: Vector[Event]): Option[Instant] =
    events.foldLeft(ended) { (now, e) =>
      e.kind match {
        case Event.Kind.Ended => Some(e.at)
        case Event.Kind.Started if now.exists(_.isBefore(e.at)) => None
        case _ => now
      }
    }

  /** What a session's row begins with, the first time an event is recorded on it. `label` is
    * kept joined with the recording transaction's floor, in that transaction's own room when it
    * has one. `at` is where the session worked, not a room: a harness session's worktree under
    * `fs`, a turn's conversation's place; it carries no label.
    */
  final case class Header(
      id: SessionId,
      parent: Option[SessionId],
      by: Principal.Person,
      label: Label,
      at: Place
  )

  /** What a session said it came to as it closed: 1 to [[Summary.Longest]] characters. */
  opaque type Summary = String

  object Summary {

    /** 2,000: the most characters a summary holds. */
    val Longest: Int = 2000

    /** `text` as a summary, or why not: empty, or over [[Longest]] characters. */
    def of(text: String): Either[String, Summary] =
      if (text.isEmpty || text.length > Longest)
        Left(s"a session's summary is 1 to $Longest characters")
      else Right(text)

    def value(s: Summary): String = s
  }

  /** What a session's logged events add up to: its tool calls (every `read`, `edited`, `ran`
    * and `commit` event), its edits, its commits and the bytes it read.
    */
  final case class Counters private (toolCalls: Long, edits: Long, commits: Long, bytesRead: Long) {
    def +(that: Counters): Counters =
      Counters(
        toolCalls + that.toolCalls,
        edits + that.edits,
        commits + that.commits,
        bytesRead + that.bytesRead
      )
  }

  object Counters {
    val zero: Counters = Counters(0, 0, 0, 0)

    /** What `events` add to a session's counters. */
    def of(events: Vector[Event]): Counters =
      events.foldLeft(zero) { (c, e) =>
        e.kind match {
          case Event.Kind.Read(bytes) => c + Counters(1, 0, 0, bytes)
          case Event.Kind.Edited => c + Counters(1, 1, 0, 0)
          case Event.Kind.Ran => c + Counters(1, 0, 0, 0)
          case Event.Kind.Commit => c + Counters(1, 0, 1, 0)
          case Event.Kind.Started | Event.Kind.Closing(_) | Event.Kind.Ended => c
        }
      }

    /** Counters as stored, or why not: any of them negative. */
    def read(
        toolCalls: Long,
        edits: Long,
        commits: Long,
        bytesRead: Long
    ): Either[String, Counters] =
      if (Vector(toolCalls, edits, commits, bytesRead).exists(_ < 0))
        Left("a session's counters are never negative")
      else Right(Counters(toolCalls, edits, commits, bytesRead))
  }
}
