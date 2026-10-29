package grit.core.session

import java.time.Instant

/** A session's log: its events in the order recorded, each at its own seq from 1, up to
  * [[HardCap]] lines, the last of them an overflow when more were offered.
  */
object Log {

  /** 5,000: the most lines a session's log holds, its overflow included. */
  val HardCap: Int = 5000

  /** A line of a session's log as stored. */
  enum Line {

    /** `event`, logged at `seq`. */
    case Logged(seq: Int, event: Event)

    /** The one line standing for the event `n` at `at`, and every event after it. */
    case Overflow(seq: Int, n: Long, at: Instant)

    def seq: Int

    /** Its stored kind: its event's [[Event.Kind.name]], or `overflow`. */
    def kind: String = this match {
      case Logged(_, e) => e.kind.name
      case Overflow(_, _, _) => "overflow"
    }
  }

  /** The lines `fresh` (events no line of the log holds) add to a log whose next seq is `next`,
    * in `fresh`'s order: each logged at the next seq below [[HardCap]], and then, when any is
    * left, one overflow at [[HardCap]] for the first left. None once `next` is past [[HardCap]]:
    * the log is full.
    */
  def admit(next: Int, fresh: Vector[Event]): Vector[Line] = {
    val room = math.max(0, HardCap - next)
    val logged = fresh.take(room).zipWithIndex.map((e, i) => Line.Logged(next + i, e))
    val over =
      if (next > HardCap) None
      else fresh.drop(room).headOption.map(e => Line.Overflow(HardCap, e.n, e.at))
    logged ++ over.toVector
  }
}
