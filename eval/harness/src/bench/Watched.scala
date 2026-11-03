package grit.eval.harness.bench

import scala.annotation.tailrec
import scala.concurrent.duration.*

import grit.core.host.{Ran, Started}

/** A started program read and watched to its end: the one loop a run's phases share, the coder,
  * the compile before it and each check alike, each passing its own `stop` and `watch`.
  */
private[bench] object Watched {

  /** What a started program wrote, a line each, how its wait ended (its exit and the tail of its
    * errors, or why the wait failed), and why it was stopped early, as the report says it.
    */
  final case class Read(lines: Vector[String], ended: Either[String, Ran], stopped: Option[String])

  /** `s` read, its input closed, until its output ends, it writes nothing for `quiet`, `stop` of
    * the lines so far gives a reason, or `watch` does, asked before its first line and then every
    * [[WatchEvery]] polls; then waited on for at most `longest`, and killed, with every process it
    * started still its descendant, early when it was stopped.
    */
  def ran(
      s: Started^,
      quiet: FiniteDuration,
      longest: FiniteDuration,
      stop: Vector[String] -> Option[String],
      watch: () => Option[String]
  ): Read = {
    s.input.close()
    val (lines, stopped) = streamOf(s, quiet, stop, watch)
    if (stopped.isDefined) s.kill()
    val ended = s.await(longest).left.map(_.message)
    s.kill()
    Read(lines, ended, stopped)
  }

  /** How often a run's output is checked for quiet: a quiet run is stopped within `quiet` and this
    * much longer.
    */
  private val Tick: FiniteDuration = 500.millis

  /** 4: every how many polls of a run's output [[ran]] asks its watch, so at most every 2 seconds
    * (4 [[Tick]]s) while the run writes nothing.
    */
  private val WatchEvery: Long = 4

  /** The lines `s` writes, a line each, and why they stopped early, as [[ran]] says. */
  private def streamOf(
      s: Started^,
      quiet: FiniteDuration,
      stop: Vector[String] -> Option[String],
      watch: () => Option[String]
  ): (Vector[String], Option[String]) = {
    @tailrec def collect(
        acc: Vector[String],
        idle: FiniteDuration,
        polls: Long
    ): (Vector[String], Option[String]) = {
      val since = polls + 1
      (if (polls % WatchEvery == 0) watch() else None) match {
        case Some(why) => (acc, Some(why))
        case None =>
          s.next(Tick) match {
            case Started.Next.Quiet if idle + Tick >= quiet =>
              (
                acc,
                Some(s"quiet for ${
                    if (quiet.toSeconds % 60 == 0) s"${quiet.toMinutes} min"
                    else s"${quiet.toSeconds} s"
                  }")
              )
            case Started.Next.Quiet => collect(acc, idle + Tick, since)
            case Started.Next.Ended => (acc, None)
            case Started.Next.Line(line) =>
              val lines = acc :+ line
              stop(lines) match {
                case Some(why) => (lines, Some(why))
                case None => collect(lines, 0.millis, since)
              }
          }
      }
    }
    collect(Vector.empty, 0.millis, 0L)
  }
}
