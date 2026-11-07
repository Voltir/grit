package grit.eval.harness.bench

import scala.annotation.tailrec
import scala.concurrent.duration.*

import grit.core.host.{HostError, Processes, RelPath}

/** Each check run, by its command line, with whether it exited 0, and why the one after them was
  * stopped, when it was, as the report says it.
  */
final case class Checked(runs: Vector[(String, Boolean)], stopped: Option[String])
    extends caps.Pure {

  /** Whether every check ran and exited 0. */
  def passed: Boolean = stopped.isEmpty && runs.forall(_._2)

  /** A line each: `check: <line> passed` or `failed`, then `checks stopped: <why>` when stopped. */
  def lines: Vector[String] =
    runs.map((run, ok) => s"check: $run ${if (ok) "passed" else "failed"}") ++
      stopped.map(why => s"checks stopped: $why")
}

/** A task's checks, run in its checkout by the checkout's own scripts. */
object Checks {

  /** 30 minutes: the longest a check runs. */
  val Longest: FiniteDuration = 30.minutes

  /** `checks`, then the law (`bash scripts/fetch-enola.sh`, then `bash scripts/enola-law.sh`),
    * run in `dir` in order, each with `variables` added to its environment; every one runs
    * whatever an earlier gave. `watch` is asked while each runs, as [[Watched.ran]] asks it: a
    * reason it gives stops that check, and none after it runs. `Left` only when one cannot start;
    * one past [[Longest]] did not pass.
    */
  def run(work: Processes)(
      dir: RelPath,
      checks: Vector[Check],
      variables: Vector[(String, String)],
      watch: () => Option[String]
  ): Either[HostError, Checked] = {
    val lines = checks.map { c =>
      val script = c.runner match {
        case Runner.Check => "scripts/check"
        case Runner.It => "scripts/it"
      }
      script +: c.args
    } ++ Vector(Vector("bash", "scripts/fetch-enola.sh"), Vector("bash", "scripts/enola-law.sh"))
    @tailrec def go(
        left: Vector[Vector[String]],
        runs: Vector[(String, Boolean)]
    ): Either[HostError, Checked] =
      left.headOption match {
        case None => Right(Checked(runs, None))
        case Some(line) =>
          val shown = line.mkString(" ")
          val read = Watched
            .limited(Longest, (Vector("env") ++ variables.map((k, v) => s"$k=$v") ++ line)*)
            .left
            .map(HostError.Failed(_))
            .flatMap(
              work.started(_, dir)(
                Watched.ran(_, Longest + 1.minute, Longest + 2.minutes, _ => None, watch)
              )
            )
          read match {
            case Left(e) => Left(e)
            case Right(Watched.Read(_, _, Some(why))) =>
              Right(Checked(runs, Some(s"`$shown` was stopped: $why")))
            case Right(Watched.Read(_, ended, None)) =>
              go(left.drop(1), runs :+ (shown -> ended.exists(_.exit == 0)))
          }
      }
    go(lines, Vector.empty)
  }
}
