package grit.eval.harness.bench

import scala.concurrent.duration.*

import grit.core.host.{Argv, HostError, Processes, RelPath}

/** Each check run, by its command line, with whether it exited 0. */
final case class Checked(runs: Vector[(String, Boolean)]) extends caps.Pure {

  /** Whether every check exited 0. */
  def passed: Boolean = runs.forall(_._2)
}

/** A task's checks, run in its checkout by the checkout's own scripts. */
object Checks {

  /** 30 minutes: the longest a check runs. */
  val Longest: FiniteDuration = 30.minutes

  /** `checks`, then the law (`bash scripts/fetch-enola.sh`, then `bash scripts/enola-law.sh`),
    * run in `dir` in order; every one runs whatever an earlier gave. `Left` only when one cannot
    * start; one past [[Longest]] did not pass.
    */
  def run(work: Processes)(dir: RelPath, checks: Vector[Check]): Either[HostError, Checked] = {
    val lines = checks.map { c =>
      val script = c.runner match {
        case Runner.Check => "scripts/check"
        case Runner.It => "scripts/it"
      }
      script +: c.args
    } ++ Vector(Vector("bash", "scripts/fetch-enola.sh"), Vector("bash", "scripts/enola-law.sh"))
    val (refused, runs) = lines.partitionMap { line =>
      val ran = line.headOption
        .toRight(HostError.Failed("a check with no program"))
        .flatMap(program => Argv.of(program, line.drop(1)*).left.map(HostError.Failed(_)))
        .flatMap(work.run(_, dir, Longest)) match {
        case Right(r) => Right(r.exit == 0)
        case Left(HostError.TimedOut(_, _)) => Right(false)
        case Left(other) => Left(other)
      }
      ran.map(passed => line.mkString(" ") -> passed)
    }
    refused.headOption.toLeft(Checked(runs))
  }
}
