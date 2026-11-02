package grit.core.host

import scala.concurrent.duration.*

/** [[Runs]] for tests, keeping [[RunsContract]] in time of its own: nothing runs, and a wait
  * moves its clock on at once, up to the moment the run waited on ends. Each command is what
  * `scripts` says of it, and a command it says nothing of cannot start.
  */
final class ScriptedRuns(scripts: Map[Argv, ScriptedRuns.Script]) extends Runs {
  import ScriptedRuns.*

  /** Its runs by id, in the order started, and its clock. */
  // Only ever replaced by new immutable values, under this object's lock.
  @caps.unsafe.untrackedCaptures
  private var kept = Vector.empty[(RunId, Run)]

  // Its clock, as `kept`.
  @caps.unsafe.untrackedCaptures
  private var now: FiniteDuration = Duration.Zero

  // How many it has started, as `kept`: each id is made from the next.
  @caps.unsafe.untrackedCaptures
  private var started = 0

  def start(
      argv: Argv,
      timeout: FiniteDuration,
      wait: FiniteDuration
  ): Either[HostError, (id: RunId, status: RunStatus)] = synchronized {
    scripts.get(argv) match {
      case None => Left(HostError.Failed(s"${argv.program}: not found"))
      case Some(_) if kept.count((_, r) => going(r)) >= Runs.Most => Left(HostError.Busy(Runs.Most))
      case Some(s) =>
        started += 1
        val id = RunId.of(s"scripted-$started")
        val run = Run(argv, s, now, timeout.min(Runs.Longest), None)
        kept = kept :+ (id -> run)
        waited(run, wait)
        val status = statusOf(id)
        kept = forgetting(kept)
        status.map(st => (id = id, status = st))
    }
  }

  def tail(id: RunId, wait: FiniteDuration): Either[HostError, (argv: Argv, status: RunStatus)] =
    synchronized {
      find(id) match {
        case None => Left(HostError.Gone(id))
        case Some(run) =>
          waited(run, wait)
          statusOf(id).map(st => (argv = run.argv, status = st))
      }
    }

  def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)] = synchronized {
    find(id) match {
      case None => Left(HostError.Gone(id))
      case Some(run) =>
        if (going(run))
          kept = kept.map((k, r) => if (k == id) (k, r.copy(stopped = Some(now))) else (k, r))
        statusOf(id).map(st => (argv = run.argv, status = st))
    }
  }

  private def find(id: RunId): Option[Run] = kept.collectFirst { case (k, r) if k == id => r }

  /** When `run` ends by itself or is killed at its timeout, the sooner. */
  private def ends(run: Run): FiniteDuration = {
    val killed = run.from + run.timeout
    run.script.lasts.fold(killed)(l => (run.from + l).min(killed))
  }

  private def going(run: Run): Boolean = run.stopped.isEmpty && now < ends(run)

  /** The clock moved on by `wait`, or to when `run` ends, the sooner. */
  private def waited(run: Run, wait: FiniteDuration): Unit =
    if (going(run)) now = (now + wait).min(ends(run))

  private def statusOf(id: RunId): Either[HostError, RunStatus] =
    find(id).fold(Left(HostError.Gone(id))) { run =>
      val s = run.script
      def clipped(text: String) = Clipped.tail(text, _ => None)
      if (s.flood) {
        kept = kept.filterNot(_._1 == id)
        Left(HostError.TooLarge(RelPath.Root, Workspace.MaxFileBytes + 1, Workspace.MaxFileBytes))
      } else
        run.stopped match {
          case Some(at) => Right(RunStatus.Stopped(at - run.from, clipped(s.early)))
          case None if going(run) =>
            Right(RunStatus.Going(now - run.from, run.timeout, clipped(s.early)))
          case None if s.lasts.exists(_ <= run.timeout) =>
            Right(RunStatus.Ended(Ran(s.exit, clipped(s.early + s.late))))
          case None => Right(RunStatus.TimedOut(run.timeout, clipped(s.early)))
        }
    }

  /** `runs` with the ended ones past [[Runs.Kept]] forgotten, the one started first first. */
  private def forgetting(runs: Vector[(RunId, Run)]): Vector[(RunId, Run)] = {
    val ended = runs.filterNot((_, r) => going(r)).map(_._1)
    val forgotten = ended.take((ended.size - Runs.Kept).max(0)).toSet
    runs.filterNot((k, _) => forgotten.contains(k))
  }
}

object ScriptedRuns {

  /** What a command does: writes `early` at once, and `late` as it ends, `lasts` after it
    * starts (never, when `None`), with `exit`; `flood` writes past [[Workspace.MaxFileBytes]].
    */
  final case class Script(
      early: String,
      late: String,
      lasts: Option[FiniteDuration],
      exit: Int,
      flood: Boolean = false
  )

  private final case class Run(
      argv: Argv,
      script: Script,
      from: FiniteDuration,
      timeout: FiniteDuration,
      stopped: Option[FiniteDuration]
  )
}
