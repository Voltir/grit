package grit.core.host

import scala.concurrent.duration.*

import utest.*

/** The contract every [[Runs]] keeps, run against [[ScriptedRuns]] in core and `LocalRuns` in
  * grit.host.
  */
abstract class RunsContract extends TestSuite {

  /** `body` over runs in an empty checkout, every run killed after. */
  protected def fresh[A](body: Runs^ => A): A

  /** Writes `hi` and exits 3 at once. */
  protected def quick: Argv

  /** Writes `started`, then runs until it is killed. */
  protected def slow: Argv

  /** Writes `done` after about 300 ms, and exits 0. */
  protected def later: Argv

  /** Writes past [[Workspace.MaxFileBytes]] at once, and exits 0. */
  protected def flood: Argv

  /** A program this machine does not have. */
  protected def missing: Argv

  /** A wait a quick command ends within. */
  private val ample = 5.seconds

  /** A wait a slow command is still going after. */
  private val brief = 200.millis

  private def text(c: Clipped): String = c.text.trim

  val tests = Tests {
    test("a command that ends within the wait is Ended, with its exit code and output") {
      fresh { runs =>
        runs.start(quick, 10.seconds, ample).map(_.status) match {
          case Right(RunStatus.Ended(ran)) => (ran.exit, text(ran.output)) ==> (3, "hi")
          case other => throw new java.lang.AssertionError(s"not ended: $other")
        }
      }
    }

    test("a command still running after the wait is Going, with its output so far and timeout") {
      fresh { runs =>
        runs.start(slow, 10.seconds, brief).map(_.status) match {
          case Right(RunStatus.Going(_, timeout, output)) =>
            (timeout, text(output)) ==> (10.seconds, "started")
          case other => throw new java.lang.AssertionError(s"not going: $other")
        }
      }
    }

    test("tail with a wait sees a running command end, naming the command") {
      fresh { runs =>
        val got = for {
          started <- runs.start(later, 10.seconds, Duration.Zero)
          tailed <- runs.tail(started.id, ample)
        } yield (started.status, tailed)
        got match {
          case Right((RunStatus.Going(_, _, _), (argv, RunStatus.Ended(ran)))) =>
            (argv, ran.exit, text(ran.output)) ==> (later, 0, "done")
          case other => throw new java.lang.AssertionError(s"not going, then ended: $other")
        }
      }
    }

    test("stop kills a running command, and stopping it again answers the same") {
      fresh { runs =>
        val got = for {
          started <- runs.start(slow, 10.seconds, brief)
          first <- runs.stop(started.id)
          again <- runs.stop(started.id)
        } yield (first, again)
        got match {
          case Right(((argv, RunStatus.Stopped(_, out)), (_, RunStatus.Stopped(_, again)))) =>
            (argv, text(out), text(again)) ==> (slow, "started", "started")
          case other => throw new java.lang.AssertionError(s"not stopped: $other")
        }
      }
    }

    test("a command past its timeout is killed: TimedOut, with its output until then") {
      fresh { runs =>
        val got = for {
          started <- runs.start(slow, 300.millis, Duration.Zero)
          tailed <- runs.tail(started.id, ample)
        } yield tailed.status
        got match {
          case Right(RunStatus.TimedOut(after, out)) =>
            (after, text(out)) ==> (300.millis, "started")
          case other => throw new java.lang.AssertionError(s"not timed out: $other")
        }
      }
    }

    test("a timeout past Runs.Longest is cut to it") {
      fresh { runs =>
        runs.start(slow, Runs.Longest + 1.hour, brief).map(_.status) match {
          case Right(RunStatus.Going(_, timeout, _)) => timeout ==> Runs.Longest
          case other => throw new java.lang.AssertionError(s"not going: $other")
        }
      }
    }

    test("a start while Runs.Most are going is Busy, and one starts again once one is stopped") {
      fresh { runs =>
        val going = (1 to Runs.Most).map(_ => runs.start(slow, 10.seconds, Duration.Zero))
        val busy = runs.start(quick, 10.seconds, ample).map(_.status)
        val stopped = going.headOption.flatMap(_.toOption).map(g => runs.stop(g.id).isRight)
        val again = runs.start(quick, 10.seconds, ample).map(_.status)
        (going.count(_.isRight), busy, stopped, again.isRight) ==>
          (Runs.Most, Left(HostError.Busy(Runs.Most)), Some(true), true)
      }
    }

    test("an ended run is forgotten once Runs.MostEnded newer ones have ended: Gone") {
      fresh { runs =>
        val ids =
          (0 to Runs.MostEnded).flatMap(_ =>
            runs.start(quick, 10.seconds, ample).toOption.map(_.id)
          )
        val first = ids.headOption.map(id => runs.tail(id, Duration.Zero))
        val last = ids.lastOption.map(id => runs.tail(id, Duration.Zero).isRight)
        (ids.size, first, last) ==>
          (Runs.MostEnded + 1, ids.headOption.map(id => Left(HostError.Gone(id))), Some(true))
      }
    }

    test("an id it never issued is Gone, to tail and to stop") {
      fresh { runs =>
        val id = RunId.of("never issued")
        (runs.tail(id, Duration.Zero), runs.stop(id)) ==>
          (Left(HostError.Gone(id)), Left(HostError.Gone(id)))
      }
    }

    test("a command whose output passes Workspace.MaxFileBytes is TooLarge") {
      fresh { runs =>
        runs.start(flood, 10.seconds, ample) match {
          case Left(HostError.TooLarge(_, bytes, max)) =>
            (bytes > max, max) ==> (true, Workspace.MaxFileBytes)
          case other => throw new java.lang.AssertionError(s"not too large: $other")
        }
      }
    }

    test("a run whose output passed the most is forgotten, so it never pushes an ended run out") {
      fresh { runs =>
        val first = runs.start(quick, 10.seconds, ample).toOption.map(_.id)
        val floods = (1 to Runs.MostEnded).map(_ => runs.start(flood, 10.seconds, ample))
        (
          floods.count {
            case Left(HostError.TooLarge(_, _, _)) => true
            case _ => false
          },
          first.map(id =>
            runs
              .tail(id, Duration.Zero)
              .map(_.status match {
                case RunStatus.Ended(ran) => Some((ran.exit, text(ran.output)))
                case _ => None
              })
          )
        ) ==> (Runs.MostEnded, Some(Right(Some((3, "hi")))))
      }
    }

    test("a program that cannot start is Failed, naming it, and nothing is kept") {
      fresh { runs =>
        val failed = runs.start(missing, 10.seconds, ample) match {
          case Left(HostError.Failed(why)) => why.contains(missing.program)
          case _ => false
        }
        val ids = (1 to Runs.Most).flatMap(_ => runs.start(slow, 10.seconds, brief).toOption)
        (failed, ids.size) ==> (true, Runs.Most)
      }
    }
  }
}
