package grit.app.main

import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.edge.{Console, Desks, Holder}
import grit.core.id.PrincipalId
import grit.core.place.{Directory, Place}
import grit.dbos.sql.TestPostgres
import grit.kit.environment.Secrets
import grit.kit.run.Kit

import utest.*

/** Against Postgres and DBOS: the way a process with its own screen reaches a database's engine
  * ([[Kit.console]]) runs the engine in the process that takes its lock, and attaches any other
  * process to that engine, each serving its own edge through the console it opened.
  */
object ConsoleLiveTests extends TestSuite {

  /** An edge that registers `place` through the desks it is given, keeping whether it could. */
  private final class Registering(place: Place, registered: ConcurrentLinkedQueue[Boolean])
      extends Console.Edge {
    def serve(desks: Desks^): Unit = {
      val _ = registered.add(desks.register(PrincipalId.Local, Set(place)).isRight)
    }
  }

  /** A holder as a process names it: its machine, pid and start, not its moving heartbeat. */
  private def named(holder: Option[Holder]): Option[(String, Long, java.time.Instant)] =
    holder.map(h => (h.machine, h.pid, h.started))

  val tests = Tests {
    test(
      "the console of the process taking the lock runs the engine here, the next attaches to it, and each serves its edge"
    ) {
      val config = TestPostgres.freshDatabase("console_runs_or_attaches")
      val deployment = ServedDeployment.of()
      val secrets =
        Secrets
          .of(ServedDeployment.env(config), deployment)
          .fold(r => sys.error(r.message), identity)
      val clock = Clock.system()
      val registered = new ConcurrentLinkedQueue[Boolean]()
      def edge(dir: String): Console.Edge^ =
        new Registering(Place.of(Directory.of(dir).fold(sys.error, identity)), registered)
      val first =
        Kit.console(deployment, secrets, 0.millis, clock, edge("/tmp/console-first")) match {
          case Left(failure) => sys.error(failure.message)
          case Right(opener) => opener
        }
      val here = first.open() match {
        case Left(why) => sys.error(why)
        case Right(console) => console
      }
      try {
        val second =
          Kit.console(deployment, secrets, 0.millis, clock, edge("/tmp/console-second")) match {
            case Left(failure) => sys.error(failure.message)
            case Right(opener) => opener
          }
        val attached = second.open() match {
          case Left(why) => sys.error(why)
          case Right(console) => console
        }
        try {
          val holder = named(here.holder())
          val runners = (
            first.runner,
            second.runner match {
              case Console.Runner.Elsewhere(h) => Some(named(h))
              case Console.Runner.Here => None
            }
          )
          (
            runners,
            holder.map(_._2),
            named(attached.holder()),
            registered.asScala.toVector
          ) ==> (
            (Console.Runner.Here, Some(holder)),
            Some(ProcessHandle.current().pid()),
            holder,
            Vector(true, true)
          )
        } finally attached.close()
      } finally here.close()
    }
  }
}
