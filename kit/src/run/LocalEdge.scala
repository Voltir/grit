package grit.kit.run

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.assembly.estimate.CharEstimate
import grit.core.edge.{Console, Desks}
import grit.core.host.Instructions
import grit.core.id.PrincipalId
import grit.core.place.{Directory, Place}
import grit.core.prompt.Fragment
import grit.core.tool.{SetRefusal, ToolSet}
import grit.edge.{PlaceFragments, Server, Tools}

/** The edge a process serves for its own checkout (ADR 0017). */
object LocalEdge {

  /** How often the instruction files are read again: 5 s. */
  val AdvertEvery: FiniteDuration = 5.seconds

  /** The edge a process serves over its checkout `directory`, for [[Kit.console]]: registered
    * at the directory's place, it offers there what `tools` runs, with the instruction files
    * `instructions` finds around the directory (estimated by [[CharEstimate]]), and runs each
    * request routed to it with `tools`, on a virtual thread, until the console closes. The
    * files are read again every [[AdvertEvery]] until the process ends, and advertised when
    * they changed, so an edit reaches the next turn without a restart. A registration or an
    * advert that fails is logged to `log` as a warning, and turns are then served by no edge
    * here; what the server says of a run, as information. `Left` names a tool `tools` cannot offer
    * with the others.
    */
  def over(
      directory: Directory,
      tools: LocalTools,
      instructions: Instructions^,
      log: org.slf4j.Logger
  ): Either[SetRefusal, Console.Edge^] =
    tools.hosted match {
      case Left(twice) => Left(twice)
      case Right(hosted) =>
        val edge: Console.Edge^ = desks =>
          serve(desks, Place.of(directory), hosted, instructions, tools, log.warn(_), log.info(_))
        Right(edge)
    }

  private def serve(
      desks: Desks^,
      place: Place,
      hosted: ToolSet,
      instructions: Instructions^,
      tools: Tools^,
      warn: String => Unit,
      info: String => Unit
  ): Unit = {
    val first: Vector[Fragment] =
      place.directory.fold(Vector.empty)(dir =>
        PlaceFragments.of(instructions.around(dir), CharEstimate)
      )
    desks.register(PrincipalId.Local, Set(place)) match {
      case Left(e) => warn(s"this edge could not register: ${e.why}")
      case Right(desk) =>
        desk
          .advertise(place, hosted, first)
          .left
          .foreach(e => warn(s"not advertised: ${e.why}"))
        place.directory.foreach { dir =>
          val _ = Thread.ofVirtual().name("grit-advert").start { () =>
            var sent: Vector[Fragment] = first
            while (true) {
              Thread.sleep(AdvertEvery.toMillis)
              val now = PlaceFragments.of(instructions.around(dir), CharEstimate)
              if (now != sent)
                desk.advertise(place, hosted, now) match {
                  case Right(()) => sent = now
                  case Left(e) => warn(s"not advertised: ${e.why}")
                }
            }
          }
        }
        new Server(
          desk,
          tools,
          run => { val _ = Thread.ofVirtual().name("grit-tool").start(() => run()) },
          info
        ).serve()
    }
  }
}
