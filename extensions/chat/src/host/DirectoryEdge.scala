package grit.chat.host

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.edge.Desks
import grit.core.host.Instructions
import grit.core.id.PrincipalId
import grit.core.place.Place
import grit.core.prompt.Fragment
import grit.core.provider.TokenEstimator
import grit.core.tool.ToolSet
import grit.edge.{PlaceFragments, Server, Tools}

/** The edge a chat's process serves for its own directory (ADR 0017). */
object DirectoryEdge {

  /** How often the instruction files are read again: 5 s. */
  val AdvertEvery: FiniteDuration = 5.seconds

  /** This process's edge, registered through `desks` for `place`: it offers `hosted` there,
    * with the instruction files `instructions` finds around the place's directory (estimated by
    * `estimator`), and runs each request addressed to it with `tools`, on a virtual thread, until
    * the console closes. The files are read again every [[AdvertEvery]] until the process ends,
    * and advertised when they changed, so an edit reaches the next turn without a restart. A
    * registration or an advert that fails is told to `warn`, and turns are then served by no
    * edge here; what the server says of a run, to `info`.
    */
  def serve(
      desks: Desks^,
      place: Place,
      hosted: ToolSet,
      instructions: Instructions^,
      estimator: TokenEstimator,
      tools: Tools^,
      warn: String => Unit,
      info: String => Unit
  ): Unit = {
    val first: Vector[Fragment] =
      place.directory.fold(Vector.empty)(dir =>
        PlaceFragments.of(instructions.around(dir), estimator)
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
              val now = PlaceFragments.of(instructions.around(dir), estimator)
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
