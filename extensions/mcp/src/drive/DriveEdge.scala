package grit.mcp.drive

import java.net.URI

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.{EdgeName, PrincipalId}
import grit.core.prompt.Fragment
import grit.core.store.StoreError
import grit.edge.{PlaceFragments, Server}
import grit.mcp.client.{Credential, KeyFiles}

/** An edge serving Drive's files at its service's place (ADR 0017). */
object DriveEdge {

  /** What the model is told of Drive at its place: the Place layer [[serving]] advertises by
    * default.
    */
  val Instructions: String =
    "drive_search finds Drive files by the words in them, and drive_read reads one by its id. " +
      "Files in folders that no one has labelled yet are not shown, so a folder may look " +
      "empty when it isn't. If someone asks about such a folder, tell them an administrator " +
      "can open it to you with the label command, typing " +
      s"${DriveItems.Opening}: the link Drive shows for the folder, or " +
      "the folder's id from a search."

  /** The edge named for `reading`'s service, serving [[DriveTools.Search]] and
    * [[DriveTools.Read]] there over Drive's API at `api` with `credential`'s tokens, read as
    * `reading` says, with `instructions` as the place's Place layer; or why not:
    * `instructions` blank, or `api` not an absolute URL. Opened, it registers the service's
    * place for grit itself ([[PrincipalId.Grit]]), advertises the two there, logs that it
    * serves them, and serves the requests addressed there with [[DriveTools]], logging what
    * they say of each run. It needs the
    * credential's variables, answers no ask, and delivers no replies. Opening it is refused,
    * naming the credential's variables, when the credential does not open (its key read from
    * this machine's files, [[KeyFiles.Local]]) or Drive does not
    * take its token; and when Drive cannot be reached, or the desk cannot be registered or
    * advertise.
    */
  def serving(
      credential: Credential,
      reading: DriveReading,
      instructions: String = Instructions,
      api: String = DriveRest.Api
  ): Either[String, ServedEdge] =
    for {
      at <- scala.util
        .Try(URI.create(api))
        .toOption
        .filter(_.isAbsolute)
        .toRight(s"Drive's API $api is not an absolute URL")
      declared <- PlaceFragments.declared(reading.service.place, instructions)
    } yield new Edge(credential, reading, at, declared)

  /** The edge [[serving]] describes; `declared`, the Place layer it advertises. */
  private final class Edge(
      credential: Credential,
      reading: DriveReading,
      api: URI,
      declared: Fragment
  ) extends ServedEdge {

    def name: EdgeName = EdgeName(reading.service.name)

    def needs: Vector[Variable] = credential.needs

    def answersAsks: Boolean = false

    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      val place = reading.service.place
      val check = s"check ${credential.needs.map(Variable.value).mkString(", ")}"
      Credential.open(credential, env, KeyFiles.Local).flatMap { authorizer =>
        val rest: DriveRest^{clock} = new DriveRest(api, authorizer, clock)
        rest.reachable() match {
          case Left(DriveError.NoToken(why)) =>
            Left(EdgeRefusal.Refused(s"Drive gave grit no access ($why); $check"))
          case Left(DriveError.Malformed(why)) =>
            Left(EdgeRefusal.Refused(s"Drive's answer could not be read: $why"))
          case Left(other) =>
            Left(EdgeRefusal.Refused(s"Drive could not be read: ${other.message}"))
          case Right(()) =>
            stores.desks.register(PrincipalId.Grit, Set(place)) match {
              case Left(e) =>
                Left(EdgeRefusal.Failed(s"its desk could not be registered: ${e.why}"))
              case Right(desk) =>
                desk.advertise(place, DriveTools.Offered, Vector(declared)) match {
                  case Left(e) =>
                    Left(EdgeRefusal.Failed(s"its tools could not be advertised: ${e.why}"))
                  case Right(()) =>
                    log(s"${place.written}: serving drive_search, drive_read")
                    val server = new Server(
                      desk,
                      new DriveTools(clock, rest, reading),
                      run => { val _ = Thread.ofVirtual().start(() => run()) },
                      said => log(s"${place.written}: $said")
                    )
                    server.serve()
                    Right(new ServedEdge.Open {
                      def deliver(): Either[StoreError, Int] = Right(0)
                      def close(): Unit = server.close()
                    })
                }
            }
        }
      }
    }
  }
}
