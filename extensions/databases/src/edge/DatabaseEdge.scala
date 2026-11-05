package grit.databases.edge

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.prompt.Fragment
import grit.core.secret.Secret
import grit.core.store.StoreError
import grit.databases.connect.{DatabaseReader, OpenRefusal}
import grit.databases.declare.{Database, Login}
import grit.edge.{PlaceFragments, Server}

/** An edge serving an application's database at its service's place (ADR 0057). */
object DatabaseEdge {

  /** The edge named for `database.service`, serving `{service}_tables` and `{service}_query`
    * ([[DatabaseTools]]) at its place over a [[DatabaseReader]] of its own, with
    * `database.instructions` as the place's Place layer. Neither declares a write, so a turn is
    * offered them only where it reads from and sends to that place (the deployment's room label
    * and its `Cleared` for the service); what they answer is believed at the place's cap. It
    * needs the URL's and the login's variables, is never asked, and delivers no replies. Or why
    * not: `instructions` blank, or the service's name over 57 characters (a tool's is at most
    * 64). Opening it is refused as [[DatabaseReader.open]] refuses, in that refusal's line, which
    * names variables, never a secret's value (an unset variable as [[EdgeRefusal.Missing]]); or
    * when the desk cannot be registered or advertise, the reader then closed. Closing it stops
    * serving and closes the reader.
    */
  def serving(database: Database): Either[String, ServedEdge] =
    for {
      declared <- PlaceFragments.declared(database.service.place, database.instructions)
      described <- DatabaseTools.described(database)
    } yield new Edge(database, declared, described)

  /** The edge [[serving]] describes; `declared`, the Place layer it advertises. */
  private final class Edge(
      database: Database,
      declared: Fragment,
      described: DatabaseTools.Described
  ) extends ServedEdge {

    def name: EdgeName = EdgeName(database.service.name)

    def needs: Vector[Variable] = database.url +: (database.login match {
      case Login.Password(user, password) => Vector(user, password)
      case driver: Login.Driver => driver.secrets.toVector.sortBy(_._1).map(_._2)
    })

    def asks: ServedEdge.Asks = ServedEdge.Asks.Never

    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
      DatabaseReader.open(database, env) match {
        case Left(refusal) => Left(refused(refusal))
        case Right(reader) =>
          val place = database.service.place
          val served = Server.open(stores.desks, place, log) { desk =>
            desk
              .advertise(place, described.offered, Vector(declared))
              .map(_ =>
                new Server.Advertised(
                  described.offered,
                  new DatabaseTools(database, described, reader)
                )
              )
          }
          served match {
            case Left(refusal) =>
              reader.close()
              Left(refusal)
            case Right(open) =>
              Right(new ServedEdge.Open {
                def deliver(): Either[StoreError, Int] = open.deliver()
                def close(): Unit = {
                  open.close()
                  reader.close()
                }
              })
          }
      }
  }

  /** `refusal` as the edge's: an unset variable as missing, any other in its own line. */
  private def refused(refusal: OpenRefusal): EdgeRefusal = refusal match {
    case OpenRefusal.Unread(Secret.Refusal.Unset(v)) => EdgeRefusal.Missing(v)
    case other => EdgeRefusal.Refused(other.message)
  }
}
