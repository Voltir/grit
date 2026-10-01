package grit.mcp.edge

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.{EdgeName, PrincipalId}
import grit.core.place.Service
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.edge.Server
import grit.mcp.client.{Bearer, McpClient, McpServer}
import grit.mcp.wire.McpError

/** An edge hosting MCP servers' tools at a service's place (ADR 0017). */
object McpEdge {

  /** The edge named for `service`, hosting `servers`' tools there, or why not: no servers, or
    * two with one name. Opened, it registers the service's place for grit itself
    * ([[PrincipalId.Grit]]), logs each listed tool it skips and why, advertises there each
    * server's tools ([[McpTools.offered]]), and serves the requests addressed there with
    * [[McpTools]]. It needs each server's token variable, answers no ask, and delivers no
    * replies. Opening it is refused when a token is unset or malformed ([[Bearer.of]]); when
    * a server's list cannot be read, it naming the server and why, and for a refused token
    * the variable; when a server lists no tool grit may offer; or when the desk cannot be
    * registered or advertise.
    */
  def serving(service: Service, servers: Vector[McpServer]): Either[String, ServedEdge] = {
    val names = servers.map(_.name)
    if (servers.isEmpty) Left(s"the MCP edge for ${service.place.written} names no server")
    else
      names.diff(names.distinct).headOption match {
        case Some(repeated) => Left(s"two MCP servers are named $repeated")
        case None => Right(new Edge(service, servers))
      }
  }

  private final class Edge(service: Service, servers: Vector[McpServer]) extends ServedEdge {

    def name: EdgeName = EdgeName(service.name)

    def needs: Vector[Variable] = servers.map(_.token).distinct

    def answersAsks: Boolean = false

    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}] = {
      val clock: Clock = Clock.system()
      val place = service.place
      val bearers = servers.foldLeft[Either[EdgeRefusal, Vector[Bearer]]](Right(Vector.empty)) {
        (read, server) => read.flatMap(bs => Bearer.of(env, server.token).map(bs :+ _))
      }
      bearers match {
        case Left(refusal) => Left(refusal)
        case Right(read) =>
          val clients: Vector[McpClient^{clock}] =
            servers.zip(read).map((server, bearer) => new McpClient(server, bearer, clock))
          clients.map(listing(_, log)).collectFirst { case Left(refusal) => refusal } match {
            case Some(refusal) => Left(refusal)
            case None =>
              stores.desks.register(PrincipalId.Grit, Set(place)) match {
                case Left(e) =>
                  Left(EdgeRefusal.Failed(s"its desk could not be registered: ${e.why}"))
                case Right(desk) =>
                  val tools = new McpTools(
                    clock,
                    clients,
                    // Not logged: `log` goes to the server, and a refusal is tried again on the
                    // next run.
                    set => desk.advertise(place, set, Vector.empty)
                  )
                  tools.offered() match {
                    case Left(e) =>
                      Left(EdgeRefusal.Failed(s"its tools could not be advertised: ${e.why}"))
                    case Right(offered) =>
                      log(
                        s"${place.written}: serving " +
                          offered.tools.map(e => ToolName.value(e.name)).mkString(", ")
                      )
                      val server = new Server(
                        desk,
                        tools,
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

    /** `client`'s list read at open, each tool skipped logged; why it is refused. */
    private def listing(client: McpClient^, log: String => Unit): Either[EdgeRefusal, Unit] = {
      val server = client.server
      client.tools() match {
        case Left(e) =>
          val token = e match {
            case McpError.Unauthorized(_) => s"; check ${Variable.value(server.token)}"
            case _ => ""
          }
          Left(EdgeRefusal.Refused(s"MCP server ${server.name} ${e.message}$token"))
        case Right(listed) =>
          listed.skipped.foreach(s => log(s"MCP server ${server.name}: ${s.message}"))
          Either.cond(
            listed.tools.nonEmpty,
            (),
            EdgeRefusal.Refused(s"MCP server ${server.name} lists no tool grit may offer")
          )
      }
    }
  }
}
