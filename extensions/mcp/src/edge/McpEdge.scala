package grit.mcp.edge

import grit.core.clock.Clock
import grit.core.edge.{Console, Desk, Desks, EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.host.Processes
import grit.core.id.{EdgeName, PrincipalId}
import grit.core.place.Service
import grit.core.prompt.Fragment
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.edge.{PlaceFragments, Server}
import grit.mcp.client.{Authorizer, Credential, KeyFiles, McpClient, McpServer, McpTransport}
import grit.mcp.wire.{McpError, Skipped}

/** An edge hosting MCP servers' tools at a service's place (ADR 0017). */
object McpEdge {

  /** The edge named for `service`, hosting `servers`' tools there, or why not: no servers, two
    * with one name, or `instructions` given blank. Opened, it registers the service's place for
    * grit itself ([[PrincipalId.Grit]]); logs in one line the listed tools each server's
    * allowlist leaves out, in one line those its scope cannot hold, in one line the allowlisted
    * names it does not list, and each other tool it skips on a line of its own; advertises
    * there each server's tools ([[McpTools.offered]]) with `instructions`, when given, as the
    * place's Place layer ([[PlaceFragments.declared]]): an aid to the model, never a bound on
    * what it reaches, which is each server's scope; and serves the requests addressed there
    * with [[McpTools]]. It needs each server's credential's variables, is never asked, and
    * delivers no replies. Opening it is refused when a credential cannot be opened
    * ([[Credential.open]]); when a server's list cannot be read, it naming the server and why,
    * and for a refused token or one not got its credential's variables; when a server lists
    * no tool grit may offer; or when the desk cannot be
    * registered or advertise.
    */
  def serving(
      service: Service,
      servers: Vector[McpServer[McpTransport.Http]],
      instructions: Option[String] = None
  ): Either[String, ServedEdge] = {
    val names = servers.map(_.name)
    if (servers.isEmpty) Left(s"the MCP edge for ${service.place.written} names no server")
    else
      names.diff(names.distinct).headOption match {
        case Some(repeated) => Left(s"two MCP servers are named $repeated")
        case None =>
          instructions
            .map(PlaceFragments.declared(service.place, _))
            .fold(Right(Vector.empty))(_.map(Vector(_)))
            .map(new Edge(service, servers, _))
      }
  }

  /** The edge [[serving]] describes; `declared`, the Place layer it advertises. */
  private final class Edge(
      service: Service,
      servers: Vector[McpServer[McpTransport.Http]],
      declared: Vector[Fragment]
  ) extends ServedEdge {

    def name: EdgeName = EdgeName(service.name)

    def needs: Vector[Variable] = servers.flatMap(_.transport.credential.needs).distinct

    def asks: ServedEdge.Asks = ServedEdge.Asks.Never

    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      val opened =
        servers.foldLeft[Either[EdgeRefusal, Vector[Authorizer]]](Right(Vector.empty)) {
          (read, server) =>
            read.flatMap(as =>
              Credential.open(server.transport.credential, env, KeyFiles.Local).map(as :+ _)
            )
        }
      opened match {
        case Left(refusal) => Left(refusal)
        case Right(authorizers) =>
          val clients: Vector[McpClient^{clock}] =
            servers.zip(authorizers).map((server, a) => McpClient(server, a, clock))
          clients.map(listing(_, log)).collectFirst { case Left(refusal) => refusal } match {
            case Some(refusal) => Left(refusal)
            case None =>
              stores.desks.register(PrincipalId.Grit, Set(service.place)) match {
                case Left(e) =>
                  Left(EdgeRefusal.Failed(s"its desk could not be registered: ${e.why}"))
                case Right(desk) =>
                  hosting(desk, service, clock, clients, declared, log).map { server =>
                    new ServedEdge.Open {
                      def deliver(): Either[StoreError, Int] = Right(0)
                      def close(): Unit = server.close()
                    }
                  }
              }
          }
      }
    }
  }

  /** The tools `clients` list, advertised through `desk` at `service`'s place with `declared`
    * as its Place layer, logged, and served there from now on, each call of a server reading the
    * asking conversation's directory sent with the directory of its request's conversation
    * ([[grit.core.edge.Desk.origin]]); `Failed` when the tools could not be advertised.
    */
  private def hosting(
      desk: Desk^,
      service: Service,
      reach: AnyRef^,
      clients: Vector[McpClient^{reach}],
      declared: Vector[Fragment],
      log: String => Unit
  ): Either[EdgeRefusal, Server^{desk, reach, log}] = {
    val place = service.place
    val tools = new McpTools(
      reach,
      clients,
      // Not logged: `log` goes to the server, and a refusal is tried again on the next run.
      set => desk.advertise(place, set, declared),
      request => desk.origin(request).left.map(_.why).map(_.flatMap(_.place.directory))
    )
    tools.offered() match {
      case Left(e) => Left(EdgeRefusal.Failed(s"its tools could not be advertised: ${e.why}"))
      case Right(offered) =>
        log(
          s"${place.written}: serving " + offered.tools
            .map(e => ToolName.value(e.name))
            .mkString(", ")
        )
        val server = new Server(
          desk,
          tools,
          run => { val _ = Thread.ofVirtual().start(() => run()) },
          said => log(s"${place.written}: $said")
        )
        server.serve()
        Right(server)
    }
  }

  /** The edge hosting the tools of `server`, a program `processes` start (its client
    * [[McpClient.stdio]], reading `clock`), at `service`'s place for the process that runs a
    * console, opened: its list is read now, its skips logged as [[serving]] logs them; or why
    * not: `instructions` given blank, the list unreadable (naming the server and why), or no tool
    * grit may offer. Served ([[Console.Edge.serve]]), it registers the place for grit itself
    * ([[PrincipalId.Grit]]), advertises the tools there with `instructions`, when given, as the
    * place's Place layer, and serves the requests addressed there as [[serving]]'s edge does:
    * each call of a server reading the asking conversation's directory
    * ([[McpServer.directoryArgument]]) is sent with its request's conversation's directory, and
    * one from a conversation that works in no directory is answered, unsent, saying so. A
    * registration or advert that fails is logged, and the place is then served by no edge here.
    * Several processes may serve one place: each call names its own directory. Closed, its
    * program is killed, and later calls are answered unsent.
    */
  def local(
      service: Service,
      server: McpServer[McpTransport.Stdio],
      processes: Processes^,
      clock: Clock^,
      instructions: Option[String],
      log: String -> Unit
  ): Either[String, Local^] = {
    val declared = instructions
      .map(PlaceFragments.declared(service.place, _))
      .fold(Right(Vector.empty))(_.map(Vector(_)))
    declared match {
      case Left(why) => Left(why)
      case Right(fragments) =>
        val client: McpClient^{processes, clock} = McpClient.stdio(server, processes, clock)
        listing(client, log) match {
          case Left(refusal) =>
            client.close()
            Left(refusal.message)
          case Right(()) =>
            val local: Local^ = new Local(service, processes, clock, client, fragments, log)
            Right(local)
        }
    }
  }

  /** The edge [[local]] describes; `declared`, the Place layer it advertises. */
  final class Local private[McpEdge] (
      service: Service,
      processes: Processes^,
      clock: Clock^,
      client: McpClient^{processes, clock},
      declared: Vector[Fragment],
      log: String -> Unit
  ) extends Console.Edge,
        AutoCloseable {

    def serve(desks: Desks^): Unit =
      desks.register(PrincipalId.Grit, Set(service.place)) match {
        case Left(e) =>
          log(s"${service.place.written}: not served: its desk could not be registered: ${e.why}")
        case Right(desk) =>
          hosting(desk, service, client, Vector(client), declared, log).left.foreach(refusal =>
            log(s"${service.place.written}: not served: ${refusal.message}")
          )
      }

    /** Kills its server's program; later calls are answered unsent. */
    def close(): Unit = client.close()
  }

  /** "1 listed tool is", or "n listed tools are", for `tools`. */
  private def counted(tools: Vector[String]): String =
    if (tools.size == 1) "1 listed tool is" else s"${tools.size} listed tools are"

  /** `client`'s list read when its edge opens, its skips logged as [[serving]] says; why it is
    * refused.
    */
  private def listing(client: McpClient^, log: String => Unit): Either[EdgeRefusal, Unit] = {
    val server = client.server
    client.tools() match {
      case Left(e) =>
        val token = (e, server.transport) match {
          case (
                McpError.Unauthorized(_) | McpError.NoToken(_),
                McpTransport.Http(_, credential)
              ) =>
            s"; check ${credential.needs.map(Variable.value).mkString(", ")}"
          case _ => ""
        }
        Left(EdgeRefusal.Refused(s"MCP server ${server.name} ${e.message}$token"))
      case Right(listed) =>
        val said = s"MCP server ${server.name}:"
        val unallowed = listed.skipped.collect { case Skipped.NotAllowed(tool) => tool }
        val outside = listed.skipped.collect { case Skipped.OutOfScope(tool) => tool }
        listed.skipped.foreach {
          case Skipped.NotAllowed(_) | Skipped.OutOfScope(_) => ()
          case other => log(s"$said ${other.message}")
        }
        if (unallowed.nonEmpty)
          log(s"$said ${counted(unallowed)} not on its allowlist: ${unallowed.mkString(", ")}")
        if (outside.nonEmpty)
          log(s"$said ${counted(outside)} outside its scope: ${outside.mkString(", ")}")
        if (listed.unlisted.nonEmpty)
          log(
            s"$said its allowlist names tools it does not list: " +
              listed.unlisted.toVector.sorted.mkString(", ")
          )
        Either.cond(
          listed.tools.nonEmpty,
          (),
          EdgeRefusal.Refused(s"MCP server ${server.name} lists no tool that may be offered")
        )
    }
  }
}
