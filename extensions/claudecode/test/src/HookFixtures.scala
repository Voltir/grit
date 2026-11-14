package grit.claudecode

import java.io.IOException
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.commands.edge.{CommandsEdge, EdgeFixtures}
import grit.commands.routes.RouteFixtures.*
import grit.commands.worker.Holders
import grit.core.clock.{Clock, SetClock}
import grit.core.edge.{EdgeContracts, EdgeSessions, ScriptedSessions}
import grit.core.host.Argv
import grit.core.id.{AttesterName, ClaimHolder, EdgeName, Harness, SessionId}
import grit.core.identity.Realm
import grit.core.place.Directory
import grit.core.store.InMemoryVoucher
import grit.core.visibility.Visibility
import grit.edge.http.{Hosts, Listen}

/** Claude Code's hook against the commands edge itself: the edge serving [[ClaudeCode.adapter]] on
  * a socket of this machine, over sessions `script` holds, and `proj`, a checkout of `machine`
  * joined to it, its hook run as Claude Code runs a handler's command.
  */
final class HookFixtures(
    val machine: JoinFixtures,
    val port: Int,
    val script: ScriptedSessions.Script,
    val proj: Path
) {

  /** The edge's address. */
  val edge: String = s"http://127.0.0.1:$port"

  /** The environment Claude Code gives a hook in `proj`. */
  val claude: Map[String, String] = Map("CLAUDE_PROJECT_DIR" -> proj.toString)

  /** The hook `proj`'s join installed. */
  def hook: Path = machine.hook(proj)

  /** The hook's `role` with `args`, run in `in` as Claude Code runs a handler's command, reading
    * `payload`, given `env` (by default [[claude]]'s).
    */
  def run(
      role: String,
      payload: String = "",
      env: Map[String, String] = claude,
      in: Path = proj,
      args: Seq[String] = Seq.empty
  ): Bash.Done =
    Bash.run(
      in,
      machine.home,
      machine.bin,
      Seq("-c", (Argv.quote(hook.toString) +: role +: args.map(Argv.quote)).mkString(" ")),
      payload,
      env
    )

  /** As [[run]], and how long it took, in milliseconds. */
  def timed(
      role: String,
      payload: String = "",
      env: Map[String, String] = claude
  ): (Bash.Done, Long) = {
    val began = System.nanoTime()
    val done = run(role, payload, env)
    (done, (System.nanoTime() - began) / 1000000)
  }

  /** The recorded payload `name`, its checkout `/work/proj` written as `proj`. */
  def payload(name: String): String =
    HookFixtures.resource(name).replace("/work/proj", proj.toString)

  /** `proj` as a directory. */
  def dir: Directory = got(Directory.of(proj.toString))

  /** The session `local` of Claude Code. */
  def session(local: String): SessionId = got(Holders.session(HookFixtures.Claude, local))

  /** The holder a session in `proj` works as, or an agent of `kind` that `local` starts. */
  def holderOf(agent: Option[(String, String)] = None): ClaimHolder =
    Holders.of(Worker, dir, agent.map((kind, local) => Holders.Agent(kind, session(local))))

  /** Every batch the edge recorded, first first. */
  def recorded: Vector[EdgeSessions.Batch] =
    script.called.collect { case ScriptedSessions.Call.Recorded(_, _, batches) => batches }.flatten

  /** Every brief the edge read, first first. */
  def briefed: Vector[ScriptedSessions.Call.Briefed] =
    script.called.collect { case b: ScriptedSessions.Call.Briefed => b }

  /** A GET of `target` from the edge, as the edge answers it: its status line and body. */
  def get(target: String): (String, String) = EdgeFixtures.get(port, s"localhost:$port", target)

  /** A POST of `body` to `target` at the edge. */
  def post(target: String, body: String): (String, String) =
    EdgeFixtures.post(port, s"localhost:$port", target, body)
}

object HookFixtures {

  val Claude: Harness = got(Harness.of("claude-code"))

  /** The test resource `name`, under `payloads/`. */
  def resource(name: String): String =
    new String(getClass.getResourceAsStream(s"/payloads/$name").readAllBytes(), UTF_8)

  /** `body` over the edge, opened at this moment on a free port of this machine, and a checkout
    * `proj` joined to it.
    */
  def apply[A](body: HookFixtures => A): A = JoinFixtures { machine =>
    val script = new ScriptedSessions.Script
    served(script, new SetClock(Instant.now())) { port =>
      val proj = machine.checkout("proj")
      val joined = machine.join(proj, Seq(s"http://127.0.0.1:$port", "--mcp-name", machine.Name))
      if (joined.exit != 0) throw new java.lang.AssertionError(s"the join failed: ${joined.err}")
      body(new HookFixtures(machine, port, script, proj))
    }
  }

  /** `body` given the port of the edge serving [[ClaudeCode.adapter]] on this machine, as
    * `clock` tells the time, over the sessions `script` holds for the routes' owner; closed after.
    */
  def served[A](script: ScriptedSessions.Script, clock: Clock^)(body: Int => A): A = {
    val realm = got(Realm.of("slack", "T1"))
    val log = new ConcurrentLinkedQueue[String]()
    val open = got(
      CommandsEdge.of(
        EdgeName("commands"),
        got(Listen.parse("127.0.0.1:0")),
        Hosts.loopback,
        EdgeContracts.Room,
        realm,
        AttesterName("desk"),
        Owners,
        Server,
        Vector.empty,
        None,
        Vector(ClaudeCode.adapter)
      )
    ).open(
      EdgeFixtures.stores(
        scripted(Vector.empty)(Unrun),
        new InMemoryVoucher(Set(realm), Set.empty, Visibility.Shipped),
        new ScriptedSessions(Set(Owner.account), script)
      ),
      Map.empty,
      clock,
      l => { val _ = log.add(l) }
    ) match {
      case Right(o) => o
      case Left(r) => throw new java.lang.AssertionError(r.toString)
    }
    try {
      val port = log.asScala
        .collectFirst {
          case l if l.startsWith("commands: serving on http://127.0.0.1:") =>
            l.stripPrefix("commands: serving on http://127.0.0.1:").toInt
        }
        .getOrElse(throw new java.lang.AssertionError(s"no address logged: $log"))
      body(port)
    } finally open.close()
  }

  /** A socket listening on a free port of 127.0.0.1, holding `backlog` connections. */
  private def loopback(backlog: Int): ServerSocket = {
    val socket = new ServerSocket()
    socket.bind(new InetSocketAddress("127.0.0.1", 0), backlog)
    socket
  }

  /** A port of this machine nothing listens on. */
  def closedPort: Int = {
    val socket = loopback(1)
    try socket.getLocalPort
    finally socket.close()
  }

  /** `body` given the port of a listener that accepts every connection and never answers. */
  def silent[A](body: Int => A): A = holding((port, _) => body(port))

  /** As [[silent]], `body` also given how many connections the listener has accepted so far. */
  def holding[A](body: (Int, () => Int) => A): A = {
    val server = loopback(50)
    val held = new ConcurrentLinkedQueue[Socket]()
    val accepting = new Thread(() =>
      try while (true) { val _ = held.add(server.accept()) }
      catch { case _: IOException => () } // the listener closed
    )
    accepting.setDaemon(true)
    accepting.start()
    try body(server.getLocalPort, () => held.size)
    finally {
      server.close()
      held.forEach(_.close())
    }
  }
}
