package grit.app.main

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.edge.{Desk, OutcomeJson, Route, ToolRequest}
import grit.core.id.{PrincipalId, SourceId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Reaches}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload, Tx}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.edge.{Server, Tools}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.models.StubProvider
import grit.slack.edge.SlackEdge
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS: a deployment whose tasks reach Slack's posting service, launched
  * as the kit launches it, offers a task's turn the tool an edge serves there, and its call is
  * answered by that edge, which rings the turn; the turn reads the answer from the request.
  */
object ReachLiveTests extends TestSuite {

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** Every task reaching [[SlackEdge.PostsAt]], and working nowhere. */
  private val deployment: Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        topics = Topics.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        reaches = Vector(Reaches(Place.under(Namespace.Task, Vector.empty), SlackEdge.PostsAt))
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(c: DbConfig): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        deployment
      )
      .fold(r => sys.error(r.message), identity)

  /** One free tool, `note_here`, answering every request "noted at {its place}". */
  private object Noting extends Tools {
    val offered: ToolSet = ToolSet
      .of(
        Vector(
          ToolSet.Entry(
            ToolName("note_here"),
            "Note something.",
            ujson.Obj("type" -> "object"),
            asks = false,
            Retry.Interrupt
          )
        )
      )
      .fold(d => sys.error(d.toString), identity)

    def run(route: Route, request: ToolRequest): Outcome =
      Outcome.Done(s"noted at ${request.workspace.written}")
  }

  /** A task's turn in a fresh database `name`, its call answered by an edge serving
    * [[Noting]] at [[SlackEdge.PostsAt]], awaited; `check` then reads the engine and the turn.
    */
  private def reached[A](name: String)(check: (Engine^, DbConfig, TurnRef) => A): A = {
    val config = TestPostgres.freshDatabase(name)
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      Launch(
        engine,
        deployment,
        secrets(config),
        Launch.Run.Served,
        sweeping = false,
        _ => ()
      )
      val place = SlackEdge.PostsAt.place
      val desk = engine.register(PrincipalId.Grit, Set(place)) match {
        case Right(d) => d
        case Left(e) => sys.error(e.why)
      }
      desk.advertise(place, Noting.offered, Vector.empty).fold(e => sys.error(e.why), identity)
      val server = new Server(
        desk,
        Noting,
        run => { val _ = Thread.ofVirtual().start(() => run()) },
        _ => ()
      )
      server.serve()
      try {
        // The stub calls the first tool offered: with no workspace, the reached one.
        val turn = (for {
          turn <- engine.inbox.ingest(
            Origin.Task("reach", name),
            SourceId(s"$name-1"),
            Message.User(s"note it ${StubProvider.CallMarker}{}"),
            Account.Local
          )
          _ <- engine.inbox.startTurn(turn)
        } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
        val _ = engine.awaitTurn(turn)
        check(engine, config, turn)
      } finally server.close()
    } finally engine.close()
  }

  /** The first column of `sql`'s rows as text, its one parameter `param`. */
  private def column(config: DbConfig, sql: String, param: String): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, param)
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[String]
          while (rs.next()) out += rs.getString(1)
          out.result()
        }
      }
    }

  val tests = Tests {
    test(
      "an edge's answer is kept in its request and its result entry; DBOS holds only the ring"
    ) {
      reached("reach_rung") { (_, config, turn) =>
        val id = WorkflowId.value(turn.workflowId)
        val answer = "noted at service:slack"
        // The outcome, once on the request's row and once in the result entry it became.
        column(config, "SELECT outcome::text FROM grit.tool_requests WHERE workflow_id = ?", id)
          .map(ujson.read(_)) ==> Vector(OutcomeJson.write(Outcome.Done(answer)))
        column(
          config,
          "SELECT payload #>> '{message,content}' FROM grit.entries WHERE payload ->> 'shown' IS NOT NULL AND id LIKE '%' || ? || '%'",
          id
        ) ==> Vector(answer)
        // DBOS keeps a String as a JSON string: the ring, in the message and in the turn's
        // journal of its wait. No step holds the answer; the summary is left out, since the
        // stub writes it by quoting the turn.
        val ring = ujson.write(ujson.Str(Desk.Doorbell))
        column(config, "SELECT message FROM dbos.notifications WHERE destination_uuid = ?", id) ==>
          Vector(ring)
        column(
          config,
          "SELECT output FROM dbos.operation_outputs WHERE workflow_uuid = ? AND function_name = 'DBOS.recv'",
          id
        ) ==> Vector(ring)
        column(
          config,
          s"SELECT function_name FROM dbos.operation_outputs WHERE workflow_uuid = ? AND output LIKE '%$answer%'",
          id
        ).filterNot(_ == "summarise") ==> Vector()
      }
    }

    test(
      "a task's turn, its deployment reaching service:slack, calls the tool served there, and keeps the answer"
    ) {
      reached("reach") { (engine, _, turn) =>
        engine.db
          .read(Subject.Public)(engine.entries.list(turn.conversationId))
          .fold(e => sys.error(e.toString), identity)
          .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
          .collect { case Payload.Result(r, _) => r } ==> Vector(
          Message.ToolResult(StubProvider.CallId, "noted at service:slack", isError = false)
        )
      }
    }
  }
}
