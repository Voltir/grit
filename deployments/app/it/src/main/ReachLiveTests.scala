package grit.app.main

import scala.concurrent.duration.*

import grit.core.edge.{Route, ToolRequest}
import grit.core.id.{PrincipalId, SourceId}
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Reaches}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
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
  * answered by that edge.
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

  val tests = Tests {
    test(
      "a task's turn, its deployment reaching service:slack, calls the tool served there, and keeps the answer"
    ) {
      val config = TestPostgres.freshDatabase("reach")
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        Launch(engine, deployment, secrets(config), Launch.Run.Served, sweeping = false, _ => ())
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
              Origin.Task("reach", "live"),
              SourceId("reach-1"),
              Message.User(s"note it ${StubProvider.CallMarker}{}"),
              PrincipalId.Local
            )
            _ <- engine.inbox.startTurn(turn)
          } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
          val _ = engine.awaitTurn(turn)
          engine.db
            .read(engine.entries.list(turn.conversationId))
            .fold(e => sys.error(e.toString), identity)
            .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
            .collect { case Payload.Result(r, _) => r } ==> Vector(
            Message.ToolResult(StubProvider.CallId, "noted at service:slack", isError = false)
          )
        } finally server.close()
      } finally engine.close()
    }
  }
}
