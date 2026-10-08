package grit.app.main

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.{Attesting, EdgeStores, Variable}
import grit.core.id.SourceId
import grit.core.identity.Account
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Service, WorksIn}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload}
import grit.core.visibility.Subject
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.mcp.client.{FakeMcpServer, McpServer}
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope
import grit.models.StubProvider
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS, with an MCP server in this JVM: a turn launched as the kit
  * launches a deployment calls a tool of the service its conversation works in, which the MCP
  * edge serves through a real desk.
  */
object McpEdgeLiveTests extends TestSuite {

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(sys.error("service"))

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** Offering what `read` offers, every task's conversation working in GitHub's service. */
  private val deployment: Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector(WorksIn(Place.under(Namespace.Task, Vector.empty), Github)),
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        topics = Topics.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit
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

  val tests = Tests {
    test(
      "a task's turn calls the GitHub tool its edge advertises, the server answers, and the turn keeps the answer"
    ) {
      val config = TestPostgres.freshDatabase("mcp_edge")
      val fake = FakeMcpServer.start()
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", FakeMcpServer.githubFileContents)
        Launch(
          engine,
          deployment,
          secrets(config),
          Launch.Run.Served,
          sweeping = false,
          _ => ()
        )
        val server =
          McpServer.of("github", fake.endpoint, Token, McpScope.Open).fold(sys.error(_), identity)
        val edge = McpEdge.serving(Github, Vector(server)).fold(sys.error(_), identity)
        val stores =
          EdgeStores(
            engine.inbox,
            engine.principals,
            engine.deliveries,
            engine.acknowledgements,
            engine.reviews,
            engine.jot,
            engine,
            new Attesting(engine.voucher(Set.empty, Set.empty), engine.jot, _ => ())
          )
        edge.open(stores, Map("FAKE_MCP_TOKEN" -> fake.token), Clock.system(), _ => ()) match {
          case Left(refused) => sys.error(refused.message)
          case Right(open) =>
            try {
              // The stub calls the first tool offered, with the JSON after the marker.
              val read = ujson.Obj("owner" -> "o", "repo" -> "r", "path" -> "README.md")
              val turn = (for {
                turn <- engine.inbox.ingest(
                  Origin.Task("mcp", "live"),
                  SourceId("mcp-1"),
                  Message.User(
                    s"what is in the README? ${StubProvider.CallMarker}${read.render()}"
                  ),
                  Account.Local
                )
                _ <- engine.inbox.startTurn(turn)
              } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
              val _ = engine.awaitTurn(turn)
              val results = engine.db
                .read(Subject.Public)(engine.entries.list(turn.conversationId))
                .fold(e => sys.error(e.toString), identity)
                .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
                .collect { case Payload.Result(r, _) => r }
              val calls = fake.received
                .filter(_.method.contains("tools/call"))
                .map(r => ujson.read(r.body)("params"))
                .map(p => (p("name").str, p("arguments")))
              (results, calls) ==> (
                Vector(
                  Message.ToolResult(
                    StubProvider.CallId,
                    "successfully downloaded text file (SHA: 5091ae127e2f9c44169ff054c4426ad726309090)\n" +
                      "# Hello-World\n\nA short neutral README, standing in for the file GitHub answered with.\n",
                    isError = false
                  )
                ),
                Vector(("get_file_contents", read))
              )
            } finally open.close()
        }
      } finally {
        engine.close()
        fake.stop()
      }
    }
  }
}
