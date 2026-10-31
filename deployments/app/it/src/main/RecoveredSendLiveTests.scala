package grit.app.main

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.files.PagedText
import grit.core.id.{EdgeName, SourceId}
import grit.core.identity.Account
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Service, WorksIn}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload, StoreError}
import grit.core.visibility.Subject
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.Secrets
import grit.kit.run.Kit
import grit.mcp.client.{Credential, FakeMcpServer, McpServer}
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope
import grit.models.StubProvider
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS, with an MCP server in this JVM: a turn DBOS starts when the
  * deployment's workflows launch, as it starts one it recovers after a restart, calls a tool of
  * the service an edge of this process serves.
  */
object RecoveredSendLiveTests extends TestSuite {

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(sys.error("service"))

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** An edge that serves nothing and takes `pause` to open, as an MCP edge listing a slow
    * server does.
    */
  private final class Slow(pause: FiniteDuration) extends ServedEdge {
    def name: EdgeName = EdgeName("slow")
    def needs: Vector[Variable] = Vector.empty
    def asks: ServedEdge.Asks = ServedEdge.Asks.Never
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      Thread.sleep(pause.toMillis)
      Right(new ServedEdge.Open {
        def deliver(): Either[StoreError, Int] = Right(0)
        def close(): Unit = ()
      })
    }
  }

  /** Serving `edges`, in order, every task's conversation working in GitHub's service. */
  private def deployment(edges: Vector[ServedEdge]): Deployment =
    Deployment
      .of(
        edges = edges,
        worksIn = Vector(WorksIn(Place.under(Namespace.Task, Vector.empty), Github)),
        plugins = Vector.empty,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        placer = Placer.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        formats = PagedText.formats
      )
      .fold(r => sys.error(r.message), identity)

  val tests = Tests {
    test("a turn waiting when the deployment opens sends its service call to the edge it serves") {
      val config = TestPostgres.freshDatabase("recovered_send")
      val fake = FakeMcpServer.start()
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", FakeMcpServer.githubFileContents)
        val server =
          McpServer
            .of("github", fake.endpoint, Credential.static(Token), McpScope.Open)
            .fold(sys.error(_), identity)
        val mcp = McpEdge.serving(Github, Vector(server)).fold(sys.error(_), identity)
        // Relies on `edges`' order: the edges open one after another, so the MCP edge registers
        // only after the slow one's pause.
        val d = deployment(Vector(new Slow(3.seconds), mcp))
        val env = Map(
          DbConfig.UrlVar -> config.jdbcUrl,
          DbConfig.UserVar -> config.user,
          DbConfig.PasswordVar -> config.password,
          "FAKE_MCP_TOKEN" -> fake.token
        )
        val secrets = Secrets.of(env, d).fold(r => sys.error(r.message), identity)
        // Enqueued before anything is launched: DBOS starts it at launch, as it starts a turn
        // it recovers.
        val read = ujson.Obj("owner" -> "o", "repo" -> "r", "path" -> "README.md")
        val turn = (for {
          turn <- engine.inbox.ingest(
            Origin.Task("mcp", "recovered"),
            SourceId("recovered-1"),
            Message.User(
              s"what is in the README? ${StubProvider.calling("github_get_file_contents")}${read.render()}"
            ),
            Account.Local,
            None
          )
          _ <- engine.inbox.startTurn(turn)
        } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
        val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
        Kit.opened(engine, d, secrets, env, Clock.system(), _ => (), log) match {
          case Left(failure) => sys.error(failure.toString)
          case Right(opened) =>
            try {
              val _ = engine.awaitTurn(turn)
              val results = engine.db
                .read(Subject.Public)(engine.entries.list(turn.conversationId))
                .fold(e => sys.error(e.toString), identity)
                .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
                .collect { case Payload.Result(r, _) => r.content }
              results ==> Vector(
                "successfully downloaded text file (SHA: 5091ae127e2f9c44169ff054c4426ad726309090)\n" +
                  "# Hello-World\n\nA short neutral README, standing in for the file GitHub answered with.\n"
              )
            } finally opened.close()
        }
      } finally {
        engine.close()
        fake.stop()
      }
    }
  }
}
