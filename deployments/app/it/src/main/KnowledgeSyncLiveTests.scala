package grit.app.main

import java.time.Instant
import java.time.temporal.ChronoUnit

import scala.concurrent.duration.*
import scala.util.Using

import grit.assembly.retrieval.RetrievalAssembler
import grit.core.clock.{Clock, SetClock}
import grit.core.edge.{Attesting, EdgeStores, Variable}
import grit.core.files.NoIntake
import grit.core.files.PagedText
import grit.core.id.{CorpusName, PluginName, ScheduleKey, SourceId, TurnRef}
import grit.core.identity.Account
import grit.core.inbox.Slotted
import grit.core.job.{Grace, SlotRule}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Service, Weight, WorksIn}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload, Tx}
import grit.core.triage.Corpus
import grit.core.visibility.{
  Cap,
  Cleared,
  Compartments,
  Integrity,
  Label,
  Labelled,
  Level,
  Party,
  RoomLabels,
  Subject,
  Visibility
}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.job.clock.ClockEdge
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.knowledge.github.GitHubCards
import grit.knowledge.plugin.{CardSource, Knowledge}
import grit.knowledge.sync.SyncSchedule
import grit.mcp.client.{Credential, FakeMcpServer, McpServer}
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope
import grit.models.{StubModels, StubProvider}
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS, with the stub model and an MCP server named `github` in this JVM:
  * the knowledge plugin, launched as the kit launches a deployment, syncs a repository of four
  * files and a directory into cards through the MCP edge, its once slots started by the clock
  * edge; and turns in rooms of two labels draw on them, or do not.
  *
  * The repository, each file's text holding the summary the stub replies with:
  * {{{
  *   /       README.md, guide.md, docs/
  *   docs    a.md, b.md
  * }}}
  * `service:github` is labelled internal (every room is, but `task:public`) and trusted
  * internal, unless a case says otherwise.
  */
object KnowledgeSyncLiveTests extends TestSuite {

  private val Token = Variable("FAKE_MCP_TOKEN")

  private def right[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private val github: Service = right(Service.of("github"))
  private val internal = Label.at(Level.Internal)
  private val confidential = Label.at(Level.Confidential)

  /** The place the cards are kept at. */
  private val at: Place = github.place

  /** The room labelled public; every other is internal. */
  private val publicRoom = "public"

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** Every room internal but `task:public`, GitHub's service trusted with `trusted` and its
    * results believed at `Member`, so its cards are kept as they are read.
    */
  private def visibility(trusted: Label): Visibility =
    Visibility
      .of(
        Compartments.Shipped,
        RoomLabels
          .of(
            Vector(Place.under(Namespace.Task, Vector(publicRoom)) -> Label.Public),
            Labelled.Mapped(internal)
          )
          .fold(p => sys.error(p.written), identity),
        Vector.empty,
        Vector.empty,
        Vector(
          Cleared(Party.Service(github), trusted),
          // The turn's calls go to the stub, cleared as a deployment clears its pairs.
          Cleared(Party.of(StubModels.Ref).getOrElse(sys.error("the stub's party")), trusted)
        ),
        caps = Vector(Cap(github.place, Integrity.Member))
      )
      .fold(r => sys.error(r.toString), identity)

  private def key(k: String): ScheduleKey = right(ScheduleKey.of(k))

  /** The repository as a source at `label`, synced once at `first` and once at `second`. */
  private def source(label: Option[Label], first: Instant, second: Instant): GitHubCards = {
    val grace = Grace.of(5.minutes).getOrElse(sys.error("grace"))
    right(
      GitHubCards.of(
        Corpus(right(CorpusName.of("repo")), "the repository", Place.Everywhere, Some(github)),
        "github",
        "octocat",
        "Hello-World",
        "main",
        Vector(""),
        Vector.empty,
        Set.empty,
        "files",
        100,
        label,
        at,
        Vector(
          right(SyncSchedule.of(key("first"), SlotRule.Once(first, grace), 10)),
          right(SyncSchedule.of(key("second"), SlotRule.Once(second, grace), 10))
        )
      )
    )
  }

  /** The deployment of the knowledge plugin over `cards` under `visible`: every task's
    * conversation working in GitHub's service, its window drawing on its own room and `at`.
    */
  private def deployment(cards: GitHubCards, visible: Visibility): Deployment = {
    val name = right(PluginName.of("knowledge"))
    val knowledge = right(Knowledge.of(name, Vector(CardSource.GitHub(cards)), visible))
    val d = LifecycleSettings.Default
    val lifecycle = right(
      LifecycleSettings.of(
        d.windows,
        d.balance,
        d.settle,
        d.resolveAt,
        d.asks,
        Weight.Default
      )
    )
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector(WorksIn(Place.under(Namespace.Task, Vector.empty), github)),
        plugins = Vector(knowledge),
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Retrieval(Tokens(4000), RetrievalAssembler.DefaultTail),
        placer = Placer.Off("no classifier here"),
        lifecycle = lifecycle,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        visibility = visible,
        formats = PagedText.formats
      )
      .fold(r => sys.error(r.message), identity)
  }

  private def secrets(c: DbConfig, d: Deployment): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        d
      )
      .fold(r => sys.error(r.message), identity)

  private def sha(n: Int): String = f"$n%040x"

  /** A file's summary, as the stub replies to the ask holding it. */
  private def summarised(summary: String, terms: String*): String =
    StubProvider.CallMarker + ujson.write(
      ujson.Obj("summary" -> summary, "terms" -> ujson.Arr(terms.map(ujson.Str(_))*))
    )

  private val files: Map[String, String] = Map(
    "README.md" -> summarised("What the project is and how to start it.", "getting started"),
    "guide.md" -> summarised("A contributor's guide: branches, reviews and releases.", "releases"),
    "docs/a.md" -> summarised(
      "The widget deployment runbook: rolling restarts and rollbacks.",
      "widget rollback"
    ),
    "docs/b.md" -> summarised("How the billing exports are scheduled.", "billing")
  )

  /** A directory entry in the recorded listing's shape. */
  private def entry(kind: String, path: String, version: String): ujson.Obj =
    ujson.Obj("type" -> kind, "name" -> path.split('/').last, "path" -> path, "sha" -> version)

  private def text(t: String): ujson.Obj =
    ujson.Obj("content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> t)))

  /** `get_file_contents` over the repository, README.md at the blob SHA `readme`: a listing
    * of `/` or `docs`, or a file's text, in the recorded answers' shapes.
    */
  private def repository(readme: Int)(arguments: ujson.Value): ujson.Obj = {
    val versions = Map("README.md" -> readme, "guide.md" -> 2, "docs/a.md" -> 3, "docs/b.md" -> 4)
    val path = arguments.objOpt.flatMap(_.get("path")).flatMap(_.strOpt).getOrElse("")
    path.stripPrefix("/").stripSuffix("/") match {
      case "" =>
        text(
          ujson.write(
            ujson.Arr(
              entry("file", "README.md", sha(readme)),
              entry("file", "guide.md", sha(2)),
              entry("dir", "docs", sha(10))
            )
          )
        )
      case "docs" =>
        text(
          ujson.write(
            ujson.Arr(entry("file", "docs/a.md", sha(3)), entry("file", "docs/b.md", sha(4)))
          )
        )
      case file =>
        ujson.Obj(
          "content" -> ujson.Arr(
            ujson.Obj(
              "type" -> "text",
              "text" ->
                s"successfully downloaded text file (SHA: ${sha(versions.getOrElse(file, 0))})"
            ),
            ujson.Obj(
              "type" -> "resource",
              "resource" -> ujson.Obj(
                "uri" -> s"repo://octocat/Hello-World/refs/heads/main/contents/$file",
                "mimeType" -> "text/plain; charset=utf-8",
                "text" -> files.getOrElse(file, "")
              )
            )
          )
        )
    }
  }

  /** The rows of `sql`, each column as text. */
  private def rows(config: DbConfig, sql: String): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val n = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to n).toVector.map(i => rs.getString(i))
          out.result()
        }
      }
    }

  /** Each current card: its path, label level (0 public … 3 restricted), and its pointer's
    * tool; by path.
    */
  private def cards(config: DbConfig): Vector[Vector[String]] =
    rows(
      config,
      """SELECT d.pointer->'arguments'->>'path', l.level::text, d.pointer->>'tool'
        |FROM grit.documents d JOIN grit.classifications l ON l.id = d.classification_id
        |WHERE d.plugin = 'knowledge' AND d.superseded_at IS NULL AND d.body IS NOT NULL
        |ORDER BY d.pointer->'arguments'->>'path' COLLATE "C"""".stripMargin
    )

  /** How many versions of cards, withdrawals included, were ever written. */
  private def versions(config: DbConfig): Int =
    rows(config, "SELECT count(*) FROM grit.documents WHERE plugin = 'knowledge'")
      .flatMap(_.headOption)
      .headOption
      .fold(0)(_.toInt)

  /** Each current card's path, how many windows held it, and how many turns followed it. */
  private def used(config: DbConfig): Vector[Vector[String]] =
    rows(
      config,
      """SELECT pointer->'arguments'->>'path', placed::text, followed::text FROM grit.documents
        |WHERE plugin = 'knowledge' AND superseded_at IS NULL AND body IS NOT NULL
        |ORDER BY pointer->'arguments'->>'path' COLLATE "C"""".stripMargin
    )

  /** The run the clock edge starts on `engine` for `d`'s slot due at `due`, and its reply. */
  private def synced(engine: Engine^, d: Deployment, due: Instant): String = {
    val ticked =
      new ClockEdge(engine.inbox, engine.schedules, engine.db, new SetClock(due), d.allJobs)
        .tick()
        .fold(e => sys.error(e.toString), identity)
    val turn = ticked.slotted
      .collectFirst { case (_, Slotted.Started(turn, _)) => turn }
      .getOrElse(sys.error(s"not started: $ticked"))
    val _ = engine.awaitTurn(turn)
    replied(engine, turn)
  }

  /** `turn`'s reply's text; "no reply" when it has none. */
  private def replied(engine: Engine^, turn: TurnRef): String =
    engine.db
      .read(Subject.Conversation(turn.conversationId))(engine.entries.get(turn.replyId))
      .fold(e => sys.error(e.toString), identity)
      .map(_.payload) match {
      case Some(Payload.Message(Message.Assistant(blocks, _, _, _, _))) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      case other => s"no reply: $other"
    }

  /** A turn in the room `room` asking `text`, run to its end. */
  private def asked(engine: Engine^, room: String, id: String, text: String): TurnRef = {
    val turn = (for {
      turn <- engine.inbox.ingest(
        Origin.Task(room, id),
        SourceId(id),
        Message.User(text),
        Account.Local,
        None
      )
      _ <- engine.inbox.startTurn(turn)
    } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
    val _ = engine.awaitTurn(turn)
    turn
  }

  /** The turn's question about the runbook, calling its card's pointer. */
  private val runbook =
    "How do I roll back a widget deployment with the runbook? " + StubProvider.calling(
      "github_get_file_contents"
    ) +
      ujson.write(ujson.Obj("owner" -> "octocat", "path" -> "docs/a.md", "repo" -> "Hello-World"))

  /** `body` over a fresh database `name` whose engine runs `cards`' deployment under `visible`,
    * the MCP edge serving the repository with README.md at `readme`; `first` and `second` are
    * its slots.
    */
  private def world[A](
      name: String,
      label: Option[Label],
      visible: Visibility
  )(body: (DbConfig, Engine^, Deployment, Instant, Instant, FakeMcpServer) => A): A = {
    val first = Instant.now().truncatedTo(ChronoUnit.SECONDS)
    val second = first.plusSeconds(60)
    val d = deployment(source(label, first, second), visible)
    val config = TestPostgres.freshDatabase(name)
    val fake = FakeMcpServer.start()
    val engine = LiveEngine.open(config, Turn.Epoch, visibility = visible)
    try {
      fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
      fake.answering("get_file_contents")(repository(1))
      Launch(engine, d, secrets(config, d), Launch.Run.Served, sweeping = false, _ => ())
      val server =
        McpServer
          .of("github", fake.endpoint, Credential.static(Token), McpScope.Open)
          .fold(sys.error(_), identity)
      val edge = McpEdge.serving(github, Vector(server)).fold(sys.error(_), identity)
      val stores =
        EdgeStores(
          engine.inbox,
          engine.administration,
          engine.joins,
          engine.principals,
          engine.deliveries,
          engine.spending,
          engine.acknowledgements,
          engine.reviews,
          engine.origins,
          engine.documentReviewsOf,
          engine.jot,
          engine,
          new Attesting(engine.voucher(Set.empty, Set.empty), engine.jot, _ => ()),
          new NoIntake
        )
      edge.open(stores, Map("FAKE_MCP_TOKEN" -> fake.token), Clock.system(), _ => ()) match {
        case Left(refused) => sys.error(refused.message)
        case Right(open) =>
          try body(config, engine, d, first, second, fake)
          finally open.close()
      }
    } finally {
      engine.close()
      fake.stop()
    }
  }

  private val tool = "github_get_file_contents"

  /** Every card at level `level`, each pointing at the read of its unit. */
  private def everyCard(level: String): Vector[Vector[String]] =
    Vector("README.md", "docs", "docs/a.md", "docs/b.md", "guide.md").map(Vector(_, level, tool))

  val tests = Tests {
    test("after a once slot, the cards are current at the source's label, each with its pointer") {
      world("knowledge_sync", None, visibility(internal)) { (config, engine, d, first, _, _) =>
        val said = synced(engine, d, first)
        (said, cards(config)) ==> (
          "repo: 5 written, 0 relabelled, 0 withdrawn, 0 unchanged, 0 left for the next run",
          everyCard("1")
        )
      }
    }

    test("a second slot after one file's SHA changes writes exactly one version, its own") {
      world("knowledge_resync", None, visibility(internal)) {
        (config, engine, d, first, second, fake) =>
          val _ = synced(engine, d, first)
          val before = versions(config)
          fake.answering("get_file_contents")(repository(5))
          val said = synced(engine, d, second)
          (said, versions(config) - before, cards(config)) ==> (
            "repo: 1 written, 0 relabelled, 0 withdrawn, 4 unchanged, 0 left for the next run",
            1,
            everyCard("1")
          )
      }
    }

    test("a turn in an internal room that calls a card's pointer leaves that card followed once") {
      world("knowledge_follow", None, visibility(internal)) { (config, engine, d, first, _, _) =>
        val _ = synced(engine, d, first)
        val _ = asked(engine, "ops", "follow-1", runbook)
        val followed = used(config).map(r => (r(0), r(2)))
        followed ==> Vector(
          ("README.md", "0"),
          ("docs", "0"),
          ("docs/a.md", "1"),
          ("docs/b.md", "0"),
          ("guide.md", "0")
        )
      }
    }

    test("a turn in a public room holds no card, though it draws on the cards' place") {
      world("knowledge_public", None, visibility(internal)) { (config, engine, d, first, _, _) =>
        val _ = synced(engine, d, first)
        val _ = asked(engine, publicRoom, "public-1", runbook)
        val inPublic = used(config).map(r => (r(0), r(1), r(2)))
        val _ = asked(engine, "ops", "internal-1", runbook)
        // The same question in an internal room holds the runbook's card: the public room's
        // window held none for want of clearance, not of a match.
        val inInternal = used(config).filter(_(0) == "docs/a.md").map(_(1))
        (inPublic.map(_._2).distinct, inPublic.map(_._3).distinct, inInternal) ==> (
          Vector("0"),
          Vector("0"),
          Vector("1")
        )
      }
    }

    test(
      "a source labelled confidential over an internal service trusted with confidential keeps its cards at confidential, and an internal room's turn holds none"
    ) {
      world("knowledge_confidential", Some(confidential), visibility(confidential)) {
        (config, engine, d, first, _, _) =>
          val _ = synced(engine, d, first)
          val _ = asked(engine, "ops", "internal-2", runbook)
          (cards(config), used(config).map(_(1)).distinct) ==> (everyCard("2"), Vector("0"))
      }
    }
  }
}
