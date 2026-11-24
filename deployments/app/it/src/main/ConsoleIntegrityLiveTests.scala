package grit.app.main

import grit.core.edge.Variable
import grit.core.id.ToolCallId
import grit.core.message.Message
import grit.core.place.{Namespace, Place, Reaches, Service}
import grit.core.store.Payload
import grit.core.visibility.{
  Cap,
  Cleared,
  Compartments,
  Integrity,
  Label,
  Labelled,
  Level,
  Party,
  Room,
  RoomLabels,
  Subject,
  Visibility
}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.kit.deployment.Offered
import grit.mcp.client.{Credential, FakeTokenServer}
import grit.mcp.searchconsole.{ConsoleEdge, ConsoleRest, FakeConsole}
import grit.models.StubModels
import grit.slack.edge.SlackEdge
import grit.turn.Turn

import utest.*

/** Against Postgres and DBOS: a task's turn reads a property's search traffic through
  * [[ConsoleEdge]], over a [[FakeConsole]], under the reference deployment's declarations for
  * [[ServedEdges.ConsoleSiteVar]] ([[Deploying.deployment]]), and is then asked to post to Slack,
  * its model obeying [[IntegrityTurn.Obeying]] and reading whatever is hidden from it. A row
  * is believed at the service's cap, whatever it is grouped by: under the member cap an answer
  * by query, as one by date, is shown, and the post after it goes through.
  */
object ConsoleIntegrityLiveTests extends TestSuite {
  import IntegrityTurn.*

  private val tokens = FakeTokenServer.start()

  override def utestAfterAll(): Unit = tokens.stop()

  private val Site = "sc-domain:example.com"

  private val Key = Variable("FAKE_SC_KEY")

  private def right[A](e: Either[String, A]): A = e.fold(sys.error(_), identity)

  private val Console: Service = right(ServedEdges.ConsoleAt)

  /** The reference deployment's visibility with [[ServedEdges.ConsoleSiteVar]] set. */
  private def deployed: Visibility =
    right(
      Deploying
        .deployment(
          Map(ServedEdges.ConsoleSiteVar -> Site),
          Offered.Read,
          Vector.empty,
          Vector.empty,
          java.time.ZoneOffset.UTC
        )
        .map(_.visibility)
    )

  private val internal = Label.at(Level.Internal)

  private val Tasks = Place.under(Namespace.Task, Vector.empty)

  /** Search Console declared as [[deployed]] declares it (the test pins that they agree), in a
    * deployment whose tasks are internal, as a channel a person labelled is, with the stub
    * model cleared for them: under the reference deployment a task is public, and reads
    * nothing at an internal service.
    */
  private def declared: Visibility =
    right(
      for {
        rooms <- RoomLabels
          .of(
            Vector(
              Console.place -> internal,
              Tasks -> internal,
              Place.under(Namespace.Slack, Vector("T0NEUTRAL")) -> internal
            ),
            Labelled.Mapped(Label.Public)
          )
          .left
          .map(_.written)
        stub <- Party.of(StubModels.Ref).toRight("the stub's party")
        v <- Visibility
          .of(
            Compartments.Shipped,
            rooms,
            Vector.empty,
            Vector.empty,
            clearances = Vector(
              Cleared(stub, internal),
              Cleared(Party.Service(Console), Compartments.Shipped.top)
            ),
            caps = Vector(Cap(Console.place, Integrity.Member)),
            invited = Vector(Console.place)
          )
          .left
          .map(_.toString)
      } yield v
    )

  /** What `v` declares of Search Console: its cap, its clearance, and whether a room in it is
    * invited.
    */
  private def ofConsole(v: Visibility) =
    (
      v.outputOf(Console),
      v.clearances.of(Party.Service(Console)),
      v.invites(Place.under(Namespace.Service, Vector("search_console", "property"))),
      v.rooms.label(Room(Console.place, None)).label
    )

  /** What a turn reading `args` came to: whether the answer it read holds `held` (a row of the
    * fixture's, not a refusal), whether each request offered `slack_post`, the
    * channels posted, what its `slack_post` call was answered, and each item `gate:1` decided
    * on, as the integrity it recorded and the item's fate.
    */
  private def ran(
      name: String,
      args: ujson.Obj,
      held: String
  ): (Boolean, Vector[Boolean], Vector[String], Option[String], Vector[(String, String)]) = {
    val config = TestPostgres.freshDatabase(s"console_integrity_$name")
    val engine = LiveEngine.open(config, Turn.Epoch, visibility = declared)
    val fake = FakeConsole.start()
    try {
      fake.sharing()
      val obeying = new Obeying(Reads("search_console_query", args, "Traffic is up."), Mode.Reads)
      val posts = new Posts
      launch(
        engine,
        obeying,
        new Answering(Asked.Honest),
        Vector(
          Reaches(Tasks, Console),
          Reaches(Tasks, SlackEdge.PostsAt)
        )
      )
      val edge = right(
        ConsoleEdge.serving(
          Credential.serviceAccount(Key, ConsoleRest.ReadOnly),
          Console,
          FakeConsole.site(Site),
          api = fake.api.toString
        )
      )
      // Today is the fixtures' last day, so the by-date range below stays one Dates takes.
      val turn = servingEdge(
        engine,
        edge,
        Map(Variable.value(Key) -> tokens.keyFile()),
        posts,
        FakeConsole.clock()
      ) { () =>
        val turn = IntegrityTurn.ask(engine, name, Request)
        val _ = engine.awaitTurn(turn)
        turn
      }
      val results = engine.db
        .read(Subject.Turn(turn))(engine.entries.list(turn.conversationId))
        .fold(e => sys.error(e.toString), identity)
        .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
        .collect { case Payload.Result(Message.ToolResult(call, content, _), _) =>
          ToolCallId.value(call) -> content
        }
        .toMap
      // The first gate after the read: each item it decided on, as its recorded integrity and
      // fate.
      val gated = LiveEngine
        .steps(config, turn.workflowId)
        .fold(e => sys.error(e.toString), identity)
        .find(_.name == "gate:1")
        .flatMap(_.output)
        .toVector
        .flatMap(out => ujson.read(out)("ok")("decided").arr)
        .map(d => (d("item")("integrity").str, d("fate")("kind").str))
        .distinct
      (
        results.get("g1").exists(_.contains(held)),
        obeying.requests.map(_.tools.exists(_.name == "slack_post")),
        posts.ran.flatMap(_.destination).map(_.written),
        results.get("p2"),
        gated
      )
    } finally {
      fake.stop()
      engine.close()
    }
  }

  val tests = Tests {
    test(
      "under the service's member cap, an answer by query, as one by date, reaches the turn at member, shown, and the post after it goes through"
    ) {
      (
        ofConsole(declared) == ofConsole(deployed),
        ran("by_query", ujson.Obj("by" -> "query"), "trail running shoes"),
        ran(
          "by_date",
          ujson.Obj("by" -> "date", "start" -> "2026-11-22", "end" -> "2026-11-24"),
          "2026-11-23"
        )
      ) ==> (
        true,
        (
          true,
          Vector(true, true, true, true),
          Vector("slack:T0NEUTRAL/C0GENERAL"),
          Some("Posted in slack:T0NEUTRAL/C0GENERAL."),
          Vector("member" -> "lowered")
        ),
        (
          true,
          Vector(true, true, true, true),
          Vector("slack:T0NEUTRAL/C0GENERAL"),
          Some("Posted in slack:T0NEUTRAL/C0GENERAL."),
          Vector("member" -> "lowered")
        )
      )
    }
  }
}
