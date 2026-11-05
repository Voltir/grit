package grit.databases.edge

import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.admin.InMemoryAdministration
import grit.core.clock.Clock
import grit.core.document.InMemoryDocuments
import grit.core.edge.{
  Attesting,
  EdgeRefusal,
  EdgeStores,
  Edges,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins,
  Permit,
  Registration,
  ServedEdge,
  ToolRequest,
  Variable
}
import grit.core.files.NoIntake
import grit.core.id.{CallSlot, ConversationId, EdgeId, EdgeName, PrincipalId, TurnRef, TurnSeq}
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.core.visibility.Subject
import grit.databases.connect.AppDatabase
import grit.databases.declare.{Database, Limits}
import grit.dbos.sql.TestTx
import grit.edge.PlaceFragments

import utest.*

/** [[DatabaseEdge]] opened over in-memory stores, and its tools run by [[DatabaseTools]], against
  * a live Postgres shaped like an application's.
  */
object DatabaseEdgeLiveTests extends TestSuite {

  private lazy val db = AppDatabase("edge_live")

  /** A reader of the leads and the applicants' unpersonal columns, which may also insert into
    * `scratch` (so that only the read-only transaction stops it).
    */
  private val Grants =
    Seq("SELECT ON app.leads", "SELECT (id, lead_id) ON app.applicants", "INSERT ON app.scratch")

  private lazy val reader = db.login("reader", Grants)

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private def stores(edges: InMemoryEdges): EdgeStores^ = {
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    EdgeStores(
      inbox,
      InMemoryAdministration.none(),
      InMemoryJoins.none(),
      inbox.principals,
      new InMemoryDeliveries,
      inbox.ledger,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      new InMemoryOrigins(inbox.conversations),
      (new InMemoryDocuments).reviews.of,
      FakeJot,
      edges,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ()),
      new NoIntake
    )
  }

  private def edge(database: Database): ServedEdge =
    DatabaseEdge.serving(database).fold(why => throw new IllegalStateException(why), identity)

  /** What opening the edge for `database` with `env` did: its result (the open edge, closed),
    * the edges it registered in, and its log.
    */
  private def opened(
      database: Database,
      env: Map[String, String]
  ): (Either[EdgeRefusal, Unit], InMemoryEdges, Vector[String]) = {
    val edges = new InMemoryEdges
    val log = new ConcurrentLinkedQueue[String]()
    val result =
      edge(database).open(
        stores(edges),
        env,
        Clock.system(),
        line => { val _ = log.add(line) }
      ) match {
        case Left(refusal) => Left(refusal)
        case Right(open) => Right(open.close())
      }
    (result, edges, log.asScala.toVector)
  }

  /** `tool` run with `arguments` by the tools of `role`'s database under `limits`, telling `said`
    * what it says, as the edge's server runs a request addressed to its place.
    */
  private def run(
      tool: String,
      arguments: ujson.Value,
      limits: Limits = Limits.Default,
      said: String => Unit = _ => ()
  ): Outcome = {
    val database = db.database(reader, limits)
    val described =
      DatabaseTools.described(database).fold(w => throw new IllegalStateException(w), identity)
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    val q = ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      database.service.place,
      PrincipalId.Grit,
      ToolName.of(tool).fold(w => throw new IllegalStateException(w), identity),
      Permit.Free,
      Retry.Rerun,
      arguments,
      Set.empty,
      None
    )
    val route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    val open = db.reader(reader, limits = limits)
    try new DatabaseTools(database, described, open).run(route, q, said)
    finally open.close()
  }

  private def text(outcome: Outcome): String = outcome match {
    case Outcome.Done(t, _, _, _, _) => t
    case other => throw new java.lang.AssertionError(other.toString)
  }

  val tests = Tests {
    test(
      "opened, it advertises its two tools at the service's place, its instructions the Place layer, logged"
    ) {
      val database = db.database(reader)
      val (result, edges, log) = opened(database, db.env(reader))
      val offered = DatabaseTools.described(database).map(_.offered)
      (
        result,
        edges.adverts.toVector.map((at, advert) =>
          (at._2, Some(advert.tools), advert.instructions)
        ),
        log
      ) ==> (
        Right(()),
        Vector(
          (
            database.service.place,
            offered.toOption.map(_.id),
            PlaceFragments.declared(database.service.place, "Leads.").toSeq.toVector.map(_.id)
          )
        ),
        Vector("service:leads: serving leads_tables, leads_query")
      )
    }

    test("it needs the URL's and the login's variables, and is never asked") {
      val e = edge(db.database(reader))
      (e.name, e.needs.map(Variable.value), e.asks) ==> (
        EdgeName("leads"),
        Vector("APP_DB_URL", "APP_DB_USER", "APP_DB_PASSWORD"),
        ServedEdge.Asks.Never
      )
    }

    test("open is refused when the URL is unset, as missing, and nothing registers") {
      val (result, edges, _) = opened(db.database(reader), db.env(reader) - "APP_DB_URL")
      (result, edges.adverts.size) ==> (Left(EdgeRefusal.Missing(AppDatabase.Url)), 0)
    }

    test("open is refused when the login lands as another role, naming no password") {
      val (result, _, _) = opened(db.database("someone_else"), db.env(reader))
      result match {
        case Left(EdgeRefusal.Refused(why)) =>
          assert(why.contains("not as the role it declares"), !why.contains(AppDatabase.Password))
        case other => throw new java.lang.AssertionError(other.toString)
      }
    }

    test("closed, its connection to the database ends") {
      def sessions = db.one(s"SELECT count(*) FROM pg_stat_activity WHERE usename = '$reader'")
      val (result, _, _) = opened(db.database(reader), db.env(reader))
      // A closed connection's backend exits just after: up to 2 s for it to.
      val gone = Iterator.fill(40)(sessions).map { n => if (n != "0") Thread.sleep(50); n }
      (result, gone.find(_ == "0")) ==> (Right(()), Some("0"))
    }

    test(
      "the tables are listed, one line each, its comment's first line, none the role may not read"
    ) {
      text(run("leads_tables", ujson.Obj())) ==>
        """The tables and views you may read in the leads database:
          |- app.applicants (table)
          |- app.leads (table): One row per lead the application took in.""".stripMargin
    }

    test("one table is its columns with their comments, none it may not read, and its references") {
      (
        text(run("leads_tables", ujson.Obj("table" -> "applicants"))),
        text(run("leads_tables", ujson.Obj("table" -> "app.leads")))
      ) ==> (
        """app.applicants (table)
          |
          || column | type | description |
          ||---|---|---|
          || id | integer |  |
          || lead_id | integer |  |
          |
          |References:
          |- (lead_id) references app.leads (id)""".stripMargin,
        """app.leads (table): One row per lead the application took in.
          |
          || column | type | description |
          ||---|---|---|
          || id | integer | Ours, in the offers URL. |
          || external_id | text | The one sent to the supplier. |""".stripMargin
      )
    }

    test("a query is answered as a table, NULL as NULL") {
      text(
        run(
          "leads_query",
          ujson.Obj("sql" -> "SELECT id, external_id FROM app.leads WHERE id IN (1, 3) ORDER BY id")
        )
      ) ==> "| id | external_id |\n|---|---|\n| 1 | x-1 |\n| 3 | NULL |"
    }

    test("a query past the row limit says it kept the first of more") {
      text(
        run(
          "leads_query",
          ujson.Obj("sql" -> "SELECT id FROM app.leads ORDER BY id"),
          limits = AppDatabase.limits(rows = 3)
        )
      ) ==> "| id |\n|---|\n| 1 |\n| 2 |\n| 3 |\n\n[The first 3 rows of more.]"
    }

    test("a query past the byte limit keeps the rows that fit and says so") {
      text(
        run(
          "leads_query",
          ujson.Obj("sql" -> "SELECT id FROM app.leads ORDER BY id"),
          limits = AppDatabase.limits(bytes = 20)
        )
      ) ==> "| id |\n|---|\n| 1 |\n\n[The first 1 rows: the rest would go over 20 bytes.]"
    }

    test("each refusal is its sentence; the operator alone hears the state") {
      val said = new ConcurrentLinkedQueue[String]()
      def failed(sql: String, limits: Limits = Limits.Default): String =
        run("leads_query", ujson.Obj("sql" -> sql), limits, s => { val _ = said.add(s) }) match {
          case Outcome.Failed(why) => why
          case other => throw new java.lang.AssertionError(other.toString)
        }
      (
        failed("SELECT ssn FROM app.applicants"),
        failed("SELECT nope FROM app.leads"),
        failed("INSERT INTO app.scratch VALUES (1)"),
        failed("SELECT pg_sleep(2)", AppDatabase.limits(timeout = 200.milliseconds)),
        failed("SELECT 1; SELECT 2"),
        said.asScala.toVector
      ) ==> (
        "The leads database does not allow reading that: permission denied for table applicants.",
        "The leads database refused the query: column \"nope\" does not exist.",
        "The leads database is only read here, and that statement would change it, so it was not run.",
        "The query ran past 200 milliseconds and was stopped: narrow it, or add a LIMIT.",
        "The call to `leads_query` was not run: Send one SQL statement, with no ; inside it. " +
          "You sent: {\"sql\":\"SELECT 1; SELECT 2\"}",
        Vector(
          "leads_query: 42501 permission denied for table applicants",
          "leads_query: 42703 column \"nope\" does not exist"
        )
      )
    }
  }
}
