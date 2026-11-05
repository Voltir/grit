package grit.databases.edge

import scala.concurrent.duration.*

import grit.core.place.Service
import grit.core.secret.Variable
import grit.core.tool.{ArgsError, Retry, ToolName}
import grit.databases.connect.{Clip, Failure, QueryRefusal, Relation, Rows, Seen}
import grit.databases.declare.{Database, DatabaseRole, Limits, Login}

import utest.*

/** What [[DatabaseTools]] tell the model of a database, its tables and a query's rows, with no
  * database: their descriptions, answers, clips and refusals.
  */
object DatabaseToolsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new IllegalStateException(why), identity)

  private def database(name: String = "leads", limits: Limits = Limits.Default): Database =
    Database(
      right(Service.of(name)),
      Variable("APP_DB_URL"),
      Login.Password(Variable("APP_DB_USER"), Variable("APP_DB_PASSWORD")),
      right(DatabaseRole.of("reader")),
      "Leads.",
      limits
    )

  private val Leads = database()

  private def limits(
      rows: Int = 200,
      bytes: Int = 32 * 1024,
      timeout: FiniteDuration = 10.seconds
  ) =
    right(Limits.of(rows, bytes, timeout))

  private val leads = Relation(
    "app",
    "leads",
    Relation.Kind.Table,
    Some("One row per lead the application took in.\nKept for a year."),
    Vector(
      Relation.Column("id", "integer", Some("Ours, in the offers URL.")),
      Relation.Column("external_id", "text", Some("The one sent to the supplier."))
    ),
    Vector.empty
  )
  private val applicants = Relation(
    "app",
    "applicants",
    Relation.Kind.Table,
    None,
    Vector(Relation.Column("id", "integer", None), Relation.Column("lead_id", "integer", None)),
    Vector(Relation.Reference("app", "leads", Vector("lead_id" -> "id")))
  )
  private val ids =
    Relation("app", "lead_ids", Relation.Kind.View, None, Vector.empty, Vector.empty)

  private val described = right(DatabaseTools.described(Leads))

  private val failure = Failure(Some("42703"), "column \"nope\" does not exist")

  val tests = Tests {
    test("both tools are named for the service, declare no writes, ask no one, and rerun") {
      val entries = described.offered.tools
      entries.map(e => (ToolName.value(e.name), e.writes, e.asks, e.retry)) ==> Vector(
        ("leads_tables", None, false, Retry.Rerun),
        ("leads_query", None, false, Retry.Rerun)
      )
    }

    test("a service named past 57 characters is refused, a tool's name being at most 64") {
      assert(DatabaseTools.described(database("a" * 57)).isRight)
      assert(DatabaseTools.described(database("a" * 58)).isLeft)
    }

    test("what the model is told names no tool, words it may echo") {
      val told = Vector(described.tables.spec.does, described.query.spec.does) ++
        described.offered.tools.map(_.parameters.render()) ++
        Vector(
          DatabaseTools.OneOnly,
          DatabaseTools.relation(Leads, Vector(leads), "nope").left.getOrElse("")
        ) ++
        Refusals.map(DatabaseTools.refused(Leads, _)) ++
        Refusals.map(DatabaseTools.listingRefused(Leads, _))
      told.filter(t => t.contains("leads_tables") || t.contains("leads_query")) ==> Vector()
    }

    test("the query's description says what it may read, one statement, and its limits") {
      described.query.spec.does ==>
        "Run one read-only SQL statement (Postgres) on the leads database and get its rows back " +
        "as a table. List the tables first: you may read only the tables and columns that " +
        "listing shows, and anything else is refused. Send a single statement with no ; inside " +
        "it. At most 200 rows and 32 KB come back, and a statement still running after 10 " +
        "seconds is stopped, so filter, aggregate and LIMIT in the SQL. NULL is shown as NULL; " +
        "text that is empty or reads NULL is shown in single quotes."
    }

    test("a text that is not one statement is refused as an argument, in one sentence") {
      def read(sql: String) = described.query.spec.args.read(ujson.Obj("sql" -> sql))
      read("SELECT 1; SELECT 2") ==> Left(
        ArgsError.Refused("sql", "Send one SQL statement, with no ; inside it.")
      )
      read("  ") ==> Left(ArgsError.Refused("sql", "Send one SQL statement, with no ; inside it."))
      assert(read("SELECT 1;").isRight)
    }

    test("a blank table name lists them all") {
      described.tables.spec.args.read(ujson.Obj("table" -> "  ")) ==> Right(None)
      described.tables.spec.args.read(ujson.Obj("table" -> " app.leads ")) ==> Right(
        Some("app.leads")
      )
    }

    test("the listing is a line for each relation: its kind, and its comment's first line") {
      DatabaseTools.listing(Leads, Vector(applicants, ids, leads)) ==>
        """The tables and views you may read in the leads database:
          |- app.applicants (table)
          |- app.lead_ids (view)
          |- app.leads (table): One row per lead the application took in.""".stripMargin
      DatabaseTools.listing(Leads, Vector.empty) ==>
        "You may read no table or view in the leads database."
    }

    test("a listing past the byte limit keeps its first lines and says so") {
      val small = database(limits = limits(bytes = 90))
      DatabaseTools.listing(small, Vector(applicants, ids, leads)) ==>
        """The tables and views you may read in the leads database:
          |- app.applicants (table)
          |
          |[The first 1 of them: the rest would go over 90 bytes. Name one to see its columns.]""".stripMargin
    }

    test("one relation is its comment, a table of its columns, and its references") {
      DatabaseTools.relation(Leads, Vector(applicants, ids, leads), "leads") ==> Right(
        """app.leads (table): One row per lead the application took in.
          |Kept for a year.
          |
          || column | type | description |
          ||---|---|---|
          || id | integer | Ours, in the offers URL. |
          || external_id | text | The one sent to the supplier. |""".stripMargin
      )
      DatabaseTools.relation(Leads, Vector(applicants, leads), "APP.Applicants") ==> Right(
        """app.applicants (table)
          |
          || column | type | description |
          ||---|---|---|
          || id | integer |  |
          || lead_id | integer |  |
          |
          |References:
          |- (lead_id) references app.leads (id)""".stripMargin
      )
      DatabaseTools.relation(Leads, Vector(ids), "lead_ids") ==> Right(
        "app.lead_ids (view)\n\nYou may read none of its columns."
      )
    }

    test("a name no relation has is refused, saying how to list them") {
      DatabaseTools.relation(Leads, Vector(leads), "offers") ==> Left(
        "The leads database has no table or view named offers that you may read: list the " +
          "tables, naming none, to see them."
      )
    }

    test("a query's rows are a table, NULL written NULL, empty and null text in quotes") {
      val rows = Rows(
        Vector("id", "external_id"),
        Vector(
          Vector(Some("1"), Some("x-1")),
          Vector(Some("3"), None),
          Vector(Some("4"), Some("")),
          Vector(Some("5"), Some("Null")),
          Vector(Some("6"), Some("a|b"))
        ),
        None
      )
      DatabaseTools.answer(Leads, Some(rows)) ==>
        """|| id | external_id |
          ||---|---|
          || 1 | x-1 |
          || 3 | NULL |
          || 4 | '' |
          || 5 | 'Null' |
          || 6 | a\|b |""".stripMargin
    }

    test("a query with no rows says so under its columns; one with no result, that") {
      DatabaseTools.answer(Leads, Some(Rows(Vector("id"), Vector.empty, None))) ==>
        "| id |\n|---|\n\nNo rows."
      DatabaseTools.answer(Leads, None) ==> "The statement ran and returned no result."
    }

    test("each clip is said after the rows it kept") {
      val one = Vector(Vector(Some("1")))
      DatabaseTools.answer(Leads, Some(Rows(Vector("id"), one, Some(Clip.Rows)))) ==>
        "| id |\n|---|\n| 1 |\n\n[The first 1 rows of more.]"
      DatabaseTools.answer(Leads, Some(Rows(Vector("id"), one, Some(Clip.Bytes)))) ==>
        "| id |\n|---|\n| 1 |\n\n[The first 1 rows: the next would go over 32 KB.]"
      DatabaseTools.answer(Leads, Some(Rows(Vector("id"), Vector.empty, Some(Clip.Bytes)))) ==>
        "| id |\n|---|\n\nNo rows.\n\n[No row fits in 32 KB: select fewer columns.]"
    }

    test("an answer written past the byte limit keeps its first rows and says so") {
      val small = database(limits = limits(bytes = 20))
      val rows = Rows(Vector("id"), Vector(Vector(Some("1")), Vector(Some("2"))), None)
      DatabaseTools.answer(small, Some(rows)) ==>
        "| id |\n|---|\n| 1 |\n\n[The first 1 rows: the rest would go over 20 bytes.]"
    }

    test("each refusal is one sentence, naming the database, never a state or a role") {
      Refusals.map(DatabaseTools.refused(Leads, _)) ==> Vector(
        "The leads database does not allow reading that: permission denied for table hidden.",
        "The leads database refused the query: column \"nope\" does not exist.",
        "The leads database is only read here, and that statement would change it, so it was not run.",
        "The query ran past 10 seconds and was stopped: narrow it, or add a LIMIT.",
        "The answer was too large to read: select fewer columns or rows.",
        "The leads database could not be reached just now.",
        "The leads database could not be read just now."
      )
      Refusals
        .map(DatabaseTools.listingRefused(database(limits = limits(timeout = 200.millis)), _))
        .slice(3, 5) ==> Vector(
        "The leads database took longer than 200 milliseconds to list its tables.",
        "The list of tables was too large to read."
      )
    }

    test("the operator is told a failure's state and message, and the roles a session was") {
      Refusals.map(DatabaseTools.logged) ==> Vector(
        Some("42501 permission denied for table hidden"),
        Some("42703 column \"nope\" does not exist"),
        None,
        None,
        Some("an answer passed the driver's buffer; its connection is closed"),
        Some("08006 An I/O error occurred while sending to the backend"),
        Some(
          "the session was other (session reader), not the declared role; its connection is closed"
        )
      )
    }
  }

  private val Refusals: Vector[QueryRefusal] = Vector(
    QueryRefusal.NotPermitted(Failure(Some("42501"), "permission denied for table hidden")),
    QueryRefusal.Rejected(failure),
    QueryRefusal.Writes,
    QueryRefusal.TimedOut,
    QueryRefusal.TooLarge,
    QueryRefusal.Unreachable(
      Failure(Some("08006"), "An I/O error occurred while sending to the backend")
    ),
    QueryRefusal.RoleChanged(Seen("other", "reader"))
  )
}
