package grit.dbos.engine

import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.{Locality, Scope, Weight}
import grit.core.store.{Origin, StoreError, Tx}
import grit.dbos.sql.{LiveDb, SqlLifecycleStore, TestPostgres}

import utest.*

/** `schema.sql`, which every [[Engine.open]] applies, against a real Postgres. */
object SchemaTests extends TestSuite {

  /** Runs `sql`, arranging rows the stores would never write. */
  private def execute(sql: String)(using tx: Tx^): Unit = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(conn.createStatement())(_.execute(sql))
    ()
  }

  val tests = Tests {
    test("opening an engine again applies the schema again, keeping the rows already there") {
      val config = TestPostgres.freshDatabase("schema_twice")
      LiveEngine.open(config, "test").close()
      val origin = Origin.Task("schema", "twice")
      val first = LiveDb.conversation(config, origin)
      LiveEngine.open(config, "test").close()
      LiveDb.conversation(config, origin).id ==> first.id
    }

    test("a period row closed in part is refused: closed means reason, last turn, entry, order") {
      val config = TestPostgres.freshDatabase("schema_periods")
      LiveEngine.open(config, "test").close()
      val c = grit.core.id.ConversationId.value(
        LiveDb.conversation(config, Origin.Task("schema", "half-closed")).id
      )
      val closed =
        Vector(
          "reason" -> "'lapsed'",
          "last_turn" -> "0",
          "closing_id" -> "'x'",
          "close_ordinal" -> "1"
        )
      val Violated = """(?s).*violates check constraint "(\w+)".*""".r
      // Each row closed but for one column. The constraint is the table's first unnamed CHECK;
      // `periods_check1` and `periods_check2` are the others.
      closed.map { (missing, _) =>
        val set = closed.filterNot(_._1 == missing)
        val refused =
          try {
            LiveDb.transaction(config)(
              execute(
                s"INSERT INTO grit.periods (conversation_id, seq, first_turn, opened_at, closed_at, ${set.map(_._1).mkString(", ")}) " +
                  s"VALUES ('$c', 1, 0, now(), now(), ${set.map(_._2).mkString(", ")})"
              )
            )
            None
          } catch { case NonFatal(e) => Option(e.getMessage) }
        missing -> refused.collect { case Violated(name) => name }
      } ==> closed.map((missing, _) => missing -> Some("periods_check"))
    }

    test(
      "a schedule row is refused when asked without its call or declared with one or a room, ended in part, or ended with a slot left"
    ) {
      val config = TestPostgres.freshDatabase("schema_schedules")
      LiveEngine.open(config, "test").close()
      val Violated = """(?s).*violates check constraint "(\w+)".*""".r
      def refused(columns: String, values: String): Option[String] =
        try {
          LiveDb.transaction(config)(
            execute(
              "INSERT INTO grit.schedules (id, job, rule, params, principal, report, created_at, " +
                s"$columns) VALUES ('s', 'j', '{}', '{}', 'grit', '{}', now(), $values)"
            )
          )
          None
        } catch { case NonFatal(e) => Option(e.getMessage).collect { case Violated(n) => n } }
      Vector(
        refused("source", "'asked'"),
        refused("source, asked_in", "'declared', 'tool:c:0:0:0'"),
        refused("source, ended", "'declared', 'ran'"),
        refused("source, ended_at", "'declared', now()"),
        refused("source, ended, ended_at, next_at", "'declared', 'ran', now(), now()"),
        refused("source, ended, ended_at", "'declared', 'gone', now()"),
        refused("source, room_id", "'declared', gen_random_uuid()")
      ) ==> Vector(
        Some("schedules_check"),
        Some("schedules_check"),
        Some("schedules_check1"),
        Some("schedules_check1"),
        Some("schedules_check2"),
        Some("schedules_ended_check"),
        Some("schedules_room_asked")
      )
      refused("source, ended, ended_at", "'declared', 'ran', now()") ==> None
    }

    test("the settings are kept as set, the ledger window with them") {
      val config = TestPostgres.freshDatabase("schema_ledger")
      LiveEngine.open(config, "test").close()
      val settings = Windows
        .of(2.minutes, 3.minutes, 6.minutes)
        .flatMap(LifecycleSettings.of(_, 300, 1.minute, Probability.One, 1, Locality.Default))
        .fold(sys.error, identity)
      LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
      LiveDb.transaction(config)(new SqlLifecycleStore().current()) ==> Right(settings)
    }

    test("a scope of the room and places is kept as set") {
      val config = TestPostgres.freshDatabase("schema_room")
      LiveEngine.open(config, "test").close()
      val scope = Scope.read("room slack:T1").fold(sys.error, identity)
      val d = LifecycleSettings.Default
      val settings = LifecycleSettings
        .of(d.windows, d.balance, d.settle, d.resolveAt, d.asks, Locality(scope, Weight.Default))
        .fold(sys.error, identity)
      LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
      LiveDb.transaction(config)(new SqlLifecycleStore().current()).map(_.locality.scope) ==>
        Right(scope)
    }

    test("settings changed by hand to break their rules read as Invalid") {
      val config = TestPostgres.freshDatabase("schema_settings")
      LiveEngine.open(config, "test").close()
      LiveDb.transaction(config)(
        execute(
          "INSERT INTO grit.lifecycle_settings " +
            "(idle, retention, ledger, balance, settle, resolve_at, asks, scope, weight) " +
            "VALUES ('10 minutes', '30 days', '180 days', 4096, '1 hour', 0.8, 3, '{everywhere}', 2)"
        )
      )
      LiveDb.transaction(config)(new SqlLifecycleStore().current()) ==>
        Left(StoreError.Invalid("lifecycle settings: settle must be shorter than idle"))
      LiveDb.transaction(config)(
        execute("UPDATE grit.lifecycle_settings SET settle = '1 minute', scope = '{fs:/a b, home}'")
      )
      LiveDb.transaction(config)(new SqlLifecycleStore().current()) ==> Left(
        StoreError.Invalid(
          "lifecycle settings: no namespace in home: write it as fs:/a/path, slack:team/channel or task:name"
        )
      )
    }
  }
}
