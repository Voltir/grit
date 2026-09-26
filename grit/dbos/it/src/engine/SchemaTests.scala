package grit.dbos.engine

import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Locality
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
      Engine.open(config, "test").close()
      val origin = Origin.Task("schema", "twice")
      val first = LiveDb.conversation(config, origin)
      Engine.open(config, "test").close()
      LiveDb.conversation(config, origin).id ==> first.id
    }

    test("a period row closed in part is refused: closed means reason, last turn, entry, order") {
      val config = TestPostgres.freshDatabase("schema_periods")
      Engine.open(config, "test").close()
      val c = LiveDb.conversation(config, Origin.Task("schema", "half-closed")).id
      val refused =
        try {
          LiveDb.transaction(config)(
            execute(
              "INSERT INTO grit.periods (conversation_id, seq, first_turn, opened_at, closed_at, reason) " +
                s"VALUES ('${grit.core.id.ConversationId.value(c)}', 1, 0, now(), now(), 'lapsed')"
            )
          )
          None
        } catch { case NonFatal(e) => Option(e.getMessage) }
      assert(refused.exists(_.contains("periods_check")))
    }

    test("the settings are kept as set, the ledger window with them") {
      val config = TestPostgres.freshDatabase("schema_ledger")
      Engine.open(config, "test").close()
      val settings = Windows
        .of(2.minutes, 3.minutes, 6.minutes)
        .flatMap(LifecycleSettings.of(_, 300, 1.minute, Probability.One, 1, Locality.Default))
        .fold(sys.error, identity)
      LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
      LiveDb.transaction(config)(new SqlLifecycleStore().current()) ==> Right(settings)
    }

    test("settings changed by hand to break their rules read as Invalid") {
      val config = TestPostgres.freshDatabase("schema_settings")
      Engine.open(config, "test").close()
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
