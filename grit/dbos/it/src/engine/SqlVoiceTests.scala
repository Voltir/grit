package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import grit.core.prompt.Voice
import grit.core.store.{Tx, VoiceContract, VoiceStore}
import grit.dbos.sql.{DbConfig, LiveDb, SqlVoiceStore, TestPostgres}

import utest.*

/** The voice contract, kept by the SQL store against a real Postgres. */
object SqlVoiceTests extends VoiceContract {

  // The voice is one row per database, so each test gets a database of its own. Only ever
  // holds an immutable config; the suite's tests run one at a time.
  @caps.unsafe.untrackedCaptures
  private var config: Option[DbConfig] = None

  private def database(): DbConfig = {
    val c = SqlVoiceDatabases.fresh()
    config = Some(c)
    c
  }

  protected def fresh(): VoiceStore = {
    val _ = database()
    new SqlVoiceStore()
  }

  protected def transaction[A](body: (Tx^) ?=> A): A =
    LiveDb.transaction(config.getOrElse(throw new java.lang.AssertionError("no database")))(body)
}

/** What only the SQL store can hold: a voice row written by another build. */
object SqlVoiceUnknownTests extends TestSuite {

  val tests = Tests {
    test("a stored name this build does not know reads as the default, sassy") {
      val c = SqlVoiceDatabases.fresh()
      LiveDb.transaction(c) { (tx: Tx^) ?=>
        val conn: java.sql.Connection^{tx} = Tx.connection(tx)
        val _ = conn
          .createStatement()
          .executeUpdate("INSERT INTO grit.voice (kind, text) VALUES ('named', 'pirate')")
      }
      LiveDb.transaction(c)(new SqlVoiceStore().current()) ==> Right(Voice.Named.Sassy)
    }
  }
}

/** A fresh database per voice test, with schema.sql applied (opening an engine applies it;
  * nothing here launches DBOS).
  */
private object SqlVoiceDatabases {

  private val made = new AtomicInteger

  def fresh(): DbConfig = {
    val c = TestPostgres.freshDatabase(s"sql_voice_${made.incrementAndGet()}")
    LiveEngine.open(c, "test").close()
    c
  }
}
