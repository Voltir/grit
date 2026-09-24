package grit.app.main

import grit.core.id.{ConversationId, EntryId}
import grit.core.store.{Entry, EntryStore, StoreError, Tx}
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig
import grit.turn.Turn

/** One phase of `M0GateLiveTests`' crash test, run in a JVM of its own: starts a turn on
  * the database named by `GRIT_DATABASE_*` and halts the JVM, as a crash would, when the
  * turn appends its reply, after the model call is recorded.
  */
object CrashingTurn {

  /** The exit status of the halt, so the test can tell it from any other exit. */
  val Halted = 37

  def main(args: Array[String]): Unit = {
    val config = DbConfig.fromEnv(sys.env).fold(e => sys.error(e.message), identity)
    val engine = Engine.open(config, Turn.Epoch)
    val entries = new EntryStore {
      def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = {
        if (EntryId.value(entry.id).startsWith("reply:")) Runtime.getRuntime.halt(Halted)
        engine.entries.insert(entry)
      }
      def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = engine.entries.get(id)
      def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
        engine.entries.list(c)
      def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
        engine.entries.lockNext(c)
    }
    LiveTurn.launch(engine, entries, new LiveTurn.CountingProvider)
    val turn = LiveTurn.say(engine, args(0))
    engine.awaitTurn(turn)
    // Reaching here means the halt never fired.
    engine.close()
    sys.exit(1)
  }
}
