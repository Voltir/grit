package grit.app.main

import grit.core.id.{ConversationId, EntryId, TurnRef}
import grit.core.message.Message
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, EntryStore, StoreError, Tx}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.DbConfig
import grit.models.StubProvider
import grit.turn.Turn

/** One phase of a crash test, run in a JVM of its own: starts a turn on the database
  * named by `GRIT_DATABASE_*` and halts the JVM, as a crash would: when the turn appends
  * its reply, after the model call is recorded (`ProviderOnceLiveTests`); or, given
  * `mid-stream`, part way through the model's streamed reply (`TurnRecordLiveTests`).
  */
object CrashingTurn {

  /** The exit status of the halt, so the test can tell it from any other exit. */
  val Halted = 37

  /** How many of the stub's words the mid-stream phase tells before it halts. */
  val MidStreamPieces = 30

  def main(args: Array[String]): Unit = {
    val config = DbConfig.fromEnv(sys.env).fold(e => sys.error(e.message), identity)
    val engine = LiveEngine.open(config, Turn.Epoch)
    val entries = new EntryStore {
      def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = {
        if (entry.id == TurnRef(entry.conversationId, entry.turnSeq).replyId)
          Runtime.getRuntime.halt(Halted)
        engine.entries.insert(entry)
      }
      def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = engine.entries.get(id)
      def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
        engine.entries.list(c)
      def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
        engine.entries.lockNext(c)
    }
    if (args.lift(1).contains("mid-stream")) LiveTurn.launch(engine, engine.entries, haltingStub)
    else LiveTurn.launch(engine, entries, new StubProvider())
    val turn = LiveTurn.say(engine, args(0))
    engine.awaitTurn(turn)
    // Reaching here means the halt never fired.
    engine.close()
    sys.exit(1)
  }

  /** A slow stub that halts the JVM [[MidStreamPieces]] deltas into its streamed reply, so
    * pieces are written before the halt.
    */
  private def haltingStub: Provider =
    new Provider {
      def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
        new StubProvider().complete(request)
      override def stream(
          request: ModelRequest,
          onDelta: Delta => Unit
      ): Either[ProviderError, Message.Assistant] = {
        var told = 0
        new StubProvider(2000).stream(
          request,
          d => {
            if (told == MidStreamPieces) Runtime.getRuntime.halt(Halted)
            told += 1
            onDelta(d)
          }
        )
      }
    }
}
