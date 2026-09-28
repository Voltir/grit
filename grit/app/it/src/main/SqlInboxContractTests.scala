package grit.app.main

import java.util.UUID

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.inbox.{Inbox, InboxContract}
import grit.core.message.{Tokens, Usage}
import grit.core.spend.Budget
import grit.core.store.Origin
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

/** The inbox contract, kept by SqlInbox against a real Postgres, under a launched engine (a
  * turn's progress is read from DBOS's tables, which launching creates).
  */
object SqlInboxContractTests extends InboxContract {
  import LiveTurn.*

  private lazy val config = TestPostgres.freshDatabase("sql_inbox_contract")

  protected def withInbox[A](budget: Budget)(
      body: (Inbox, BigDecimal => Unit, Origin => Boolean) => A
  ): A = {
    val engine = LiveEngine.open(config, Turn.Epoch, budget)
    try {
      launch(engine, engine.entries, new CountingProvider)
      def spend(usd: BigDecimal): Unit = {
        val turn = TurnRef(ConversationId(UUID.randomUUID().toString), TurnSeq.First)
        val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(usd))
        engine.jot
          .write(
            engine.ledger
              .record(
                EntryId(s"spent:${UUID.randomUUID()}"),
                turn,
                turn.workflowId,
                "m",
                usage,
                Tokens(1)
              )
          )
          .fold(e => sys.error(e.toString), identity)
      }
      def exists(origin: Origin): Boolean =
        engine.db
          .read(engine.conversations.find(origin))
          .fold(e => sys.error(e.toString), _.nonEmpty)
      body(engine.inbox, spend, exists)
    } finally engine.close()
  }
}
