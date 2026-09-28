package grit.app.main

import grit.core.inbox.{Inbox, InboxContract}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

/** The inbox contract, kept by SqlInbox against a real Postgres, under a launched engine (a
  * turn's progress is read from DBOS's tables, which launching creates).
  */
object SqlInboxContractTests extends InboxContract {
  import LiveTurn.*

  private lazy val config = TestPostgres.freshDatabase("sql_inbox_contract")

  protected def withInbox[A](body: Inbox => A): A = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(engine, engine.entries, new CountingProvider)
      body(engine.inbox)
    } finally engine.close()
  }
}
