package grit.dbos.engine

import grit.core.id.{PrincipalId, SourceId}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.store.Origin
import grit.dbos.sql.TestPostgres

import utest.*

/** What only the inbox against a real Postgres shows; the inbox contract is kept by
  * SqlInboxContractTests in grit.app, under a launched engine. The stores' own contract is
  * [[SqlStoreTests]].
  */
object SqlInboxTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_inbox")
    LiveEngine.open(c, "test").close()
    c
  }

  val tests = Tests {

    test("a turn's progress that cannot be read is Unavailable, never its end") {
      // DBOS never launched on this database, so its workflow tables do not exist.
      val origin = Origin.Task("sql", "progress")
      val engine = LiveEngine.open(config, "test")
      val progress =
        try
          engine.inbox
            .ingest(origin, SourceId("m1"), Message.User("one"), PrincipalId.Local)
            .flatMap(engine.inbox.progress)
        finally engine.close()
      progress.left.map {
        case InboxError.Unavailable(why) => why.contains("dbos.workflow_status")
        case other => false
      } ==> Left(true)
    }
  }
}
