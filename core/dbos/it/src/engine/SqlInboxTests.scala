package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.id.{
  CallSlot,
  ConversationId,
  PeriodRef,
  PeriodSeq,
  SourceId,
  StitchRef,
  TriageRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.speech.Reach
import grit.core.store.{Origin, StoreError, Tx}
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** What only the inbox against a real Postgres shows; the inbox contract is kept by
  * SqlInboxContractTests in grit.app, under a launched engine. The stores' own contract is
  * [[SqlStoreTests]].
  */
object SqlInboxTests extends TestSuite {

  // Opening an engine applies schema.sql and DBOS's; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_inbox")
    LiveEngine.open(c, "test").close()
    c
  }

  /** Runs `sql`, deleting rows as a purge would. */
  private def execute(sql: String)(using tx: Tx^): Unit = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(conn.createStatement())(_.execute(sql))
    ()
  }

  /** Each workflow queued whose id names `conversation`: its id, queue and partition key, in
    * the order created.
    */
  private def queued(
      conversation: ConversationId
  )(using tx: Tx^): Vector[(String, String, String)] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(
      conn.prepareStatement(
        """SELECT workflow_uuid, queue_name, queue_partition_key FROM dbos.workflow_status
          | WHERE workflow_uuid LIKE '%' || ? || '%' ORDER BY created_at, workflow_uuid""".stripMargin
      )
    ) { ps =>
      ps.setString(1, ConversationId.value(conversation))
      Using.resource(ps.executeQuery()) { rs =>
        val rows = Vector.newBuilder[(String, String, String)]
        while (rs.next()) rows += ((rs.getString(1), rs.getString(2), rs.getString(3)))
        rows.result()
      }
    }
  }

  val tests = Tests {
    test(
      "an opening heard or said is queued for placement under its room, before its triage; a reply is not"
    ) {
      val heard = Origin.Slack("T1", "C1", "1.0")
      val said = Origin.Slack("T1", "C1", "2.0")
      val at = Instant.parse("2026-10-02T16:59:36.123456Z")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.inbox.hear(
          heard,
          SourceId("1.0"),
          "lunch?",
          Account.Local,
          at,
          Reach.Nowhere
        ) ==>
          Right(())
        engine.inbox.hear(heard, SourceId("1.1"), "yes", Account.Local, at, Reach.Nowhere) ==>
          Right(())
        val asked = engine.inbox
          .ingest(said, SourceId("2.0"), Message.User("@grit lunch?"), Account.Local)
          .fold(e => sys.error(e.toString), identity)
        val h = LiveDb.conversation(config, heard).id
        val placement = StitchRef(TurnRef(h, TurnSeq.First), at).workflowId
        val triages =
          Vector(0L, 1L).map(t => TriageRef(PeriodRef(h, PeriodSeq.First), TurnSeq(t)).workflowId)
        LiveDb.transaction(config)(Right(queued(h))) ==> Right(
          Vector(
            (WorkflowId.value(placement), "stitches", "slack:T1/C1"),
            (WorkflowId.value(triages(0)), "turns", ConversationId.value(h)),
            (WorkflowId.value(triages(1)), "turns", ConversationId.value(h))
          )
        )
        LiveDb
          .transaction(config)(Right(queued(asked.conversationId)))
          .map(_.map(_._1).filter(_.startsWith(StitchRef.Prefix))) ==> Right(
          Vector(
            WorkflowId.value(
              StitchRef(
                asked,
                LiveDb
                  .transaction(config)(engine.entries.list(asked.conversationId))
                  .toOption
                  .flatMap(_.headOption)
                  .fold(Instant.EPOCH)(_.createdAt)
              ).workflowId
            )
          )
        )
      } finally engine.close()
    }

    test("a post is searched by its text, and the call that made it goes with its entry") {
      val origin = Origin.Task("sql", "posted")
      val slot = CallSlot
        .of(TurnRef(ConversationId("0190a000-0000-7000-8000-000000000001"), TurnSeq.First), 0, 0)
        .getOrElse(throw new java.lang.AssertionError("a slot at 0, 0 reads"))
      val engine = LiveEngine.open(config, "test")
      try {
        engine.inbox.posted(
          origin,
          SourceId("root"),
          "the retries issue is open",
          Instant.parse("2026-09-25T09:00:00Z"),
          slot,
          Account.Local
        ) ==> Right(true)
        val c = LiveDb.conversation(config, origin).id
        LiveDb
          .transaction(config)(
            engine.search.search(c, TurnSeq.First, TurnSeq.First.next, "retries", 5)
          )
          .map(_.map(_.turn.turnSeq)) ==> Right(Vector(TurnSeq.First))
        LiveDb.transaction(config)(engine.conversations.postedBy(c)) ==> Right(Some(slot))
        LiveDb.transaction(config) {
          execute(
            s"DELETE FROM grit.entries WHERE conversation_id = '${ConversationId.value(c)}'"
          )
          engine.conversations.postedBy(c)
        } ==> (Right(None): Either[StoreError, Option[CallSlot]])
      } finally engine.close()
    }

    test("a turn's progress that cannot be read is Unavailable, never its end") {
      // Its own database, whose DBOS workflow table is taken away once the engine is open.
      val unreadable = TestPostgres.freshDatabase("sql_inbox_unreadable")
      val origin = Origin.Task("sql", "progress")
      val engine = LiveEngine.open(unreadable, "test")
      val progress =
        try {
          LiveDb.transaction(unreadable)(
            execute("ALTER TABLE dbos.workflow_status RENAME TO workflow_status_gone")
          )
          engine.inbox
            .ingest(origin, SourceId("m1"), Message.User("one"), Account.Local)
            .flatMap(engine.inbox.progress)
        } finally engine.close()
      progress.left.map {
        case InboxError.Unavailable(why) => why.contains("dbos.workflow_status")
        case other => false
      } ==> Left(true)
    }
  }
}
