package grit.dbos.engine

import java.time.{Instant, LocalTime, ZoneOffset}

import scala.util.Using

import grit.core.id.{
  ConversationId,
  Declarer,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  SourceId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.inbox.{InboxError, Slotted}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.hour
import grit.core.job.{Declared, Ending, Slot, SlotRule}
import grit.core.store.{StoreError, Tx}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** What only the SQL inbox shows of starting slots: a run as DBOS knows it, read back into
  * what the schedule gets. The engine is never launched, so a run enqueued stays queued until a
  * test moves its status; the shared cases are the inbox contract's.
  */
object SqlStartSlotTests extends TestSuite {

  // Opening an engine applies schema.sql and DBOS's; nothing here launches DBOS.
  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("sql_start_slot")
    LiveEngine.open(c, "test").close()
    c
  }

  private val remind = new Counting("remind")

  private val Due = Instant.parse("2026-10-07T09:00:00Z")

  private val Daily = SlotRule.Daily(LocalTime.of(9, 0), ZoneOffset.UTC)

  private def key(text: String): ScheduleKey = ScheduleKey.of(text).fold(sys.error, identity)

  private def scheduled(k: String): ScheduleId = ScheduleId.declared(Declarer.Deployment, key(k))

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** Makes the declared schedules `rules`, by key, as of `now`. */
  private def declare(engine: Engine^, rules: Vector[(String, SlotRule)], now: Instant): Unit =
    right(
      engine.jot.write(
        engine.schedules.declare(
          rules.map((k, rule) => (Declarer.Deployment, Declared(key(k), remind, rule, Count(1)))),
          now
        )
      )
    )

  private def begun(started: Either[InboxError, Slotted]): TurnRef =
    started match {
      case Right(Slotted.Started(turn, _)) => turn
      case other => throw new java.lang.AssertionError(s"not started: $other")
    }

  /** Runs `sql` on DBOS's tables, `id` its one parameter, as a crash or DBOS itself would. */
  private def execute(sql: String, id: WorkflowId)(using tx: Tx^): Unit = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(conn.prepareStatement(sql)) { ps =>
      ps.setString(1, WorkflowId.value(id))
      val _ = ps.executeUpdate()
    }
  }

  /** The workflow `id`'s name, queue, partition key and status; `None` when DBOS has none. */
  private def workflow(id: WorkflowId)(using tx: Tx^): Option[(String, String, String, String)] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(
      conn.prepareStatement(
        """SELECT name, queue_name, queue_partition_key, status FROM dbos.workflow_status
          | WHERE workflow_uuid = ?""".stripMargin
      )
    ) { ps =>
      ps.setString(1, WorkflowId.value(id))
      Using.resource(ps.executeQuery()) { rs =>
        Option.when(rs.next())((rs.getString(1), rs.getString(2), rs.getString(3), rs.getString(4)))
      }
    }
  }

  /** The ids of the placements DBOS has of `conversation`'s turns. */
  private def stitches(conversation: ConversationId)(using tx: Tx^): Vector[String] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(
      conn.prepareStatement(
        """SELECT workflow_uuid FROM dbos.workflow_status
          | WHERE name = 'stitch' AND workflow_uuid LIKE '%' || ? || '%'""".stripMargin
      )
    ) { ps =>
      ps.setString(1, ConversationId.value(conversation))
      Using.resource(ps.executeQuery()) { rs =>
        val ids = Vector.newBuilder[String]
        while (rs.next()) ids += rs.getString(1)
        ids.result()
      }
    }
  }

  private def withEngine[A](body: Engine^ => A): A = {
    val engine = LiveEngine.open(config, "test")
    try body(engine)
    finally engine.close()
  }

  val tests = Tests {
    test(
      "a run starts enqueued as its job's run on the turns queue under its conversation, which grit created; started again at that version it is left going"
    ) {
      withEngine { engine =>
        declare(engine, Vector("enqueued" -> SlotRule.Once(Due, hour)), Due)
        val turn = begun(engine.inbox.startSlot(scheduled("enqueued"), Some(1), Due))
        LiveDb.transaction(config)(workflow(turn.workflowId)) ==>
          Some(("run", "turns", ConversationId.value(turn.conversationId), "ENQUEUED"))
        right(engine.db.read(engine.conversations.get(turn.conversationId))).map(_.createdBy) ==>
          Some(PrincipalId.Grit)
        engine.inbox.startSlot(scheduled("enqueued"), Some(1), Due.plusSeconds(5)) ==>
          Right(Slotted.Idle)
        right(engine.db.read(engine.entries.list(turn.conversationId))).size ==> 1
      }
    }

    test(
      "a run whose workflow ended without a reply fails, however it ended, and is never enqueued again"
    ) {
      withEngine { engine =>
        val ends = Vector("ERROR", "CANCELLED", "SUCCESS")
        val keys = ends.map(s => s"ended-${s.toLowerCase}")
        declare(engine, keys.map(_ -> SlotRule.Once(Due, hour)), Due)
        val turns = keys.map(k => begun(engine.inbox.startSlot(scheduled(k), Some(1), Due)))
        LiveDb.transaction(config) {
          turns.zip(ends).foreach { (turn, status) =>
            execute(
              s"UPDATE dbos.workflow_status SET status = '$status' WHERE workflow_uuid = ?",
              turn.workflowId
            )
          }
        }
        keys.map(k => engine.inbox.startSlot(scheduled(k), Some(1), Due.plusSeconds(5))) ==>
          keys.map(k => Right(Slotted.Failed(Slot(scheduled(k), Due))))
        keys.map(k =>
          right(engine.db.read(engine.schedules.read(scheduled(k)))).flatMap(_.ended)
        ) ==>
          keys.map(_ => Some(Ending.Failed))
        turns.map(t => LiveDb.transaction(config)(workflow(t.workflowId)).map(_._4)) ==>
          ends.map(Some(_))
      }
    }

    // A run's opening is grit's, at a task's place where nothing interleaves: placing it among
    // its room's exchanges (ADR 0023) would cost a call for nothing.
    test("a run's opening is never placed: starting a slot enqueues no stitch workflow") {
      withEngine { engine =>
        declare(engine, Vector("unplaced" -> SlotRule.Once(Due, hour)), Due)
        val turn = begun(engine.inbox.startSlot(scheduled("unplaced"), Some(1), Due))
        LiveDb.transaction(config)(stitches(turn.conversationId)) ==> Vector()
      }
    }

    test("a run DBOS does not know, its enqueue lost, is enqueued again as the same turn") {
      withEngine { engine =>
        declare(engine, Vector("lost" -> SlotRule.Once(Due, hour)), Due)
        val turn = begun(engine.inbox.startSlot(scheduled("lost"), Some(1), Due))
        LiveDb.transaction(config)(
          execute("DELETE FROM dbos.workflow_status WHERE workflow_uuid = ?", turn.workflowId)
        )
        engine.inbox.startSlot(scheduled("lost"), Some(1), Due.plusSeconds(5)) ==>
          Right(Slotted.Restarted(turn))
        LiveDb.transaction(config)(workflow(turn.workflowId)).map(_._4) ==> Some("ENQUEUED")
      }
    }

    test(
      "a run going at another version is superseded by a new turn of its own slot, past its grace; a recurrence's by its latest due slot"
    ) {
      withEngine { engine =>
        declare(
          engine,
          Vector("old-once" -> SlotRule.Once(Due, hour), "old-daily" -> Daily),
          Due.minusSeconds(60)
        )
        val once = begun(engine.inbox.startSlot(scheduled("old-once"), Some(1), Due))
        val daily = begun(engine.inbox.startSlot(scheduled("old-daily"), Some(1), Due))
        val later = Due.plusSeconds(2 * 86400 + 3 * 3600)
        val again = engine.inbox.startSlot(scheduled("old-once"), Some(2), later)
        again ==> Right(
          Slotted.Superseding(
            TurnRef(once.conversationId, TurnSeq.First.next),
            Slot(scheduled("old-once"), Due)
          )
        )
        engine.inbox.ingested(
          Slot(scheduled("old-once"), Due).origin(remind.name),
          SourceId("v2")
        ) ==> Right(Some(TurnRef(once.conversationId, TurnSeq.First.next)))
        val latest = Slot(scheduled("old-daily"), Due.plusSeconds(2 * 86400))
        engine.inbox.startSlot(scheduled("old-daily"), Some(2), later).map {
          case Slotted.Superseding(turn, slot) =>
            (slot, turn.turnSeq, turn.conversationId == daily.conversationId)
          case other => other
        } ==> Right((latest, TurnSeq.First, false))
        Vector("old-once", "old-daily").map(k =>
          engine.inbox.startSlot(scheduled(k), Some(2), later.plusSeconds(5))
        ) ==> Vector(Right(Slotted.Idle), Right(Slotted.Idle))
      }
    }

    test(
      "a once schedule declared again after its run replied while undeclared ends ran, its slot never run again"
    ) {
      withEngine { engine =>
        declare(engine, Vector("revived" -> SlotRule.Once(Due, hour)), Due)
        val turn = begun(engine.inbox.startSlot(scheduled("revived"), Some(1), Due))
        declare(engine, Vector(), Due.plusSeconds(1))
        right(engine.jot.write(engine.schedules.replied(Slot(scheduled("revived"), Due), 1, Due)))
        declare(engine, Vector("revived" -> SlotRule.Once(Due, hour)), Due.plusSeconds(2))
        engine.inbox.startSlot(scheduled("revived"), Some(1), Due.plusSeconds(3)) ==>
          Right(Slotted.Idle)
        right(engine.db.read(engine.schedules.read(scheduled("revived")))).flatMap(_.ended) ==>
          Some(Ending.Ran)
        right(engine.db.read(engine.entries.list(turn.conversationId))).size ==> 1
      }
    }
  }
}
