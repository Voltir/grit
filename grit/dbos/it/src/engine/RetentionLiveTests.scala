package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq, SourceId, TurnSeq, WorkflowId}
import grit.core.message.Message
import grit.core.period.{CloseReason, Closing, LifecycleSettings, PeriodState, Windows}
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlPeriodStore,
  TestPostgres
}

import dev.dbos.transact.DBOSClient
import utest.*

/** The sweep's purge over DBOS against a real Postgres: a period closed longer than the
  * retention window loses its raw entries and its turn and close workflows, and keeps its
  * closing entry and its row.
  */
object RetentionLiveTests extends TestSuite {

  private val periods = new SqlPeriodStore(new SqlEntryStore())

  /** A minute idle, a minute's grace, a minute's retention. */
  private def minutes(config: DbConfig): Unit = {
    val settings = Windows
      .of(1.minute, 1.minute, 1.minute)
      .flatMap(LifecycleSettings.of(_, 3))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  /** A turn that records one step. */
  private def turn(id: WorkflowId)(using d: Durable^): String =
    d.step("say")(() => WorkflowId.value(id))

  /** A close that seals its period at once, in a step, with a fixed closing. */
  private def close(id: WorkflowId)(using d: Durable^): String =
    CloseRef.fromWorkflowId(id) match {
      case None => "not a close"
      case Some(attempt) =>
        val closing = Closing
          .of("kept", None, Vector(), Vector(), Vector(), Vector())
          .getOrElse(sys.error("closing"))
        d.transact("seal")(
          periods.seal(attempt, CloseReason.Lapsed, closing, Instant.now()).toString
        )
    }

  /** How many of `ids` DBOS still has a workflow or a step of. */
  private def kept(config: DbConfig, ids: Vector[WorkflowId]): Vector[Int] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Vector("dbos.workflow_status", "dbos.operation_outputs").map { table =>
        Using.resource(
          conn.prepareStatement(s"SELECT count(*) FROM $table WHERE workflow_uuid = ANY(?)")
        ) { ps =>
          ps.setArray(
            1,
            conn.createArrayOf(
              "text",
              // A fresh array the driver only reads; separation checking treats arrays as mutable.
              caps.unsafe.unsafeAssumePure(ids.map(WorkflowId.value).toArray[AnyRef])
            )
          )
          Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) })
        }
      }
    }

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  val tests = Tests {
    test(
      "a purge deletes a period's raw entries and workflows, keeping its closing entry and row"
    ) {
      val config = TestPostgres.freshDatabase("retention")
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minutes(config)
        val origin = Origin.Task("retention", "purge")
        val t0 = engine.inbox
          .ingest(origin, SourceId("one"), Message.User("one"))
          .fold(e => sys.error(s"$e"), identity)
        engine.inbox.startTurn(t0) ==> Right(())
        engine.awaitTurn(t0)
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        val attempt = CloseRef(p1, TurnSeq(0))
        engine.sweep(Instant.now().plusSeconds(120)).map(_.enqueued) ==> Right(Vector(attempt))
        assert(
          eventually(
            LiveDb
              .transaction(config)(periods.get(p1))
              .exists(_.exists(_.state match {
                case PeriodState.Closed(_, _, _, _, _, _) => true
                case PeriodState.Open(_) => false
              }))
          )
        )
        val workflows = Vector(t0.workflowId, attempt.workflowId)
        assert(eventually(engine.status(t0) != TurnStatus.Unknown))
        kept(config, workflows).map(_ > 0) ==> Vector(true, true)

        // As if a sweep deleted the workflows and died before the entries: the next one
        // deletes them again, which DBOS takes as nothing to do.
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try client.deleteWorkflows(java.util.List.of(WorkflowId.value(t0.workflowId)), false)
        finally client.close()

        val later = Instant.now().plusSeconds(180)
        engine.sweep(later).map(_.purged) ==> Right(Vector(p1))
        kept(config, workflows) ==> Vector(0, 0)
        LiveDb
          .transaction(config)(new SqlEntryStore().list(t0.conversationId))
          .map(_.map(e => EntryId.value(e.id))) ==>
          Right(Vector(EntryId.value(p1.closingId)))
        LiveDb
          .transaction(config)(periods.get(p1))
          .map(_.map(_.state match {
            case PeriodState.Closed(_, _, _, _, _, purged) => purged.nonEmpty
            case PeriodState.Open(_) => false
          })) ==> Right(Some(true))
        engine.sweep(later.plusSeconds(60)) ==> Right(Swept.nothing)
      } finally engine.close()
    }
  }
}
