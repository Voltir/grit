package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, SourceId, WorkflowId}
import grit.core.message.Message
import grit.core.period.{
  CloseReason,
  LifecycleSettings,
  PeriodState,
  Probability,
  TestClosings,
  Windows
}
import grit.core.place.Locality
import grit.core.retention.Target
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlPeriodStore,
  SqlTombstones,
  TestPostgres
}
import grit.dbos.workflow.DurableWorkflow

import dev.dbos.transact.DBOSClient
import utest.*

/** The collector over DBOS against a real Postgres: a period closed longer than the retention
  * window loses its raw entries and its turn and close workflows, and keeps its closing entry
  * and its row; a tombstone whose workflows are still running waits for them.
  */
object CollectorLiveTests extends TestSuite {

  private val periods = new SqlPeriodStore(new SqlEntryStore())

  private val tombstones = new SqlTombstones

  /** A minute idle, a minute's retention. */
  private def minutes(config: DbConfig): Unit = {
    val settings = Windows
      .of(1.minute, 1.minute, 1.day)
      .flatMap(LifecycleSettings.of(_, 4096, 30.seconds, Probability.One, 1, Locality.Default))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  /** A turn that records one step. */
  private def turn(id: WorkflowId)(using d: Durable^): String =
    d.step("say")(() => WorkflowId.value(id))

  /** A close that seals its period at once, in a step, with a fixed closing, marking its raw
    * entries for deletion; one whose id ends `:hold` waits a few seconds for a message that
    * never comes.
    */
  private def close(id: WorkflowId)(using d: Durable^): String =
    CloseRef.fromWorkflowId(id) match {
      case None if WorkflowId.value(id).endsWith(":hold") =>
        d.recv("never", 4.seconds).getOrElse("held")
      case None => "not a close"
      case Some(attempt) =>
        val closing = TestClosings.prose("kept")
        val now = Instant.now()
        d.transact("seal")(
          periods
            .seal(attempt, CloseReason.Lapsed, closing, now)
            .flatMap(s => tombstones.write(Target.Raw(attempt.period), now).map(_ => s))
            .toString
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

  /** Whether every workflow whose id starts with `prefix` has ended. */
  private def ended(config: DbConfig, prefix: String): Boolean = {
    val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
    try
      client
        .listWorkflows(
          new dev.dbos.transact.workflow.ListWorkflowsInput().withWorkflowIdPrefix(prefix)
        )
        .asScala
        .forall(w => Option(w.status()).exists(!_.isActive()))
    finally client.close()
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
      "a raw collection deletes a period's raw entries and workflows, keeping its closing entry and row"
    ) {
      val config = TestPostgres.freshDatabase("collect_raw")
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
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
        val swept = engine.sweep(Instant.now().plusSeconds(120)).map(_.enqueued)
        val attempt = swept.toOption.flatMap(_.headOption).getOrElse(sys.error(s"none: $swept"))
        attempt.period ==> p1
        assert(
          eventually(
            LiveDb
              .transaction(config)(periods.get(p1))
              .exists(_.exists(_.state match {
                case PeriodState.Closed(_, _, _, _, _, _) => true
                case PeriodState.Open => false
              }))
          )
        )
        val workflows = Vector(t0.workflowId, attempt.workflowId)
        assert(eventually(engine.status(t0) != TurnStatus.Unknown))
        // The collector waits for a workflow still running: here, the close that sealed.
        val watching = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try
          assert(
            eventually(
              Option(
                watching
                  .retrieveWorkflow[String, Exception](WorkflowId.value(attempt.workflowId))
                  .getStatus()
              ).exists(s => !s.status().isActive())
            )
          )
        finally watching.close()
        kept(config, workflows).map(_ > 0) ==> Vector(true, true)

        // As if a sweep deleted the workflows and died before the entries: the next one
        // deletes them again, which DBOS takes as nothing to do.
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try client.deleteWorkflows(java.util.List.of(WorkflowId.value(t0.workflowId)), false)
        finally client.close()

        val later = Instant.now().plusSeconds(180)
        engine.sweep(later).map(_.collected) ==> Right(Vector(Target.Raw(p1)))
        kept(config, workflows) ==> Vector(0, 0)
        LiveDb
          .transaction(config)(new SqlEntryStore().list(t0.conversationId))
          .map(_.map(e => EntryId.value(e.id))) ==>
          Right(Vector(EntryId.value(p1.closingId)))
        LiveDb
          .transaction(config)(periods.get(p1))
          .map(_.map(_.state match {
            case PeriodState.Closed(_, _, _, _, _, purged) => purged.nonEmpty
            case PeriodState.Open => false
          })) ==> Right(Some(true))
        engine.sweep(later.plusSeconds(60)) ==> Right(Swept.nothing)
      } finally engine.close()
    }

    test(
      "a raw collection finds its period's close attempts and questions by their ids' prefix, and not period 10's"
    ) {
      val config = TestPostgres.freshDatabase("collect_prefix")
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minutes(config)
        val t0 = engine.inbox
          .ingest(Origin.Task("retention", "prefix"), SourceId("one"), Message.User("one"))
          .fold(e => sys.error(s"$e"), identity)
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        engine.sweep(Instant.now().plusSeconds(120)).map(_.enqueued.size) ==> Right(1)
        assert(
          eventually(
            LiveDb
              .transaction(config)(periods.get(p1))
              .exists(_.exists(_.state match {
                case PeriodState.Closed(_, _, _, _, _, _) => true
                case PeriodState.Open => false
              }))
          )
        )
        // Workflows named as close attempts and questions on period 1 and on period 10 would
        // be, whatever their turns: the stand-ins run nothing for any.
        val c = ConversationId.value(t0.conversationId)
        val one = Vector(WorkflowId(s"close:$c:1:stray"), WorkflowId(s"settle:$c:1:stray"))
        val ten = Vector(WorkflowId(s"close:$c:10:stray"), WorkflowId(s"settle:$c:10:stray"))
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try
          (one ++ ten).foreach { id =>
            val name = WorkflowId.value(id).takeWhile(_ != ':')
            val _ = client.enqueueWorkflow[String, Exception](
              new DBOSClient.EnqueueOptions(name, DurableWorkflow.ClassName, "turns")
                .withWorkflowId(WorkflowId.value(id))
                .withQueuePartitionKey(c),
              // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
              caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
            )
          }
        finally client.close()
        assert(eventually(kept(config, one) == Vector(2, 0)))
        assert(eventually(kept(config, ten) == Vector(2, 0)))
        // The collector waits for a workflow still running; these all end at once.
        assert(eventually(ended(config, s"close:$c:") && ended(config, s"settle:$c:")))
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==> Right(
          Vector(Target.Raw(p1))
        )
        (kept(config, one), kept(config, ten)) ==> (Vector(0, 0), Vector(2, 0))
      } finally engine.close()
    }

    test("a tombstone whose workflow is still running is deferred, and collected once it ends") {
      val config = TestPostgres.freshDatabase("collect_deferred")
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minutes(config)
        val t0 = engine.inbox
          .ingest(Origin.Task("collect", "deferred"), SourceId("one"), Message.User("one"))
          .fold(e => sys.error(s"$e"), identity)
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        engine.sweep(Instant.now().plusSeconds(120)).map(_.enqueued.size) ==> Right(1)
        assert(
          eventually(
            LiveDb
              .transaction(config)(periods.get(p1))
              .exists(_.exists(_.state match {
                case PeriodState.Closed(_, _, _, _, _, _) => true
                case PeriodState.Open => false
              }))
          )
        )
        // A workflow named as an attempt on period 1, still waiting when the sweep comes.
        val c = ConversationId.value(t0.conversationId)
        val hold = WorkflowId(s"close:$c:1:hold")
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try {
          val _ = client.enqueueWorkflow[String, Exception](
            new DBOSClient.EnqueueOptions("close", DurableWorkflow.ClassName, "turns")
              .withWorkflowId(WorkflowId.value(hold))
              .withQueuePartitionKey(c + ":hold"),
            // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
            caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
          )
          assert(eventually(kept(config, Vector(hold)) == Vector(1, 0)))
          val later = Instant.now().plusSeconds(180)
          engine.sweep(later).map(s => (s.collected, s.deferred)) ==>
            Right((Vector(), Vector(Target.Raw(p1))))
          kept(config, Vector(hold)).headOption ==> Some(1)
          assert(
            eventually(
              Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(hold)).getStatus())
                .exists(s => !s.status().isActive())
            )
          )
        } finally client.close()
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Raw(p1)))
        kept(config, Vector(hold)) ==> Vector(0, 0)
      } finally engine.close()
    }
  }
}
