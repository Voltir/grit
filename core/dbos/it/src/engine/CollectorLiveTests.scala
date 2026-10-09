package grit.dbos.engine

import java.time.{Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms, Written}
import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  ConversationId,
  Declarer,
  DocKey,
  DocumentVersion,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PluginName,
  ScheduleId,
  ScheduleKey,
  SourceId,
  TriageRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.job.{Declared, Ending, JobTests, SlotRule}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Policy}
import grit.core.period.{
  CloseOrdinal,
  CloseReason,
  LifecycleSettings,
  PeriodState,
  Probability,
  TestClosings,
  Windows
}
import grit.core.place.{Directory, Locality, Namespace, Place}
import grit.core.plugin.{CacheDocs, CachePosting, DocumentPosting, Documents, Plugin, PostRef}
import grit.core.retention.Target
import grit.core.store.{ClosedPeriod, Origin, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Subject}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlCacheDocs,
  SqlDocuments,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlPeriodStore,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlSchedules,
  SqlSpeechStore,
  SqlTombstones,
  SqlUsageLedger,
  TestPostgres
}
import grit.dbos.workflow.{DurableWorkflow, Posts}

import dev.dbos.transact.workflow.QueueName
import dev.dbos.transact.{DBOSClient, EnqueueOptions}
import utest.*

/** The collector over DBOS against a real Postgres: a period closed longer than the retention
  * window loses its raw entries and its turn and close workflows, and keeps its closing entry
  * and its row; a tombstone whose workflows are still running waits for them.
  */
object CollectorLiveTests extends TestSuite {

  private val periods = new SqlPeriodStore(new SqlEntryStore())

  private val tombstones = new SqlTombstones

  /** A minute idle, a minute's retention, and a `ledger` window. */
  private def minutes(
      config: DbConfig,
      ledger: FiniteDuration = 1.day,
      idle: FiniteDuration = 1.minute
  ): Unit = {
    val settings = Windows
      .of(idle, 1.minute, ledger)
      .flatMap(LifecycleSettings.of(_, 4096, 30.seconds, Probability.One, 1, Locality.Default))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  /** A turn that records one step, a transaction step as a real turn's are, so DBOS keeps its
    * output in `dbos.tx_step_outputs` too.
    */
  private def turn(id: WorkflowId)(using d: Durable^): String = {
    val said = WorkflowId.value(id)
    d.transact("say", Subject.Public)(said)
  }

  /** A close that seals its period at once, in a step, with a fixed closing, marking its raw
    * entries for deletion; one whose id ends `:hold` runs until the test sends it
    * [[release]].
    */
  private def close(id: WorkflowId)(using d: Durable^): String =
    CloseRef.fromWorkflowId(id) match {
      case None if WorkflowId.value(id).endsWith(":hold") =>
        // Long past any test's run: a test that fails before releasing it leaves it to the
        // engine's close.
        d.recv(Release, 10.minutes).getOrElse("held")
      case None => "not a close"
      case Some(attempt) =>
        val closing = TestClosings.prose("kept")
        val now = Instant.now()
        d.transact("seal", Subject.Public)(
          periods
            .seal(attempt, CloseReason.Lapsed, closing, now)
            .flatMap(s =>
              for {
                // As Close's seal writes them.
                _ <- tombstones.write(Target.Raw(attempt.period), now)
                _ <- PeriodSeq.of(PeriodSeq.value(attempt.period.seq) - 1) match {
                  case Some(before) =>
                    tombstones.write(
                      Target.Superseded(PeriodRef(attempt.period.conversationId, before)),
                      now
                    )
                  case None => Right(())
                }
                _ <- tombstones.write(Target.Quiet(attempt.period), now)
              } yield s
            )
            .toString
        )
    }

  private val Release = "release"

  /** Ends the `:hold` stand-in `hold`, and waits until it has ended. */
  private def release(config: DbConfig, hold: WorkflowId): Unit = {
    val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
    try client.send(WorkflowId.value(hold), "go", Release, s"release:${WorkflowId.value(hold)}")
    finally client.close()
    assert(eventually(ended(config, WorkflowId.value(hold))))
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

  /** Every table DBOS keeps a workflow's rows in, and the column naming the workflow. */
  private val DbosTables: Vector[(String, String)] = Vector(
    "workflow_status" -> "workflow_uuid",
    "workflow_input" -> "workflow_uuid",
    "workflow_output" -> "workflow_uuid",
    "operation_outputs" -> "workflow_uuid",
    "tx_step_outputs" -> "workflow_id",
    "streams" -> "workflow_uuid",
    "notifications" -> "destination_uuid",
    "workflow_events" -> "workflow_uuid",
    "workflow_events_history" -> "workflow_uuid"
  )

  /** How many rows each of [[DbosTables]] keeps for `ids`, by table. */
  private def dbosRows(config: DbConfig, ids: Vector[WorkflowId]): Vector[(String, Int)] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      DbosTables.map { (table, column) =>
        Using.resource(
          conn.prepareStatement(s"SELECT count(*) FROM dbos.$table WHERE $column = ANY(?)")
        ) { ps =>
          ps.setArray(
            1,
            conn.createArrayOf(
              "text",
              // A fresh array the driver only reads; separation checking treats arrays as mutable.
              caps.unsafe.unsafeAssumePure(ids.map(WorkflowId.value).toArray[AnyRef])
            )
          )
          (table, Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) }))
        }
      }
    }

  /** How many transaction-step outputs DBOS keeps for `ids`. */
  private def txOutputs(config: DbConfig, ids: Vector[WorkflowId]): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT count(*) FROM dbos.tx_step_outputs WHERE workflow_id = ANY(?)"
        )
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

  /** `text` ingested on `origin` and its turn run to its end. */
  private def turnOn(engine: Engine^, origin: Origin, text: String): TurnRef = {
    val t = engine.inbox
      .ingest(origin, SourceId(text), Message.User(text), Account.Local)
      .fold(e => sys.error(s"$e"), identity)
    engine.inbox.startTurn(t) ==> Right(())
    engine.awaitTurn(t)
    t
  }

  /** `period` closed by a sweep at `at`, waited for until its close has ended. */
  private def closeOf(
      engine: Engine^,
      config: DbConfig,
      period: PeriodRef,
      at: Instant
  ): CloseRef = {
    val swept = engine.sweep(at).map(_.enqueued)
    val attempt = swept.toOption
      .flatMap(_.find(_.period == period))
      .getOrElse(sys.error(s"no attempt on $period: $swept"))
    assert(
      eventually(
        LiveDb
          .transaction(config)(periods.get(period))
          .exists(_.exists(_.state != PeriodState.Open)) &&
          ended(config, WorkflowId.value(attempt.workflowId))
      )
    )
    attempt
  }

  /** Whether `period` has closed and its close has ended. */
  private def closed(config: DbConfig, period: PeriodRef): Boolean =
    LiveDb
      .transaction(config)(periods.get(period))
      .exists(_.exists(_.state != PeriodState.Open)) &&
      ended(config, s"close:${ConversationId.value(period.conversationId)}:")

  /** `period`'s close ordinal. */
  private def ordinalOf(config: DbConfig, period: PeriodRef): CloseOrdinal =
    LiveDb.transaction(config)(periods.get(period)) match {
      case Right(Some(p)) =>
        p.state match {
          case PeriodState.Closed(_, _, _, _, order, _) => order
          case PeriodState.Open => sys.error(s"$period is open")
        }
      case other => sys.error(s"$period: $other")
    }

  /** How many rows of `plugin`'s documents are kept, of any generation. */
  private def docRows(config: DbConfig, plugin: PluginName): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("SELECT count(*) FROM grit.plugin_docs WHERE plugin = ?")
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) })
      }
    }

  /** How many usage rows are kept for `entries`, and profiles and prompts for `turns`. */
  private def ledgered(
      config: DbConfig,
      entries: Vector[String],
      turns: Vector[WorkflowId]
  ): (Int, Int, Int) =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      def count(sql: String, keys: Vector[String]): Int =
        Using.resource(conn.prepareStatement(sql)) { ps =>
          ps.setString(1, ujson.Arr.from(keys.map(ujson.Str(_))).render())
          Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) })
        }
      (
        count(
          "SELECT count(*) FROM grit.usage_ledger WHERE entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))",
          entries
        ),
        count(
          "SELECT count(*) FROM grit.turn_model_profiles WHERE workflow_id IN (SELECT jsonb_array_elements_text(?::jsonb))",
          turns.map(WorkflowId.value)
        ),
        count(
          "SELECT count(*) FROM grit.turn_prompts WHERE workflow_id IN (SELECT jsonb_array_elements_text(?::jsonb))",
          turns.map(WorkflowId.value)
        )
      )
    }

  /** A usage row for `entry`, made for `turn` under `workflow`, and `turn`'s profile. */
  private def spent(config: DbConfig, entry: String, turn: TurnRef, workflow: WorkflowId): Unit = {
    val usage = Usage(Tokens(1), Tokens(1), Tokens(0), None)
    val ref = ModelRef(ModelId.of("a/m").getOrElse(sys.error("model")), None)
    val a = Assignment(ref, 100, None)
    LiveDb.transaction(config) {
      for {
        _ <- new SqlUsageLedger().record(EntryId(entry), turn, workflow, "m", usage, Tokens(1))
        _ <- new SqlModelProfileStore()
          .pin(turn.workflowId, Catalog.of(Policy(a, a, a, a), Vector.empty).pin)
        _ <- new grit.dbos.sql.SqlPromptStore().record(
          turn.workflowId,
          grit.core.prompt.SystemPrompt.of(
            Vector(grit.core.prompt.Fragment(grit.core.prompt.Layer.Base, "grit", "You are grit."))
          )
        )
      } yield ()
    } ==> Right(())
  }

  /** A decision to draft kept on `turn`, as triage keeps one (grit.speech: ledger). */
  private def decided(config: DbConfig, turn: TurnRef): Unit = {
    val p = grit.core.period.Probability.clamped(0.9)
    val tags = grit.core.triage.Tags.Weighed(
      grit.core.triage.Tags.V1.answers(grit.core.triage.Kind.Question, p, p, p, p),
      "jev",
      Usage(Tokens(1), Tokens(0), Tokens(0), None)
    )
    val heard = grit.core.speech.Heard(
      turn,
      EntrySeq(0),
      grit.core.place.Place.Everywhere,
      Instant.now(),
      grit.core.speech.Reach.Nowhere,
      tags
    )
    LiveDb.transaction(config)(
      new SqlSpeechStore().decided(heard, grit.core.speech.Decision.Drafting(turn), Instant.now())
    ) ==> Right(true)
  }

  /** Whether `conversation` is kept, and how many places are. */
  private def kept(config: DbConfig, conversation: ConversationId): (Boolean, Int) =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      def count(sql: String): Int =
        Using.resource(conn.prepareStatement(sql)) { ps =>
          Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) })
        }
      (
        count(
          s"SELECT count(*) FROM grit.conversations WHERE id = '${ConversationId.value(conversation)}'"
        ) == 1,
        count("SELECT count(*) FROM grit.places")
      )
    }

  private def launched(
      config: DbConfig,
      ledger: FiniteDuration,
      plugins: Vector[Plugin] = Vector.empty
  ): Engine^ = {
    val engine = LiveEngine.open(config, "test")
    engine.launch(
      turn,
      close,
      (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
      (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
      (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
      LiveEngine.Unplaced,
      plugins
    )
    minutes(config, ledger)
    engine
  }

  private def tui(session: String): Origin =
    Origin.Tui(Directory.of("/work/shared").getOrElse(sys.error("directory")), session)

  /** A plugin that keeps nothing. */
  private final class Idle(val name: PluginName) extends Plugin {
    val version: Int = 1
    override val cache: Option[CachePosting] = Some(new CachePosting {
      def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
        Right(())
    })
  }

  /** Documents kept two minutes once no longer current. */
  private val Terms: DocumentTerms =
    (for {
      label <- DocLabel.of("notes")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 2.minutes, 10)
    } yield terms).getOrElse(sys.error("terms"))

  /** A plugin with documents under [[Terms]] that it never posts to. */
  private final class Keeping(val name: PluginName) extends Plugin {
    val version: Int = 1
    override val documents: Option[Documents] = Some(new Documents with DocumentPosting {
      val posting: Option[DocumentPosting] = Some(this)
      val terms: DocumentTerms = Terms
      def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] =
        Right(())
    })
  }

  private val documents = new SqlDocuments(tombstones)

  /** `texts` written in turn under one key of `plugin`'s at `at`: their versions. */
  private def wrote(
      config: DbConfig,
      plugin: PluginName,
      at: Instant,
      texts: String*
  ): Vector[DocumentVersion] = {
    val keeper = documents.keeper(plugin, Terms)
    val key = DocKey.of("k").getOrElse(sys.error("key"))
    val here = Place.under(Namespace.Task, Vector("collect"))
    texts.toVector.map { t =>
      val text = DocText.of(t).getOrElse(sys.error("text"))
      LiveDb.transaction(config, Clearance.of(Label.Public))(
        keeper.write(key, Label.Public, here, text, ujson.Obj(), at)
      ) match {
        case Right(Written.Versioned(v, _, _)) => v
        case other => sys.error(s"writing $t: $other")
      }
    }
  }

  private def enqueue(config: DbConfig, runs: Vector[PostRef]): Unit = {
    val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
    try
      runs.foreach { run =>
        val _ = client.enqueueWorkflow[String, Exception](
          Posts.enqueueOptions(run),
          // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
          caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
        )
      }
    finally client.close()
  }

  /** A morning the schedule tests declare from: no clock reads, and every window measured from
    * it.
    */
  private val Morning = Instant.parse("2026-10-07T08:00:00Z")

  /** The deployment's daily schedules of one job, declared and read through the SQL store. */
  private final class Schedules(config: DbConfig) {
    private val store = new SqlSchedules(tombstones)
    private val job = new JobTests.Counting("standup")

    def id(key: String): ScheduleId =
      ScheduleId.declared(Declarer.Deployment, ScheduleKey.of(key).fold(sys.error, identity))

    /** Exactly the schedules `keys` declared, at `at`. */
    def declare(at: Instant, keys: String*): Unit =
      LiveDb.transaction(config)(
        store.declare(
          keys.toVector.map(k =>
            (
              Declarer.Deployment,
              Declared(
                ScheduleKey.of(k).fold(sys.error, identity),
                job,
                SlotRule.Daily(LocalTime.of(9, 0), ZoneOffset.UTC),
                JobTests.Count(1)
              )
            )
          ),
          at
        )
      ) ==> Right(())

    /** How `key`'s schedule ended, if it has; `None` once its row is gone. */
    def ended(key: String): Option[Option[Ending]] =
      LiveDb.transaction(config)(store.read(id(key))).fold(e => sys.error(s"$e"), _.map(_.ended))
  }

  val tests = Tests {
    test(
      "a raw collection deletes a period's raw entries and workflows, keeping its closing entry and row"
    ) {
      val config = TestPostgres.freshDatabase("collect_raw")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          LiveEngine.Unplaced,
          Vector.empty
        )
        minutes(config)
        val origin = Origin.Task("retention", "purge")
        val t0 = engine.inbox
          .ingest(origin, SourceId("one"), Message.User("one"), Account.Local)
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
        // The turn's workflow and its one step.
        kept(config, Vector(t0.workflowId)) ==> Vector(1, 1)
        // Where DBOS 1.2 keeps them: the status, its input and output, the step, twice.
        dbosRows(config, Vector(t0.workflowId)).filter(_._2 > 0) ==> Vector(
          "workflow_status" -> 1,
          "workflow_input" -> 1,
          "workflow_output" -> 1,
          "operation_outputs" -> 1,
          "tx_step_outputs" -> 1
        )
        kept(config, workflows).map(_ > 0) ==> Vector(true, true)

        // As if a sweep deleted the close's workflow and died before the rest: the next one
        // deletes it again, which DBOS takes as nothing to do.
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try client.deleteWorkflows(java.util.List.of(WorkflowId.value(attempt.workflowId)), false)
        finally client.close()

        spent(config, "u0", t0, t0.workflowId)
        val later = Instant.now().plusSeconds(180)
        engine.sweep(later).map(_.collected) ==> Right(Vector(Target.Raw(p1)))
        // Usage goes with the closing, not the raw entries.
        ledgered(config, Vector("u0"), Vector(t0.workflowId)) ==> (1, 1, 1)
        // Nothing of either workflow is left in any of DBOS's tables.
        dbosRows(config, workflows).filter(_._2 > 0) ==> Vector()
        kept(config, Vector(t0.workflowId)) ==> Vector(0, 0)
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
      "collecting a period's raw entries leaves no transaction-step output of its workflows, even one a sweep that died after deleting the workflow left"
    ) {
      val config = TestPostgres.freshDatabase("collect_tx_outputs")
      val engine = launched(config, 1.day)
      try {
        val t0 = turnOn(engine, Origin.Task("retention", "tx-outputs"), "one")
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        val attempt = closeOf(engine, config, p1, Instant.now().plusSeconds(120))
        // The close's seal is a transaction step: DBOS keeps its output in tx_step_outputs too.
        txOutputs(config, Vector(attempt.workflowId)) ==> 1
        // So is the turn's, so the sweep is seen reclaiming a turn's as well as a close's.
        txOutputs(config, Vector(t0.workflowId)) ==> 1

        // As if a sweep deleted the close's workflow and died before its rows: the next sweep
        // no longer finds the workflow, and must still reclaim its step outputs.
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try client.deleteWorkflows(java.util.List.of(WorkflowId.value(attempt.workflowId)), false)
        finally client.close()

        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Raw(p1)))
        txOutputs(config, Vector(t0.workflowId, attempt.workflowId)) ==> 0
      } finally engine.close()
    }

    test(
      "a raw collection finds its period's close attempts, questions, triages, shadows and placements by their ids' prefix, and not period 10's or another turn's; a heard turn, which ran no turn workflow, is no hindrance"
    ) {
      val config = TestPostgres.freshDatabase("collect_prefix")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          LiveEngine.Unplaced,
          Vector.empty
        )
        minutes(config)
        val t0 = engine.inbox
          .ingest(
            Origin.Task("retention", "prefix"),
            SourceId("one"),
            Message.User("one"),
            Account.Local
          )
          .fold(e => sys.error(s"$e"), identity)
        // Turn 1 is heard: its triage runs, and no turn workflow ever does.
        engine.inbox.hear(
          Origin.Task("retention", "prefix"),
          SourceId("two"),
          "heard",
          Account.Local,
          java.time.Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==> Right(())
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        val triaged = TriageRef(p1, TurnSeq(1)).workflowId
        assert(
          eventually(
            ended(config, WorkflowId.value(triaged)) && kept(config, Vector(triaged)) == Vector(
              1,
              0
            )
          )
        )
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
        // Workflows named as close attempts, questions, triages and shadows on period 1 and on
        // period 10 would be, whatever their turns, and placements of period 1's turn 0 and of
        // a turn after it, or of another conversation: the stand-ins run nothing for any.
        val c = ConversationId.value(t0.conversationId)
        val one = Vector(
          WorkflowId(s"close:$c:1:stray"),
          WorkflowId(s"settle:$c:1:stray"),
          WorkflowId(s"triage:$c:1:stray"),
          WorkflowId(s"shadow:$c:1:1:stray"),
          WorkflowId(s"stitch:0000000000001:$c:0")
        )
        val ten = Vector(
          WorkflowId(s"close:$c:10:stray"),
          WorkflowId(s"settle:$c:10:stray"),
          WorkflowId(s"triage:$c:10:stray"),
          WorkflowId(s"shadow:$c:10:1:stray"),
          WorkflowId(s"stitch:0000000000001:$c:2"),
          WorkflowId(s"stitch:0000000000001:other:0")
        )
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try
          (one ++ ten).foreach { id =>
            val name = WorkflowId.value(id).takeWhile(_ != ':')
            // A shadow runs on its own queue, which is not partitioned.
            val options =
              if (name == "shadow")
                new EnqueueOptions(name, DurableWorkflow.ClassName, QueueName.of("shadows"))
                  .withWorkflowId(WorkflowId.value(id))
              else if (name == "stitch")
                new EnqueueOptions(name, DurableWorkflow.ClassName, QueueName.of("stitches"))
                  .withWorkflowId(WorkflowId.value(id))
                  .withQueuePartitionKey("room")
              else
                new EnqueueOptions(name, DurableWorkflow.ClassName, QueueName.of("turns"))
                  .withWorkflowId(WorkflowId.value(id))
                  .withQueuePartitionKey(c)
            val _ = client.enqueueWorkflow[String, Exception](
              options,
              // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
              caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
            )
          }
        finally client.close()
        assert(eventually(kept(config, one) == Vector(5, 0)))
        assert(eventually(kept(config, ten) == Vector(6, 0)))
        // The collector waits for a workflow still running; these all end at once.
        assert(
          eventually(
            ended(config, s"close:$c:") && ended(config, s"settle:$c:") &&
              ended(config, s"triage:$c:") && ended(config, s"shadow:$c:") &&
              ended(config, "stitch:")
          )
        )
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==> Right(
          Vector(Target.Raw(p1))
        )
        (kept(config, one :+ triaged), kept(config, ten)) ==> (Vector(0, 0), Vector(6, 0))
      } finally engine.close()
    }

    test("a tombstone whose workflow is still running is deferred, and collected once it ends") {
      val config = TestPostgres.freshDatabase("collect_deferred")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          LiveEngine.Unplaced,
          Vector.empty
        )
        minutes(config)
        val t0 = engine.inbox
          .ingest(
            Origin.Task("collect", "deferred"),
            SourceId("one"),
            Message.User("one"),
            Account.Local
          )
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
            new EnqueueOptions("close", DurableWorkflow.ClassName, QueueName.of("turns"))
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
        } finally client.close()
        release(config, hold)
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Raw(p1)))
        kept(config, Vector(hold)) ==> Vector(0, 0)
      } finally engine.close()
    }

    test(
      "a superseded closing goes with its row, its turns' usage, speech decisions, profiles and prompts and its close's cost; the latest stays"
    ) {
      val config = TestPostgres.freshDatabase("collect_superseded")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          LiveEngine.Unplaced,
          Vector.empty
        )
        minutes(config, ledger = 2.minutes)
        val origin = Origin.Task("collect", "superseded")
        val t0 = turnOn(engine, origin, "one")
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        val a1 = closeOf(engine, config, p1, Instant.now().plusSeconds(120))
        spent(config, "u0", t0, t0.workflowId)
        spent(config, EntryId.value(p1.closingId), t0, a1.workflowId)
        val t1 = turnOn(engine, origin, "two")
        val p2 = PeriodRef(t0.conversationId, PeriodSeq.First.next)
        // This sweep collects period 1's raw entries, past the raw window by now.
        val a2 = closeOf(engine, config, p2, Instant.now().plusSeconds(240))
        spent(config, "u1", t1, t1.workflowId)
        spent(config, EntryId.value(p2.closingId), t1, a2.workflowId)
        Vector(t0, t1).foreach(decided(config, _))
        // A third period open: the conversation is not quiet. Idle for an hour from here, so
        // no sweep below makes it due.
        minutes(config, ledger = 2.minutes, idle = 1.hour)
        turnOn(engine, origin, "three")
        // A plugin's documents, one posted from each closing.
        val cached = PluginName.of("cached").getOrElse(sys.error("name"))
        LiveDb.transaction(config) {
          for {
            _ <- new SqlPluginCursors(tombstones).start(cached, 1, Instant.now())
            _ <- new SqlCacheDocs(cached, ordinalOf(config, p1)).put("one", ujson.Str("one"))
            _ <- new SqlCacheDocs(cached, ordinalOf(config, p2)).put("two", ujson.Str("two"))
          } yield ()
        } ==> Right(())

        // The third period stays open: a close of it would race the reads below.
        engine
          .sweep(Instant.now().plusSeconds(600))
          .map(s => (s.collected, s.enqueued.map(_.period))) ==> Right(
          (Vector(Target.Raw(p2), Target.Superseded(p1)), Vector.empty)
        )
        LiveDb.transaction(config)(periods.get(p1)) ==> Right(None)
        LiveDb.transaction(config)(periods.get(p2)).map(_.map(_.ref)) ==> Right(Some(p2))
        LiveDb
          .transaction(config)(new SqlEntryStore().list(t0.conversationId))
          .map(_.map(e => EntryId.value(e.id))) ==>
          Right(
            Vector(
              EntryId.value(p2.closingId),
              s"in:${ConversationId.value(t0.conversationId)}:three"
            )
          )
        ledgered(config, Vector("u0", EntryId.value(p1.closingId)), Vector(t0.workflowId)) ==> (
          0,
          0,
          0
        )
        ledgered(config, Vector("u1", EntryId.value(p2.closingId)), Vector(t1.workflowId)) ==> (
          2,
          1,
          1
        )
        LiveDb.transaction(config)(new SqlPluginDocs(cached).newest("", 10)).map(_.map(_._1)) ==>
          Right(Vector("two"))
        LiveDb
          .transaction(config)(new SqlSpeechStore().spoken(Instant.EPOCH))
          .map(_.map(_.turn).filter(_.conversationId == t0.conversationId)) ==> Right(Vector(t1))
      } finally engine.close()
    }

    test(
      "a conversation quiet past the ledger window is removed whole, its place with it once no other conversation is there"
    ) {
      val config = TestPostgres.freshDatabase("collect_quiet")
      val engine = launched(config, 2.minutes)
      try {
        val quiet = turnOn(engine, tui("quiet"), "one")
        // Another session in the same directory: the same place.
        val busy = turnOn(engine, tui("busy"), "two")
        val p = PeriodRef(quiet.conversationId, PeriodSeq.First)
        val q1 = PeriodRef(busy.conversationId, PeriodSeq.First)
        closeOf(engine, config, p, Instant.now().plusSeconds(120))
        assert(eventually(closed(config, q1)))
        spent(config, "u-quiet", quiet, quiet.workflowId)
        // Busy again: its next period is open.
        turnOn(engine, tui("busy"), "three")
        kept(config, quiet.conversationId) ==> (true, 1)

        // The two periods closed concurrently, so their raw tombstones come in either order.
        engine
          .sweep(Instant.now().plusSeconds(600))
          .map(s => (s.collected.take(2).toSet, s.collected.drop(2), s.spared)) ==>
          Right(
            (
              Set(Target.Raw(p), Target.Raw(q1)),
              Vector(Target.Quiet(p)),
              Vector(Target.Quiet(q1))
            )
          )
        kept(config, quiet.conversationId) ==> (false, 1)
        kept(config, busy.conversationId) ==> (true, 1)
        ledgered(config, Vector("u-quiet"), Vector(quiet.workflowId)) ==> (0, 0, 0)

        val q2 = PeriodRef(busy.conversationId, PeriodSeq.First.next)
        // That sweep found it due, and enqueued its close.
        assert(eventually(closed(config, q2)))
        engine.sweep(Instant.now().plusSeconds(1500)).map(_.collected) ==>
          Right(Vector(Target.Raw(q2), Target.Superseded(q1), Target.Quiet(q2)))
        kept(config, busy.conversationId) ==> (false, 0)
      } finally engine.close()
    }

    test("a quiet conversation waits while an earlier period's raw entries are kept") {
      val config = TestPostgres.freshDatabase("collect_quiet_waits")
      val engine = launched(config, 2.minutes)
      try {
        val origin = Origin.Task("collect", "waits")
        val t0 = turnOn(engine, origin, "one")
        val c = ConversationId.value(t0.conversationId)
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        closeOf(engine, config, p1, Instant.now().plusSeconds(120))
        // A workflow named as an attempt on period 1, running through the sweeps below.
        val hold = WorkflowId(s"close:$c:1:hold")
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try {
          val _ = client.enqueueWorkflow[String, Exception](
            new EnqueueOptions("close", DurableWorkflow.ClassName, QueueName.of("turns"))
              .withWorkflowId(WorkflowId.value(hold))
              .withQueuePartitionKey(c + ":hold"),
            // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
            caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
          )
          assert(eventually(kept(config, Vector(hold)).headOption.contains(1)))
          turnOn(engine, origin, "two")
          val p2 = PeriodRef(t0.conversationId, PeriodSeq.First.next)
          closeOf(engine, config, p2, Instant.now().plusSeconds(120))
          engine.sweep(Instant.now().plusSeconds(600)).map(s => (s.collected, s.deferred)) ==>
            Right(
              (
                Vector(Target.Raw(p2)),
                Vector(Target.Raw(p1), Target.Superseded(p1), Target.Quiet(p2))
              )
            )
          kept(config, t0.conversationId)._1 ==> true
          release(config, hold)
          assert(eventually(ended(config, s"close:$c:")))
          engine.sweep(Instant.now().plusSeconds(600)).map(_.collected) ==>
            Right(Vector(Target.Raw(p1), Target.Superseded(p1), Target.Quiet(p2)))
        } finally client.close()
        kept(config, t0.conversationId)._1 ==> false
        kept(config, Vector(hold)) ==> Vector(0, 0)
      } finally engine.close()
    }

    test("an ended schedule's row is deleted once its tombstone is due, and not before") {
      val config = TestPostgres.freshDatabase("collect_schedule")
      val engine = launched(config, 1.day)
      try {
        val s = new Schedules(config)
        s.declare(Morning, "gone", "kept")
        s.declare(Morning.plusSeconds(10), "kept")
        s.ended("gone") ==> Some(Some(Ending.Undeclared))
        engine.sweep(Morning.plusSeconds(30)).map(_.collected) ==> Right(Vector())
        s.ended("gone") ==> Some(Some(Ending.Undeclared))
        engine.sweep(Morning.plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Schedule(s.id("gone"))))
        s.ended("gone") ==> None
        s.ended("kept") ==> Some(None)
      } finally engine.close()
    }

    test(
      "a schedule revived before its tombstone is due is spared, and ending it again marks it again"
    ) {
      val config = TestPostgres.freshDatabase("collect_schedule_revived")
      val engine = launched(config, 1.day)
      try {
        val s = new Schedules(config)
        s.declare(Morning, "back")
        s.declare(Morning.plusSeconds(10))
        s.declare(Morning.plusSeconds(20), "back")
        engine.sweep(Morning.plusSeconds(180)).map(w => (w.collected, w.spared)) ==>
          Right((Vector(), Vector(Target.Schedule(s.id("back")))))
        s.ended("back") ==> Some(None)
        s.declare(Morning.plusSeconds(300))
        engine.sweep(Morning.plusSeconds(330)).map(_.collected) ==> Right(Vector())
        engine.sweep(Morning.plusSeconds(400)).map(_.collected) ==>
          Right(Vector(Target.Schedule(s.id("back"))))
        s.ended("back") ==> None
      } finally engine.close()
    }

    test(
      "a restarted plugin's earlier documents and its runs at the old version go after the raw window"
    ) {
      val config = TestPostgres.freshDatabase("collect_restarted")
      val engine = launched(config, 2.minutes)
      try {
        val p = PluginName.of("restarted").getOrElse(sys.error("name"))
        val cursors = new SqlPluginCursors(tombstones)
        LiveDb.transaction(config) {
          for {
            _ <- cursors.start(p, 1, Instant.now())
            _ <- new SqlCacheDocs(p, CloseOrdinal.Start).put("old", ujson.Str("v1"))
          } yield ()
        } ==> Right(())
        val old = PostRef(p, 1, CloseOrdinal.Start, 0)
        val current = PostRef(p, 2, CloseOrdinal.Start, 0)
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try
          Vector(old, current).foreach { run =>
            val _ = client.enqueueWorkflow[String, Exception](
              Posts.enqueueOptions(run),
              // Empty, and DBOS only reads it; separation checking treats arrays as mutable.
              caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
            )
          }
        finally client.close()
        assert(
          eventually(
            ended(config, s"post:${PluginName.value(p)}:") &&
              kept(config, Vector(old.workflowId, current.workflowId)).headOption.contains(2)
          )
        )
        LiveDb.transaction(config) {
          for {
            _ <- cursors.start(p, 2, Instant.now())
            _ <- new SqlCacheDocs(p, CloseOrdinal.Start).put("new", ujson.Str("v2"))
          } yield ()
        } ==> Right(())
        docRows(config, p) ==> 2
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Restarted(p)))
        docRows(config, p) ==> 1
        LiveDb.transaction(config)(new SqlPluginDocs(p).newest("", 10)).map(_.map(_._1)) ==>
          Right(Vector("new"))
        (
          kept(config, Vector(old.workflowId)).headOption,
          kept(config, Vector(current.workflowId)).headOption
        ) ==> (Some(0), Some(1))
      } finally engine.close()
    }

    test(
      "the finished runs from a cursor passed go after the raw window, and those from another stay"
    ) {
      val config = TestPostgres.freshDatabase("collect_post_runs")
      val engine = launched(config, 2.minutes)
      try {
        val p = PluginName.of("posted").getOrElse(sys.error("name"))
        val five = CloseOrdinal.of(5).getOrElse(sys.error("ordinal"))
        val passed = Vector(0, 1).map(PostRef(p, 1, CloseOrdinal.Start, _))
        val current = PostRef(p, 1, five, 0)
        enqueue(config, passed :+ current)
        assert(
          eventually(
            ended(config, PostRef.prefix(p)) && kept(
              config,
              passed :+ current map (_.workflowId)
            ).headOption.contains(3)
          )
        )
        LiveDb.transaction(config)(
          tombstones.write(Target.PostRuns(p, 1, CloseOrdinal.Start), Instant.now())
        ) ==>
          Right(true)
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.PostRuns(p, 1, CloseOrdinal.Start)))
        (
          kept(config, passed.map(_.workflowId)).headOption,
          kept(config, Vector(current.workflowId)).headOption
        ) ==>
          (Some(0), Some(1))
      } finally engine.close()
    }

    test("a plugin not enabled loses its documents, cursor and runs after the ledger window") {
      val config = TestPostgres.freshDatabase("collect_disabled")
      val engine = launched(config, 2.minutes)
      try {
        val p = PluginName.of("gone").getOrElse(sys.error("name"))
        LiveDb.transaction(config) {
          for {
            _ <- new SqlPluginCursors(tombstones).start(p, 1, Instant.now())
            _ <- new SqlCacheDocs(p, CloseOrdinal.Start).put("k", ujson.Str("v"))
          } yield ()
        } ==> Right(())
        val run = PostRef(p, 1, CloseOrdinal.Start, 0)
        enqueue(config, Vector(run))
        assert(
          eventually(
            ended(config, PostRef.prefix(p)) && kept(config, Vector(run.workflowId)).headOption
              .contains(1)
          )
        )
        engine.sweep(Instant.now()).map(s => (s.disabled, s.collected)) ==> Right(
          (Vector(p), Vector())
        )
        engine.sweep(Instant.now()).map(_.disabled) ==> Right(Vector())
        engine.sweep(Instant.now().plusSeconds(180)).map(_.collected) ==>
          Right(Vector(Target.Disabled(p)))
        docRows(config, p) ==> 0
        LiveDb.transaction(config)(new SqlPluginCursors(tombstones).stored()) ==> Right(Vector())
        kept(config, Vector(run.workflowId)).headOption ==> Some(0)
      } finally engine.close()
    }

    test("a plugin's version no longer current goes its declared retention after, not before") {
      val config = TestPostgres.freshDatabase("collect_document")
      val p = PluginName.of("keeping").getOrElse(sys.error("name"))
      val engine = launched(config, 1.day, Vector(new Keeping(p)))
      try {
        val now = Instant.now()
        val versions = wrote(config, p, now, "first", "second")
        engine.sweep(now.plusSeconds(60)).map(_.collected) ==> Right(Vector())
        engine.sweep(now.plusSeconds(180)).map(_.collected) ==>
          Right(versions.take(1).map(Target.Document(_)))
        LiveDb.transaction(config)(documents.read(versions)).map(_.map(_.version)) ==>
          Right(versions.drop(1))
      } finally engine.close()
    }

    test(
      "a plugin not enabled loses its documents, terms and their tombstones after the ledger window"
    ) {
      val config = TestPostgres.freshDatabase("collect_disabled_documents")
      val p = PluginName.of("unkept").getOrElse(sys.error("name"))
      // Declared by an earlier start; this engine starts without it, and it has no cursor.
      LiveEngine.open(config, "test").close()
      LiveDb.transaction(config)(documents.declare(Vector(p -> Terms))) ==> Right(())
      val now = Instant.now()
      val versions = wrote(config, p, now, "first", "second")
      val engine = launched(config, 2.minutes)
      try {
        engine.sweep(now).map(s => (s.disabled, s.collected)) ==> Right((Vector(p), Vector()))
        engine.sweep(now.plusSeconds(180)).map(_.collected) ==> Right(Vector(Target.Disabled(p)))
        LiveDb.transaction(config)(documents.read(versions)) ==> Right(Vector())
        LiveDb.transaction(config)(documents.kept()) ==> Right(Vector())
        LiveDb.transaction(config)(
          tombstones.due(Target.Kind.Document, now.plusSeconds(3600), 10)
        ) ==> Right(Vector())
      } finally engine.close()
    }

    test("a plugin enabled again before the ledger window keeps its documents") {
      val config = TestPostgres.freshDatabase("collect_enabled_documents")
      val p = PluginName.of("back-again").getOrElse(sys.error("name"))
      val now = Instant.now()
      val first = launched(config, 2.minutes)
      val versions =
        try {
          val written = wrote(config, p, now, "kept")
          first.sweep(now).map(_.disabled) ==> Right(Vector(p))
          written
        } finally first.close()
      val again = launched(config, 2.minutes, Vector(new Keeping(p)))
      try {
        again.sweep(now.plusSeconds(180)).map(_.collected) ==> Right(Vector())
        LiveDb.transaction(config)(documents.read(versions)).map(_.map(_.version)) ==>
          Right(versions)
        LiveDb.transaction(config)(
          tombstones.due(Target.Kind.Disabled, now.plusSeconds(3600), 10)
        ) ==> Right(Vector())
      } finally again.close()
    }

    test("an enabled plugin's disabled tombstone is spared") {
      val config = TestPostgres.freshDatabase("collect_enabled")
      val p = PluginName.of("back").getOrElse(sys.error("name"))
      val engine = launched(config, 2.minutes, Vector(new Idle(p)))
      try {
        LiveDb.transaction(config)(tombstones.write(Target.Disabled(p), Instant.now())) ==> Right(
          true
        )
        engine.sweep(Instant.now().plusSeconds(180)).map(s => (s.spared, s.collected)) ==>
          Right((Vector(), Vector()))
        LiveDb.transaction(config)(
          tombstones.due(Target.Kind.Disabled, Instant.now().plusSeconds(3600), 10)
        ) ==>
          Right(Vector())
      } finally engine.close()
    }
  }
}
