package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.durable.Durable
import grit.core.id.{
  ConversationId,
  EntryId,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  ShadowName,
  ShadowRef,
  SourceId,
  TriageRef,
  WorkflowId
}
import grit.core.message.{Tokens, Usage}
import grit.core.spend.DailyCap
import grit.core.store.Origin
import grit.core.triage.{Shadowed, Shadowing, Tags}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlEntryStore,
  SqlTriageShadows,
  SqlTriageStore,
  TestPostgres
}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.ListWorkflowsInput
import utest.*

/** Shadows over DBOS against a real Postgres, with stand-in bodies: what the sweep enqueues
  * for a declared variant, on which queue, and when it enqueues nothing; not what a shadow
  * asks.
  */
object ShadowLiveTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private val Words: ShadowName =
    ShadowName.of("words").getOrElse(throw new java.lang.AssertionError("a name"))

  private def cap(usd: String): DailyCap =
    DailyCap.of(usd).getOrElse(throw new java.lang.AssertionError(usd))

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  /** `texts` heard in `here` as its first period's turns, in order, each tagged a second
    * after the one before from `at`: their entries and triages.
    */
  private def heardAndTagged(
      engine: Engine^,
      config: DbConfig,
      here: Origin,
      texts: Vector[String],
      at: Instant
  ): Vector[(EntryId, TriageRef)] = {
    texts.zipWithIndex.foreach { (text, i) =>
      engine.inbox.hear(
        here,
        SourceId(s"m$i"),
        text,
        PrincipalId.Local,
        Instant.now(),
        grit.core.speech.Reach.Nowhere
      ) ==> Right(())
    }
    val c = LiveDb.conversation(config, here).id
    val heard = LiveDb.transaction(config)(new SqlEntryStore().list(c)).getOrElse(Vector.empty)
    heard.zipWithIndex.map { (e, i) =>
      LiveDb.transaction(config)(
        new SqlTriageStore().record(e.id, Tags.Unanswered("stand-in"), at.plusSeconds(i.toLong))
      ) ==> Right(true)
      (e.id, TriageRef(PeriodRef(c, PeriodSeq.First), e.turnSeq))
    }
  }

  private def queues(config: DbConfig, c: ConversationId): Vector[String] = {
    val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
    try
      client
        .listWorkflows(
          new ListWorkflowsInput().withWorkflowIdPrefix(s"shadow:${ConversationId.value(c)}:")
        )
        .asScala
        .toVector
        .map(w => Option(w.queueName()).getOrElse("none"))
    finally client.close()
  }

  val tests = Tests {
    test(
      "the sweep enqueues a variant's unshadowed messages, oldest tagged first, on the shadows queue, and none while one is queued or running"
    ) {
      val config = TestPostgres.freshDatabase("shadow_enqueue")
      val ran = new ConcurrentLinkedQueue[String]()
      val started = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      def shadow(id: WorkflowId)(using d: Durable^): String = {
        ran.add(WorkflowId.value(id))
        started.countDown()
        release.await(30, TimeUnit.SECONDS)
        "shadowed"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          nothing,
          nothing,
          nothing,
          nothing,
          nothing,
          LiveEngine.Unplaced,
          Vector.empty,
          shadow,
          Vector(Shadowing(Words, Instant.EPOCH, cap("1")))
        )
        val here = Origin.Task("shadow", "enqueue")
        val heard =
          heardAndTagged(engine, config, here, Vector("one", "two", "three"), Instant.now())
        val shadows = heard.map((_, t) => ShadowRef(t, Words))
        engine.sweep(Instant.now()).map(_.shadowed) ==> Right(shadows)
        assert(started.await(30, TimeUnit.SECONDS))
        // One runs, the others wait behind it: the variant enqueues nothing more meanwhile, not
        // even a message tagged since.
        val later = heardAndTagged(
          engine,
          config,
          Origin.Task("shadow", "later"),
          Vector("four"),
          Instant.now().plusSeconds(10)
        )
        engine.sweep(Instant.now()).map(s => (s.shadowed, s.stuck)) ==>
          Right((Vector.empty, Vector.empty))
        release.countDown()
        assert(eventually(engine.unfinished() == Right(0)))
        // Once they have ended, the later one is enqueued.
        engine.sweep(Instant.now()).map(_.shadowed) ==>
          Right(later.map((_, t) => ShadowRef(t, Words)))
        assert(eventually(engine.unfinished() == Right(0)))
        ran.asScala.toVector ==>
          (shadows ++ later.map((_, t) => ShadowRef(t, Words))).map(s =>
            WorkflowId.value(s.workflowId)
          )
        queues(
          config,
          heard.headOption.fold(ConversationId("none"))(_._2.period.conversationId)
        ) ==>
          Vector("shadows", "shadows", "shadows")
        // Each ran once and kept nothing: stuck, never enqueued again.
        engine.sweep(Instant.now()).map(s => (s.shadowed, s.stuck.size)) ==> Right(
          (Vector.empty, 4)
        )
      } finally engine.close()
    }

    test(
      "a shadow that ended keeping nothing gives up its place: the next sweeps enqueue the messages after it, and report it stuck"
    ) {
      val config = TestPostgres.freshDatabase("shadow_stuck")
      val engine = LiveEngine.open(config, "test")
      try {
        // $0.0004 at the first-call estimate of $0.0002 covers two a sweep.
        engine.launch(
          nothing,
          nothing,
          nothing,
          nothing,
          nothing,
          LiveEngine.Unplaced,
          Vector.empty,
          nothing,
          Vector(Shadowing(Words, Instant.EPOCH, cap("0.0004")))
        )
        val heard = heardAndTagged(
          engine,
          config,
          Origin.Task("shadow", "stuck"),
          Vector("one", "two", "three", "four", "five"),
          Instant.now()
        )
        val shadows = heard.map((_, t) => ShadowRef(t, Words))
        engine.sweep(Instant.now()).map(_.shadowed) ==> Right(shadows.take(2))
        assert(eventually(engine.unfinished() == Right(0)))
        // Both ended keeping no row: skipped, and the next two enqueued in their place.
        engine.sweep(Instant.now()).map(s => (s.shadowed, s.stuck)) ==>
          Right((shadows.slice(2, 4), shadows.take(2).map(_.workflowId)))
        assert(eventually(engine.unfinished() == Right(0)))
        engine.sweep(Instant.now()).map(s => (s.shadowed, s.stuck)) ==>
          Right((shadows.drop(4), shadows.take(4).map(_.workflowId)))
      } finally engine.close()
    }

    test(
      "a shadow that throws leaves its message's triage as it was, and the next sweep moves past it"
    ) {
      val config = TestPostgres.freshDatabase("shadow_throws")
      val engine = LiveEngine.open(config, "test")
      def throws(id: WorkflowId)(using d: Durable^): String =
        throw new IllegalStateException(s"no classifier for ${WorkflowId.value(id)}")
      try {
        // $0.0002 at the first-call estimate covers one a sweep.
        engine.launch(
          nothing,
          nothing,
          nothing,
          nothing,
          nothing,
          LiveEngine.Unplaced,
          Vector.empty,
          throws,
          Vector(Shadowing(Words, Instant.EPOCH, cap("0.0002")))
        )
        val heard = heardAndTagged(
          engine,
          config,
          Origin.Task("shadow", "throws"),
          Vector("one", "two"),
          Instant.now()
        )
        val entries = heard.map(_._1)
        def tags = LiveDb.transaction(config)(new SqlTriageStore().of(entries))
        val before = tags
        val shadows = heard.map((_, t) => ShadowRef(t, Words))
        engine.sweep(Instant.now()).map(_.shadowed) ==> Right(shadows.take(1))
        assert(eventually(engine.unfinished() == Right(0)))
        (tags, before.map(_.size)) ==> (before, Right(2))
        engine.sweep(Instant.now()).map(s => (s.shadowed, s.stuck)) ==>
          Right((shadows.drop(1), shadows.take(1).map(_.workflowId)))
      } finally engine.close()
    }

    test(
      "a variant whose day's cap is spent gets nothing enqueued, and its messages wait for a day the cap covers"
    ) {
      val config = TestPostgres.freshDatabase("shadow_capped")
      val engine = LiveEngine.open(config, "test")
      val now = Instant.parse("2031-03-04T12:00:00Z")
      try {
        engine.launch(
          nothing,
          nothing,
          nothing,
          nothing,
          nothing,
          LiveEngine.Unplaced,
          Vector.empty,
          nothing,
          Vector(Shadowing(Words, Instant.EPOCH, cap("0.01")))
        )
        val heard = heardAndTagged(
          engine,
          config,
          Origin.Task("shadow", "capped"),
          Vector("one", "two", "three"),
          Instant.parse("2031-03-01T00:00:00Z")
        )
        // Today's spend so far is the whole cap: one answered call at $0.006, then one at $0.004.
        def answered(usd: String) = Shadowed.Answered(
          "d1g35t",
          Vector.empty,
          Usage(Tokens(800), Tokens(0), Tokens.Zero, Some(BigDecimal(usd))),
          "jev",
          "jev",
          1.second
        )
        val Vector((first, _), (second, _), (_, third)) = heard: @unchecked
        LiveDb.transaction(config) {
          val shadows = new SqlTriageShadows
          for {
            _ <- shadows.record(first, Words, answered("0.006"), now.minusSeconds(3600))
            _ <- shadows.record(second, Words, answered("0.004"), now.minusSeconds(60))
          } yield ()
        } ==> Right(())
        engine.sweep(now).map(_.shadowed) ==> Right(Vector.empty)
        // The next UTC day: $0.01 left at $0.005 a call covers two, and one is left.
        engine.sweep(now.plusSeconds(86_400)).map(_.shadowed) ==> Right(
          Vector(ShadowRef(third, Words))
        )
      } finally engine.close()
    }
  }
}
