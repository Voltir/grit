package grit.app.main

import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.{Clock, SetClock}
import grit.core.id.{
  ConversationId,
  Declarer,
  JobName,
  PluginName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TestCallSlots,
  TurnRef,
  TurnSeq
}
import grit.core.identity.TestAccounts
import grit.core.inbox.Slotted
import grit.core.job.{
  Declared,
  Ending,
  Grace,
  Job,
  JobRun,
  Report,
  Schedule,
  ScheduleContract,
  Slot,
  SlotRule,
  When
}
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.job.clock.{ClockEdge, Ticked}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.turn.{Turn, TurnLoop}

import utest.*

/** A deployment's jobs and declared schedules over live engines launched as the kit launches
  * one: what each start reconciles, and what grit's clock edge starts and reads back across
  * restarts, redeploys and runs that end without a reply. Each pass is the clock edge's own
  * `tick`, at a time the test sets, so no pass races the assertions.
  */
object DeclaredLiveTests extends TestSuite {

  /** A job named `called` at `version`, with no parameters, whose runs reply as `says` says. */
  final class Ping(called: String, val version: Int, says: Ping.Says = Ping.Says.Reply)
      extends Job[Ping.None.type] {
    val name: JobName = JobName.of(called).fold(sys.error, identity)
    def write(params: Ping.None.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, Ping.None.type] = Right(Ping.None)
    def reply(run: JobRun[Ping.None.type]): String = says match {
      case Ping.Says.Reply => s"$called v$version"
      case Ping.Says.Unwritable => s"$called\u0000"
      case Ping.Says.Crash => throw new StackOverflowError(s"$called crashes")
    }
  }

  object Ping {
    case object None extends caps.Pure

    enum Says {

      /** "{called} v{version}". */
      case Reply

      /** Text Postgres refuses to keep (a NUL), so the run's reply cannot be written and it ends
        * without one, its workflow's status `SUCCESS`.
        */
      case Unwritable

      /** Nothing: it throws an error no transaction catches, so its workflow ends `ERROR`. */
      case Crash
    }
  }

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  private def deployment(
      jobs: Vector[Job[?]],
      schedules: Vector[Declared[?]] = Vector.empty
  ): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(8000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        jobs = jobs,
        schedules = schedules
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(c: DbConfig, d: Deployment): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        d
      )
      .fold(r => sys.error(r.message), identity)

  /** `body` over an engine on `config` launched for `d` as the kit launches one, but sweeping
    * nothing and ticking no clock edge of its own; closed after.
    */
  private def launched[A](config: DbConfig, d: Deployment)(body: Engine^ => A): A = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      Launch(engine, d, secrets(config, d), Launch.Run.Served, sweeping = false, _ => ())
      body(engine)
    } finally engine.close()
  }

  /** `body` over an engine on `config` with nothing launched: what it enqueues stays queued
    * until a later engine launches. Closed after.
    */
  private def unlaunched[A](config: DbConfig, clock: Clock^)(
      body: Engine^ => A
  ): A = {
    val engine = LiveEngine.open(config, Turn.Epoch, clock = clock)
    try body(engine)
    finally engine.close()
  }

  /** One pass of grit's clock edge on `engine` at `at`, by `d`'s jobs. */
  private def tick(engine: Engine^, d: Deployment, at: Instant): Ticked =
    right(
      new ClockEdge(engine.inbox, engine.schedules, engine.db, new SetClock(at), d.allJobs).tick()
    )

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  private def read(engine: Engine^, id: ScheduleId): Option[Schedule] =
    right(engine.db.read(Subject.Public)(engine.schedules.read(id)))

  /** The schedules with a run in flight on `engine`. */
  private def flying(engine: Engine^): Vector[ScheduleId] =
    right(engine.db.read(Subject.Public)(engine.schedules.inFlight(100))).map(_._1)

  /** What one pass started of the one schedule it took up. */
  private def only(ticked: Ticked): Slotted =
    (ticked.slotted, ticked.failed) match {
      case (Vector((_, slotted)), Vector()) => slotted
      case other => sys.error(s"not one schedule slotted: $other")
    }

  /** What the run `slotted` started returned, once it has; `None` when it started none. */
  private def awaited(engine: Engine^, slotted: Slotted): Option[String] = slotted match {
    case Slotted.Started(turn, _) => Some(engine.awaitTurn(turn))
    case Slotted.Superseding(turn, _) => Some(engine.awaitTurn(turn))
    case _ => None
  }

  private def started(slotted: Slotted): TurnRef = slotted match {
    case Slotted.Started(turn, _) => turn
    case Slotted.Superseding(turn, _) => turn
    case other => sys.error(s"not started: $other")
  }

  /** The status of each workflow DBOS keeps for `conversation`'s turns, by turn. Read in SQL
    * because only `grit.dbos` names DBOS's client.
    */
  private def workflows(config: DbConfig, conversation: ConversationId): Vector[(String, String)] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT workflow_uuid, status FROM dbos.workflow_status WHERE workflow_uuid LIKE ? ORDER BY workflow_uuid"
        )
      ) { ps =>
        ps.setString(1, s"${ConversationId.value(conversation)}:%")
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String)]
          while (rs.next()) rows += ((rs.getString(1), rs.getString(2)))
          rows.result()
        }
      }
    }

  /** `turn`'s workflow status once it has ended, waiting up to 30 s; `None` if it has not. */
  private def finished(config: DbConfig, turn: TurnRef): Option[String] = {
    val id = grit.core.id.WorkflowId.value(turn.workflowId)
    def status = workflows(config, turn.conversationId).collectFirst { case (`id`, s) => s }
    val ended = eventually(status.exists(s => s != "PENDING" && s != "ENQUEUED"))
    Option.when(ended)(status).flatten
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

  private def key(s: String): ScheduleKey = ScheduleKey.of(s).fold(sys.error, identity)

  private def declared(k: String, job: Ping, rule: SlotRule): Declared[Ping.None.type] =
    Declared(key(k), job, rule, Ping.None)

  private def id(k: String): ScheduleId = ScheduleId.declared(Declarer.Deployment, key(k))

  private val Daily = SlotRule.Daily(LocalTime.of(9, 0), ZoneOffset.UTC)

  private val Grace5: Grace = Grace.of(5.minutes).getOrElse(sys.error("grace"))

  /** The conversation of `slot`'s runs of `job`, if one was ever started. */
  private def ran(engine: Engine^, slot: Slot, job: Ping): Option[ConversationId] =
    right(engine.db.read(Subject.Public)(engine.conversations.find(slot.origin(job.name))))
      .map(_.id)

  val tests = Tests {
    test(
      "each start writes new declared schedules, rewrites changed ones, ends undeclared ones and revives one declared again"
    ) {
      val config = TestPostgres.freshDatabase("declared_reconcile")
      val ping = new Ping("ping", 1)
      val weekly = SlotRule.Weekly(DayOfWeek.MONDAY, LocalTime.of(9, 0), ZoneOffset.UTC)
      def kept(rule: SlotRule, ended: Option[Ending]) =
        Some(
          Schedule(ping.name, ujson.Obj(), PrincipalId.Grit, Report.Kept, rule, ended, Label.Public)
        )
      val first = launched(
        config,
        deployment(Vector(ping), Vector(declared("a", ping, Daily), declared("b", ping, Daily)))
      )(e => (read(e, id("a")), read(e, id("b"))))
      val second = launched(config, deployment(Vector(ping), Vector(declared("a", ping, weekly))))(
        e => (read(e, id("a")), read(e, id("b")))
      )
      val third = launched(
        config,
        deployment(Vector(ping), Vector(declared("a", ping, weekly), declared("b", ping, Daily)))
      )(e => (read(e, id("a")), read(e, id("b"))))
      (first, second, third) ==> (
        (kept(Daily, None), kept(Daily, None)),
        (kept(weekly, None), kept(Daily, Some(Ending.Undeclared))),
        (kept(weekly, None), kept(Daily, None))
      )
    }

    test("a recurrence whose slots passed while grit was down runs only its latest") {
      val config = TestPostgres.freshDatabase("declared_downtime")
      val ping = new Ping("ping", 1)
      val d = deployment(Vector(ping), Vector(declared("daily", ping, Daily)))
      launched(config, d) { engine =>
        val first = Daily.after(Instant.now()).getOrElse(sys.error("no slot"))
        val latest = first.plus(2, ChronoUnit.DAYS)
        val slotted = only(tick(engine, d, latest.plus(1, ChronoUnit.MINUTES)))
        val turn = started(slotted)
        (
          slotted,
          engine.awaitTurn(turn),
          ran(engine, Slot(id("daily"), first), ping),
          ran(engine, Slot(id("daily"), first.plus(1, ChronoUnit.DAYS)), ping),
          flying(engine)
        ) ==> (
          Slotted.Started(turn, Slot(id("daily"), latest)),
          s"replied: ${grit.core.id.EntryId.value(turn.replyId)}",
          None,
          None,
          Vector()
        )
      }
    }

    test(
      "a once slot whose run started at an old version runs again at the new one, past its grace"
    ) {
      val config = TestPostgres.freshDatabase("declared_once_redeployed")
      val at = Instant.now().truncatedTo(ChronoUnit.SECONDS).plus(1, ChronoUnit.MINUTES)
      val rule = SlotRule.Once(at, Grace5)
      val (v1, v2) = (new Ping("ping", 1), new Ping("ping", 2))
      val (d1, d2) = (
        deployment(Vector(v1), Vector(declared("once", v1, rule))),
        deployment(Vector(v2), Vector(declared("once", v2, rule)))
      )
      // Started at v1 by an engine that stops before running it.
      val stale = unlaunched(config, Clock.system()) { engine =>
        right(Launch.reconcile(engine.schedules, engine.jot, d1, Instant.now()))
        started(only(tick(engine, d1, at.plus(1, ChronoUnit.MINUTES))))
      }
      launched(config, d2) { engine =>
        val superseded = engine.awaitTurn(stale)
        val slotted = only(tick(engine, d2, at.plus(2, ChronoUnit.HOURS)))
        // The same slot's conversation, its next turn.
        val rerun = TurnRef(stale.conversationId, stale.turnSeq.next)
        (
          superseded,
          slotted,
          awaited(engine, slotted),
          read(engine, id("once")).flatMap(_.ended)
        ) ==> (
          "superseded: started at v1, its job at v2",
          Slotted.Superseding(rerun, Slot(id("once"), at)),
          Some(s"replied: ${grit.core.id.EntryId.value(rerun.replyId)}"),
          Some(Ending.Ran)
        )
      }
    }

    test(
      "a recurrence's run at an old version is superseded by its latest due slot, its own never run again"
    ) {
      val config = TestPostgres.freshDatabase("declared_daily_redeployed")
      val (v1, v2) = (new Ping("ping", 1), new Ping("ping", 2))
      val (d1, d2) = (
        deployment(Vector(v1), Vector(declared("daily", v1, Daily))),
        deployment(Vector(v2), Vector(declared("daily", v2, Daily)))
      )
      val first = Daily.after(Instant.now()).getOrElse(sys.error("no slot"))
      val next = first.plus(1, ChronoUnit.DAYS)
      val stale = unlaunched(config, Clock.system()) { engine =>
        right(Launch.reconcile(engine.schedules, engine.jot, d1, Instant.now()))
        started(only(tick(engine, d1, first.plus(1, ChronoUnit.MINUTES))))
      }
      launched(config, d2) { engine =>
        val superseded = engine.awaitTurn(stale)
        val slotted = only(tick(engine, d2, next.plus(1, ChronoUnit.MINUTES)))
        val latest = ran(engine, Slot(id("daily"), next), v2).map(TurnRef(_, TurnSeq.First))
        (
          superseded,
          Some(slotted),
          awaited(engine, slotted),
          workflows(config, stale.conversationId).map(_._1),
          flying(engine)
        ) ==> (
          "superseded: started at v1, its job at v2",
          latest.map(Slotted.Superseding(_, Slot(id("daily"), next))),
          latest.map(t => s"replied: ${grit.core.id.EntryId.value(t.replyId)}"),
          Vector(grit.core.id.WorkflowId.value(stale.workflowId)),
          Vector()
        )
      }
    }

    test(
      "a once slot whose run ends without its reply, cleanly or in error, ends its schedule failed and is never started again"
    ) {
      // The once slot of a job saying as `says`: the next pass fails its run, the schedule ends
      // failed, a later pass starts nothing, and its conversation has the one workflow. What
      // that workflow ended as is returned.
      def ending(says: Ping.Says, suite: String) = {
        val config = TestPostgres.freshDatabase(suite)
        val job = new Ping("job", 1, says)
        val at = Instant.now().truncatedTo(ChronoUnit.SECONDS).plus(1, ChronoUnit.MINUTES)
        val d = deployment(Vector(job), Vector(declared("once", job, SlotRule.Once(at, Grace5))))
        launched(config, d) { engine =>
          val turn = started(only(tick(engine, d, at.plus(1, ChronoUnit.MINUTES))))
          val status = finished(config, turn)
          (
            only(tick(engine, d, at.plus(2, ChronoUnit.MINUTES))),
            read(engine, id("once")).flatMap(_.ended),
            tick(engine, d, at.plus(3, ChronoUnit.MINUTES)),
            workflows(config, turn.conversationId).map(_._1)
          ) ==> (
            Slotted.Failed(Slot(id("once"), at)),
            Some(Ending.Failed),
            Ticked(Vector(), Vector()),
            Vector(grit.core.id.WorkflowId.value(turn.workflowId))
          )
          status
        }
      }
      (
        ending(Ping.Says.Unwritable, "declared_once_unwritten"),
        ending(Ping.Says.Crash, "declared_once_error")
      ) ==> (Some("SUCCESS"), Some("ERROR"))
    }

    test(
      "a recurrence whose run ends in error has its run cleared, and that slot is never started again"
    ) {
      val config = TestPostgres.freshDatabase("declared_daily_error")
      val crash = new Ping("crash", 1, Ping.Says.Crash)
      val d = deployment(Vector(crash), Vector(declared("daily", crash, Daily)))
      launched(config, d) { engine =>
        val first = Daily.after(Instant.now()).getOrElse(sys.error("no slot"))
        val turn = started(only(tick(engine, d, first.plus(1, ChronoUnit.MINUTES))))
        (
          finished(config, turn),
          only(tick(engine, d, first.plus(2, ChronoUnit.MINUTES))),
          flying(engine),
          read(engine, id("daily")).map(_.ended),
          tick(engine, d, first.plus(3, ChronoUnit.MINUTES)),
          workflows(config, turn.conversationId).map(_._1)
        ) ==> (
          Some("ERROR"),
          Slotted.Failed(Slot(id("daily"), first)),
          Vector(),
          Some(None),
          Ticked(Vector(), Vector()),
          Vector(grit.core.id.WorkflowId.value(turn.workflowId))
        )
      }
    }

    test("a run whose job left the deployment while it ran is read as failed on the next pass") {
      val config = TestPostgres.freshDatabase("declared_job_removed")
      val ping = new Ping("ping", 1)
      val (with_, without) = (deployment(Vector(ping)), deployment(Vector()))
      val now = Instant.now().truncatedTo(ChronoUnit.SECONDS)
      val (asked, stale) = unlaunched(config, new SetClock(now)) { engine =>
        // The asking thread's turn, recorded once the engine has applied the schema.
        val asking =
          TurnRef(LiveDb.conversation(config, Origin.Slack("T1", "C1", "1.0")).id, TurnSeq.First)
        LiveDb.asking(config, asking, TestAccounts.account("slack:T1/U1"), Some("C1/1.0"))
        val desk = engine.desk(PluginName.of("pings").fold(sys.error, identity), Vector(ping.name))
        val asked = desk
          .ask(
            TestCallSlots.at(asking),
            ScheduleContract.booking(ping),
            When.In(1.minute),
            Grace5,
            Ping.None
          )
          .fold(r => sys.error(r.said), identity)
        (asked, started(only(tick(engine, with_, now.plus(2, ChronoUnit.MINUTES)))))
      }
      launched(config, without) { engine =>
        (
          engine.awaitTurn(stale),
          only(tick(engine, without, now.plus(3, ChronoUnit.MINUTES))),
          read(engine, asked.id).flatMap(_.ended)
        ) ==> (
          "jobless: no job ping",
          Slotted.Failed(Slot(asked.id, asked.at)),
          Some(Ending.Failed)
        )
      }
    }
  }
}
