package grit.app.main

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.SetClock
import grit.core.edge.EdgeStores
import grit.core.id.{
  ConversationId,
  Declarer,
  EntryId,
  PluginName,
  ScheduleId,
  ScheduleKey,
  TurnRef
}
import grit.core.inbox.Slotted
import grit.core.job.{Declared, Ending, Job, SlotRule}
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.{LifecycleSettings, Windows}
import grit.core.plugin.Plugin
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.job.clock.ClockEdge
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.remind.Reminders
import grit.slack.client.{FakeSlack, Self}
import grit.slack.edge.SlackEdge
import grit.slack.event.{Payloads, TeamId, UserId}
import grit.turn.{Turn, TurnLoop}

import utest.*

/** What a job's run leaves behind, reclaimed by retention alone: a reminder's run and one slot
  * of a declared daily recurrence, each run to its reply over a live engine launched as the
  * kit launches one, its period sealed by a sweep past the idle window, and everything it wrote
  * gone after a sweep past the retention and ledger windows. Time is the launch's clock, which
  * the test sets, and the windows are hours, so no wait is real.
  */
object RunRetentionLiveTests extends TestSuite {
  import Payloads.*

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** Two hours idle, three of retention, a four-hour ledger, half an hour to settle. */
  private val Small: LifecycleSettings = {
    val d = LifecycleSettings.Default
    Windows
      .of(2.hours, 3.hours, 4.hours)
      .flatMap(LifecycleSettings.of(_, d.balance, 30.minutes, d.resolveAt, d.asks, d.locality))
      .fold(why => sys.error(why), identity)
  }

  private val Ping = new DeclaredLiveTests.Ping("ping", 1)

  private val DailyKey: ScheduleKey = ScheduleKey.of("daily").fold(sys.error, identity)

  private val Daily = SlotRule.Daily(LocalTime.of(9, 0), ZoneOffset.UTC)

  private def deployment(
      plugins: Vector[Plugin],
      jobs: Vector[Job[?]],
      schedules: Vector[Declared[?]]
  ): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = plugins,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(3).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(8000)),
        topics = Topics.Stub,
        lifecycle = Small,
        budget = Budget(ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        jobs = jobs,
        schedules = schedules
      )
      .fold(r => sys.error(r.message), identity)

  /** The Reminders plugin, nothing declared. */
  private val Reminding: Deployment =
    deployment(
      Vector(new Reminders(PluginName.of("remind").fold(sys.error, identity))),
      Vector.empty,
      Vector.empty
    )

  /** `ping` declared daily at 09:00 UTC, no plugin. */
  private val Declaring: Deployment =
    deployment(
      Vector.empty,
      Vector(Ping),
      Vector(Declared(DailyKey, Ping, Daily, DeclaredLiveTests.Ping.None))
    )

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

  /** `body` over an engine on `config` launched for `d` with the scripted model, the stub
    * classifier and `clock`, sweeping nothing and ticking no clock edge of its own, and a Slack
    * edge over it and `slack`; closed after.
    */
  private def launched[A](config: DbConfig, d: Deployment, clock: SetClock^, slack: FakeSlack^)(
      body: (Engine^, SlackEdge^) => A
  ): A = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    val model = new ReminderLiveTests.Scripted
    val classifier = new ReminderLiveTests.Counting
    try {
      Launch.asking(
        engine,
        d,
        secrets(config, d),
        new ReminderLiveTests.Through(model),
        classifier,
        classifier,
        clock,
        sweeping = false,
        _ => ()
      )
      body(
        engine,
        new SlackEdge(
          slack,
          Self(TeamId(Team), UserId(Bot)),
          EdgeStores(
            engine.inbox,
            engine.principals,
            engine.deliveries,
            engine.acknowledgements,
            engine.reviews,
            engine.jot,
            engine
          ),
          Set.empty,
          None,
          clock,
          _ => ()
        )
      )
    } finally engine.close()
  }

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** The one run a pass of grit's clock edge on `engine` at `clock`'s time started, by `d`'s
    * jobs.
    */
  private def ticked(engine: Engine^, d: Deployment, clock: SetClock^): TurnRef =
    right(
      new ClockEdge(engine.inbox, engine.schedules, engine.db, clock, d.allJobs).tick()
    ).slotted match {
      case Vector((_, Slotted.Started(turn, _))) => turn
      case other => sys.error(s"not one run started: $other")
    }

  /** `edge`'s delivery passes until Slack holds `posts` posts or 60 s pass. */
  private def delivered(edge: SlackEdge^, slack: FakeSlack^, posts: Int): Unit = {
    val until = System.nanoTime() + 60.seconds.toNanos
    while (slack.posts.size < posts && System.nanoTime() < until) {
      val _ = right(edge.deliver())
      Thread.sleep(100)
    }
    slack.posts.size ==> posts
  }

  /** `payload` heard by `edge`'s Slack, and delivered until Slack holds `posts` posts. */
  private def heard(edge: SlackEdge^, slack: FakeSlack^, payload: String, posts: Int): Unit = {
    val _ = slack.listen(edge.receive)
    slack.deliver(payload) ==> true
    delivered(edge, slack, posts)
  }

  /** Each row `sql` reads with `params` bound, as `row` reads it. */
  private def query[A](config: DbConfig, sql: String, params: String*)(
      row: java.sql.ResultSet => A
  ): Vector[A] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[A]
          while (rs.next()) rows += row(rs)
          rows.result()
        }
      }
    }

  /** Waits up to 30 s for every workflow DBOS keeps to have ended; whether they all had. */
  private def quiet(config: DbConfig): Boolean = {
    def busy =
      query(
        config,
        "SELECT count(*) FROM dbos.workflow_status WHERE status IN ('PENDING', 'ENQUEUED')"
      )(_.getLong(1)).exists(_ > 0)
    val until = System.nanoTime() + 30.seconds.toNanos
    var going = busy
    while (going && System.nanoTime() < until) {
      Thread.sleep(100)
      going = busy
    }
    !going
  }

  /** Sweeps of `engine` at `at`, `clock` set to it, each after the workflows the one before
    * started have ended: enough for a seal's closes, and for a collection that waits on
    * another.
    */
  private def swept(config: DbConfig, engine: Engine^, clock: SetClock^, at: Instant): Unit = {
    clock.at = at
    (1 to 3).foreach { _ =>
      val _ = right(engine.sweep(at))
      assert(quiet(config))
    }
  }

  /** What a run wrote, found while it is kept: its conversation, its place, its entries and
    * its deliveries.
    */
  private final case class Wrote(
      conversation: ConversationId,
      place: String,
      entries: Vector[String],
      deliveries: Vector[String]
  )

  private def wrote(config: DbConfig, conversation: ConversationId): Wrote = {
    val c = ConversationId.value(conversation)
    Wrote(
      conversation,
      query(config, "SELECT place_id::text FROM grit.conversations WHERE id = ?::uuid", c)(
        _.getString(1)
      ) match {
        case Vector(place) => place
        case other => sys.error(s"not one conversation: $other")
      },
      query(config, "SELECT id FROM grit.entries WHERE conversation_id = ?::uuid", c)(
        _.getString(1)
      ),
      query(config, "SELECT workflow FROM grit.deliveries WHERE conversation_id = ?::uuid", c)(
        _.getString(1)
      )
    )
  }

  /** How many rows hold what `w` names, by table: every table of grit's with a conversation,
    * its conversation and place, its entries' inbound rows, its deliveries' parts, the schedule
    * `schedule`, and every table of DBOS's keyed by a workflow whose id names its conversation.
    */
  private def held(config: DbConfig, w: Wrote, schedule: Option[ScheduleId]): Map[String, Long] = {
    val c = ConversationId.value(w.conversation)
    def count(sql: String, params: String*): Long =
      query(config, sql, params*)(_.getLong(1)).sum
    def in(values: Vector[String]): String =
      ujson.Arr.from(values.map(ujson.Str(_))).render()
    val byConversation =
      query(
        config,
        """SELECT table_name FROM information_schema.columns
          | WHERE table_schema = 'grit' AND column_name = 'conversation_id'""".stripMargin
      )(_.getString(1)).map { t =>
        s"grit.$t" -> count(s"SELECT count(*) FROM grit.$t WHERE conversation_id = ?::uuid", c)
      }
    val byWorkflow =
      query(
        config,
        """SELECT table_name, column_name FROM information_schema.columns
          | WHERE table_schema = 'dbos' AND column_name IN ('workflow_uuid', 'workflow_id')""".stripMargin
      )(rs => (rs.getString(1), rs.getString(2))).map { (t, col) =>
        s"dbos.$t" -> count(
          s"SELECT count(*) FROM dbos.$t WHERE strpos($col, ?) > 0",
          c
        )
      }
    (byConversation ++ byWorkflow).toMap ++ Map(
      "grit.conversations" -> count(
        "SELECT count(*) FROM grit.conversations WHERE id = ?::uuid",
        c
      ),
      "grit.places" -> count("SELECT count(*) FROM grit.places WHERE id = ?::uuid", w.place),
      "grit.inbound" -> count(
        "SELECT count(*) FROM grit.inbound WHERE entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))",
        in(w.entries)
      ),
      "grit.delivery_parts" -> count(
        "SELECT count(*) FROM grit.delivery_parts WHERE workflow IN (SELECT jsonb_array_elements_text(?::jsonb))",
        in(w.deliveries)
      ),
      "grit.schedules" -> schedule.fold(0L)(id =>
        count("SELECT count(*) FROM grit.schedules WHERE id = ?", ScheduleId.value(id))
      )
    )
  }

  /** The tables of `rows` holding any. */
  private def nonEmpty(rows: Map[String, Long]): Set[String] = rows.filter(_._2 > 0).keySet

  /** Each tombstone naming `conversation`'s periods or `schedule`: its kind, and whether it
    * was collected (not spared).
    */
  private def tombstones(
      config: DbConfig,
      conversation: ConversationId,
      schedule: Option[ScheduleId]
  ): Set[(String, Boolean)] =
    query(
      config,
      """SELECT kind, collected_at IS NOT NULL AND NOT spared FROM grit.tombstones
        | WHERE starts_with(target, ?) OR (kind = 'schedule' AND target = ?)""".stripMargin,
      s"${ConversationId.value(conversation)}:",
      schedule.fold("")(ScheduleId.value)
    )(rs => (rs.getString(1), rs.getBoolean(2))).toSet

  /** The test's start, to the second. */
  private def start: Instant = Instant.now().truncatedTo(ChronoUnit.SECONDS)

  /** Past the idle window after `at`, by a minute. */
  private def idle(at: Instant): Instant =
    at.plusMillis(Small.windows.idle.toMillis).plus(1, ChronoUnit.MINUTES)

  /** Past the retention and ledger windows after `at`, by a minute. */
  private def kept(at: Instant): Instant =
    at.plusMillis((Small.windows.retention + Small.windows.ledger).toMillis)
      .plus(1, ChronoUnit.MINUTES)

  val tests = Tests {
    test(
      "a reminder's run, once sealed, is gone whole past retention and ledger: its schedule, conversation, place, entries, delivery and workflows; the asking conversation is untouched"
    ) {
      val config = TestPostgres.freshDatabase("retention_reminder")
      val t0 = start
      val (clock, slack) = (new SetClock(t0), new FakeSlack)
      launched(config, Reminding, clock, slack) { (engine, edge) =>
        heard(edge, slack, mention("1.0", s"<@$Bot> remind me to stretch in a minute"), 1)
        val schedule =
          query(config, "SELECT id FROM grit.schedules")(rs =>
            ScheduleId.of(rs.getString(1)).fold(sys.error, identity)
          ) match {
            case Vector(one) => one
            case other => sys.error(s"not one schedule: $other")
          }
        clock.at = t0.plus(2, ChronoUnit.MINUTES)
        val run = ticked(engine, Reminding, clock)
        engine.awaitTurn(run) ==> s"replied: ${EntryId.value(run.replyId)}"
        delivered(edge, slack, 2)
        val sealing = idle(clock.at)
        // The asking thread speaks again just before the run's period seals, so its own period
        // is open through the run's purge.
        clock.at = sealing.minus(1, ChronoUnit.MINUTES)
        heard(edge, slack, mentionIn("1.0", "1.5"), 3)
        val asking = query(
          config,
          "SELECT conversation_id::text FROM grit.conversations c JOIN grit.entries e ON e.conversation_id = c.id WHERE c.id <> ?::uuid GROUP BY conversation_id",
          ConversationId.value(run.conversationId)
        )(rs => ConversationId(rs.getString(1))) match {
          case Vector(one) => one
          case other => sys.error(s"not one asking conversation: $other")
        }
        swept(config, engine, clock, sealing)
        val w = wrote(config, run.conversationId)
        val before = held(config, w, Some(schedule))
        val ended =
          right(engine.db.read(Subject.Public)(engine.schedules.read(schedule))).flatMap(_.ended)
        val askingBefore = wrote(config, asking)
        swept(config, engine, clock, kept(sealing))
        val after = held(config, w, Some(schedule))
        val askingAfter = wrote(config, asking)
        (
          ended,
          nonEmpty(before),
          nonEmpty(after),
          tombstones(config, run.conversationId, Some(schedule)),
          askingBefore.entries.nonEmpty,
          askingBefore.entries.filterNot(askingAfter.entries.contains)
        ) ==> (
          Some(Ending.Ran),
          Set(
            "grit.schedules",
            "grit.conversations",
            "grit.places",
            "grit.entries",
            "grit.inbound",
            "grit.periods",
            "grit.deliveries",
            "grit.delivery_parts",
            "dbos.workflow_status",
            "dbos.workflow_input",
            "dbos.workflow_output",
            "dbos.operation_outputs",
            "dbos.tx_step_outputs"
          ),
          Set(),
          Set(("raw", true), ("quiet", true), ("schedule", true)),
          true,
          Vector()
        )
      }
    }

    test(
      "a declared daily recurrence's run, once sealed, is gone whole past retention and ledger, and its schedule stays pending"
    ) {
      val config = TestPostgres.freshDatabase("retention_daily")
      val first = Daily.after(start).getOrElse(sys.error("no slot"))
      val id = ScheduleId.declared(Declarer.Deployment, DailyKey)
      // Declared before its first slot, which the clock edge then finds due.
      val (clock, slack) = (new SetClock(first.minus(1, ChronoUnit.MINUTES)), new FakeSlack)
      launched(config, Declaring, clock, slack) { (engine, _) =>
        clock.at = first.plus(1, ChronoUnit.MINUTES)
        val run = ticked(engine, Declaring, clock)
        engine.awaitTurn(run) ==> s"replied: ${EntryId.value(run.replyId)}"
        val sealing = idle(clock.at)
        swept(config, engine, clock, sealing)
        val w = wrote(config, run.conversationId)
        val before = held(config, w, None)
        swept(config, engine, clock, kept(sealing))
        (
          nonEmpty(before),
          nonEmpty(held(config, w, None)),
          tombstones(config, run.conversationId, Some(id)),
          right(engine.db.read(Subject.Public)(engine.schedules.read(id))).map(_.ended)
        ) ==> (
          Set(
            "grit.conversations",
            "grit.places",
            "grit.entries",
            "grit.inbound",
            "grit.periods",
            "dbos.workflow_status",
            "dbos.workflow_input",
            "dbos.workflow_output",
            "dbos.operation_outputs",
            "dbos.tx_step_outputs"
          ),
          Set(),
          Set(("raw", true), ("quiet", true)),
          Some(None)
        )
      }
    }
  }
}
