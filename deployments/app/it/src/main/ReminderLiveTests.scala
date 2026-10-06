package grit.app.main

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.classify.{Answers, Classifier, ClassifierError, Question}
import grit.core.clock.{SetClock, Utc}
import grit.core.edge.EdgeStores
import grit.core.id.{CloseRef, PeriodRef, PeriodSeq, PluginName, ScheduleId, ToolCallId, TurnRef}
import grit.core.inbox.Slotted
import grit.core.job.{Ending, Schedule, Slot}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.LifecycleSettings
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{StoreError, Tx}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.job.clock.ClockEdge
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.models.{StubClassifier, StubModels}
import grit.prose.markdown.Markdown
import grit.remind.Reminders
import grit.slack.client.{FakeSlack, Self}
import grit.slack.edge.SlackEdge
import grit.slack.event.{Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText
import grit.turn.{Turn, TurnLoop}

import utest.*

/** A reminder set in a Slack thread, end to end over live engines launched as the kit launches
  * one, with the Reminders plugin, a fake Slack and a model scripted to ask for it: the engine
  * that heard the asking closes before the reminder is due, and the one open when it falls due
  * posts it. Time is the jobs' clock, which the test sets; each of the clock edge's passes is
  * the test's own `tick`, so none races the assertions.
  */
object ReminderLiveTests extends TestSuite {
  import Payloads.*

  /** A model that asks for a reminder to stretch in a minute when it is offered `remind_me`
    * and the last message is the person's, saying "remind"; otherwise it answers "Done.".
    * Counting its calls.
    */
  final class Scripted extends Provider {
    val calls = new AtomicInteger(0)

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val _ = calls.incrementAndGet()
      val asks = request.tools.exists(_.name == "remind_me") &&
        request.messages.lastOption.exists {
          case Message.User(text) => text.contains("remind")
          case _ => false
        }
      Right(
        Message.Assistant(
          if (asks)
            Vector(
              AssistantBlock.ToolCall(
                ToolCallId("remind"),
                "remind_me",
                ujson.Obj("text" -> "stretch", "in_minutes" -> 1)
              )
            )
          else Vector(AssistantBlock.Text("Done.")),
          if (asks) StopReason.ToolUse else StopReason.EndTurn,
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
          "test/scripted"
        )
      )
    }
  }

  /** The stub's catalog, every role's calls made through `model`. */
  final class Through(model: Provider^) extends Models {
    def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)
    def provider(pinned: Pinned): Provider^{model} = model
  }

  /** The stub classifier, counting the requests it answers. */
  final class Counting extends Classifier {
    val calls = new AtomicInteger(0)
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      val _ = calls.incrementAndGet()
      Right(StubClassifier.answers(state, questions))
    }
  }

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  private val deployment: Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector(new Reminders(PluginName.of("remind").fold(sys.error, identity))),
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(3).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(8000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(c: DbConfig): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        deployment
      )
      .fold(r => sys.error(r.message), identity)

  /** `body` over an engine on `config` launched for the deployment with `model`, `classifier`
    * and the jobs' `clock`, sweeping nothing and ticking no clock edge of its own, and the
    * Slack edge over it and `slack`; closed after.
    */
  private def launched[A](
      config: DbConfig,
      model: Scripted^,
      classifier: Counting^,
      clock: SetClock^,
      slack: FakeSlack^
  )(body: (Engine^, SlackEdge^) => A): A = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      Launch.asking(
        engine,
        deployment,
        secrets(config),
        new Through(model),
        classifier,
        classifier,
        clock,
        sweeping = false,
        _ => ()
      )
      body(engine, edge(engine, slack))
    } finally engine.close()
  }

  /** `body` over an engine on `config` with nothing launched; closed after. */
  private def unlaunched[A](config: DbConfig)(body: Engine^ => A): A = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    try body(engine)
    finally engine.close()
  }

  private def edge(engine: Engine^, slack: FakeSlack^): SlackEdge^ =
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
      grit.core.clock.Clock.system(),
      _ => ()
    )

  /** What one pass of grit's clock edge on `engine` started, at `clock`'s time now. */
  private def tick(engine: Engine^, clock: SetClock^): Vector[Slotted] =
    right(
      new ClockEdge(engine.inbox, engine.schedules, engine.db, clock, deployment.allJobs)
        .tick()
    ) match {
      case ticked if ticked.failed.isEmpty => ticked.slotted.map(_._2)
      case ticked => sys.error(s"not started: ${ticked.failed}")
    }

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  private def read(engine: Engine^, id: ScheduleId): Option[Schedule] =
    right(engine.db.read(engine.schedules.read(id)))

  /** `edge`'s delivery passes until Slack holds `posts` posts or 60 s pass, then one more,
    * which must deliver nothing.
    */
  private def delivered(edge: SlackEdge^, slack: FakeSlack^, posts: Int): Unit = {
    val until = System.nanoTime() + 60.seconds.toNanos
    while (slack.posts.size < posts && System.nanoTime() < until) {
      val _ = right(edge.deliver())
      Thread.sleep(100)
    }
    edge.deliver() ==> Right(0)
  }

  /** Each post Slack holds: its thread and its fallback text. */
  private def posted(slack: FakeSlack^): Vector[(Ts, String)] =
    slack.posts.map(p => (p.thread, p.post.fallback))

  /** What Slack shows for a reply whose text is `text`. */
  private def shown(text: String): String =
    RichText.render(Markdown.parse(text)).headOption.fold("")(_.fallback)

  /** The asking: a mention in the thread `1.0` asking for a reminder, heard by `edge`'s Slack,
    * answered by the scripted model's call of `remind_me` and then "Done.", posted in that
    * thread as Slack's post number `posts`.
    */
  private def ask(edge: SlackEdge^, slack: FakeSlack^, posts: Int = 1): Unit =
    heard(edge, slack, mention("1.0", s"<@$Bot> remind me to stretch in a minute"), posts)

  /** `payload` heard by `edge`'s Slack, and delivered until Slack holds `posts` posts. */
  private def heard(edge: SlackEdge^, slack: FakeSlack^, payload: String, posts: Int): Unit = {
    val _ = slack.listen(edge.receive)
    slack.deliver(payload) ==> true
    delivered(edge, slack, posts)
  }

  /** The status of every workflow DBOS keeps, by id. Read in SQL because only `grit.dbos`
    * names DBOS's client.
    */
  private def workflows(config: DbConfig): Vector[(String, String)] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT workflow_uuid, status FROM dbos.workflow_status ORDER BY workflow_uuid"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String)]
          while (rs.next()) rows += ((rs.getString(1), rs.getString(2)))
          rows.result()
        }
      }
    }

  /** The id of every schedule kept. */
  private def scheduled(config: DbConfig): Vector[ScheduleId] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SELECT id FROM grit.schedules ORDER BY id")) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[ScheduleId]
          while (rs.next())
            rows += ScheduleId.of(rs.getString(1)).fold(why => sys.error(why), identity)
          rows.result()
        }
      }
    }

  private def execute(config: DbConfig, sql: String, id: String): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, id)
        val _ = ps.executeUpdate()
      }
    }

  /** Waits up to 30 s for every workflow DBOS keeps to have ended; whether they all had. */
  private def quiet(config: DbConfig): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    def busy = workflows(config).exists((_, s) => s == "PENDING" || s == "ENQUEUED")
    var going = busy
    while (going && System.nanoTime() < until) {
      Thread.sleep(100)
      going = busy
    }
    !going
  }

  /** `turn`'s workflow status once it has ended, waiting up to 30 s; `None` if DBOS does not
    * have it ended by then.
    */
  private def finished(config: DbConfig, turn: TurnRef): Option[String] = {
    val id = grit.core.id.WorkflowId.value(turn.workflowId)
    def status = workflows(config).collectFirst { case (`id`, s) => s }
    val until = System.nanoTime() + 30.seconds.toNanos
    var now = status
    while (!now.exists(s => s != "PENDING" && s != "ENQUEUED") && System.nanoTime() < until) {
      Thread.sleep(100)
      now = status
    }
    now.filter(s => s != "PENDING" && s != "ENQUEUED")
  }

  private val Idle = LifecycleSettings.Default.windows.idle

  /** The test's start, to the second: the jobs' clock reads it while the reminder is asked. */
  private def start: Instant = Instant.now().truncatedTo(ChronoUnit.SECONDS)

  val tests = Tests {
    test(
      "a reminder asked in a thread is posted there once, late, by the engine open when it falls due, and its run's period closes with no model or classifier call"
    ) {
      val config = TestPostgres.freshDatabase("reminder_exit")
      val t0 = start
      val (model, classifier, clock, slack) =
        (new Scripted, new Counting, new SetClock(t0), new FakeSlack)
      def calls = (model.calls.get, classifier.calls.get)
      val due = t0.plus(1, ChronoUnit.MINUTES)
      // A thread answered with no reminder, then the asking thread.
      launched(config, model, classifier, clock, slack) { (_, edge) =>
        heard(edge, slack, mention("0.5"), 1)
        ask(edge, slack, 2)
      }
      launched(config, model, classifier, clock, slack) { (engine, edge) =>
        clock.at = due.plus(5, ChronoUnit.MINUTES)
        val run = tick(engine, clock) match {
          case Vector(Slotted.Started(turn, slot)) if slot.nominal == due => turn -> slot
          case other => sys.error(s"not one run started at $due: $other")
        }
        val (turn, slot) = run
        engine.awaitTurn(turn) ==> s"replied: ${grit.core.id.EntryId.value(turn.replyId)}"
        delivered(edge, slack, 3)
        // Ended when it replied: no later pass starts it again.
        val after = tick(engine, clock)
        // The asking threads' periods fall due first, a day after them; the run's, a day after
        // its run, is quiet then but never asked whether anyone waits.
        assert(quiet(config))
        val first =
          right(engine.sweep(t0.plus(Idle.toSeconds, ChronoUnit.SECONDS).plusSeconds(180)))
        assert(quiet(config))
        val before = calls
        val second = right(
          engine.sweep(t0.plus(Idle.toSeconds, ChronoUnit.SECONDS).plus(7, ChronoUnit.MINUTES))
        )
        val period = PeriodRef(turn.conversationId, PeriodSeq.First)
        assert(quiet(config))
        (
          posted(slack),
          after,
          read(engine, slot.schedule).flatMap(_.ended),
          first.asked,
          second.enqueued.map(_.period),
          second.asked,
          workflows(config).collect { case (id, s) if id.startsWith(CloseRef.prefix(period)) => s },
          calls
        ) ==> (
          Vector(
            (Ts("0.5"), shown("Done.")),
            (Ts("1.0"), shown("Done.")),
            (Ts("1.0"), shown(s"Reminder: stretch\n(due ${Utc.toMinute(due)}; sent late)"))
          ),
          Vector(),
          Some(Ending.Ran),
          Vector(),
          Vector(period),
          Vector(),
          Vector("SUCCESS"),
          before
        )
      }
    }

    test(
      "a reminder whose run was started but never enqueued before a restart is started again and posted once"
    ) {
      val config = TestPostgres.freshDatabase("reminder_repaired")
      val t0 = start
      val (model, classifier, clock, slack) =
        (new Scripted, new Counting, new SetClock(t0), new FakeSlack)
      val due = t0.plus(1, ChronoUnit.MINUTES)
      launched(config, model, classifier, clock, slack)((_, edge) => ask(edge, slack))
      // The slot started by an engine whose enqueue is lost, as if it stopped between the
      // start's commit and the enqueue after it.
      val lost = unlaunched(config) { engine =>
        clock.at = due.plusSeconds(10)
        tick(engine, clock) match {
          case Vector(Slotted.Started(turn, slot)) if slot.nominal == due => turn
          case other => sys.error(s"not one run started at $due: $other")
        }
      }
      execute(
        config,
        "DELETE FROM dbos.workflow_status WHERE workflow_uuid = ?",
        grit.core.id.WorkflowId.value(lost.workflowId)
      )
      launched(config, model, classifier, clock, slack) { (engine, edge) =>
        clock.at = due.plusSeconds(20)
        val restarted = tick(engine, clock)
        val ended = finished(config, lost)
        delivered(edge, slack, 2)
        (restarted, ended, LiveTurn.reply(engine, lost), posted(slack)) ==> (
          Vector(Slotted.Restarted(lost)),
          Some("SUCCESS"),
          Some("Reminder: stretch"),
          Vector((Ts("1.0"), shown("Done.")), (Ts("1.0"), shown("Reminder: stretch")))
        )
      }
    }

    test(
      "a reminder whose engine was down past its hour of grace is missed, and nothing is posted"
    ) {
      val config = TestPostgres.freshDatabase("reminder_missed")
      val t0 = start
      val (model, classifier, clock, slack) =
        (new Scripted, new Counting, new SetClock(t0), new FakeSlack)
      val due = t0.plus(1, ChronoUnit.MINUTES)
      launched(config, model, classifier, clock, slack)((_, edge) => ask(edge, slack))
      launched(config, model, classifier, clock, slack) { (engine, edge) =>
        clock.at = due.plus(1, ChronoUnit.HOURS).plusSeconds(1)
        val id = scheduled(config) match {
          case Vector(one) => one
          case other => sys.error(s"not one schedule: $other")
        }
        val missed = tick(engine, clock)
        val _ = edge.deliver()
        (
          missed,
          read(engine, id).flatMap(_.ended),
          right(
            engine.db.read(
              engine.conversations.find(Slot(id, due).origin(Reminders.Remind.name))
            )
          ),
          posted(slack)
        ) ==> (
          Vector(Slotted.Missed(Slot(id, due))),
          Some(Ending.Missed),
          None,
          Vector((Ts("1.0"), shown("Done.")))
        )
      }
    }
  }
}
