package grit.app.main

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.id.{CloseRef, SourceId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.{CloseReason, LifecycleSettings, TestClosings}
import grit.core.speech.{Limits, Reach, Speaking, Stage}
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Origin, StoreError}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Unprompted speech end to end, over a live engine launched as the kit launches a deployment
  * (ADR 0022): a message heard in a thread with a record is triaged, drafted in its own turn
  * by the stub model, judged by the stub classifier, and posted, awaiting its edge; under
  * Shadow, judged the same and not posted. A question triage reads as put to grit by name is
  * answered as said to grit, in shadow too.
  */
object UnpromptedLiveTests extends TestSuite {

  private lazy val config = TestPostgres.freshDatabase("unprompted")

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** A gate the stub classifier can pass: V2's `gap` choosing `asks` (the stub answers every
    * yes/no alike, so V2's own gate, which needs `open` high and `to` low, holds everything).
    */
  private val asks = grit.core.triage.Tags.V2.asks(grit.core.period.Probability.clamped(0.5))

  /** The stub classifier answers every yes/no alike, triage's `to-grit` and the judge's
    * questions too: a heard question marked `~0.4` is not read as put to grit (that needs at
    * least one half), so its draft is judged, and posts at 0.4 only under this `postAt`.
    */
  private val limits =
    Limits
      .suggested(DailyCap.of("0.25").getOrElse(sys.error("a cap")), asks)
      .copy(postAt = grit.core.period.Probability.clamped(0.3))

  private def deployment(speaking: Speaking): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = speaking,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(d: Deployment, c: DbConfig): Secrets =
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

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** In `thread`'s conversation: a remark heard (its period then closed with a record), and a
    * question heard live after it, the stub classifier reading it as a question, put to grit by
    * name when `named`; the question's turn.
    */
  private def converse(engine: Engine^, thread: String, named: Boolean = false): TurnRef = {
    val origin = Origin.Slack("T1", "C1", thread)
    val now = Instant.now()
    engine.inbox.hear(
      origin,
      SourceId(s"$thread:0"),
      "morning",
      Account.Local,
      now,
      Reach.Nowhere
    ) ==>
      Right(())
    val c = right(engine.db.read(Subject.Public)(engine.conversations.find(origin)))
      .getOrElse(sys.error("heard"))
    val first = TurnRef(c.id, TurnSeq.First)
    right(engine.jot.write(Subject.Public)(engine.periods.of(first))).foreach { p =>
      right(
        engine.jot.write(Subject.Public)(
          engine.periods.seal(
            CloseRef(p.ref, first.turnSeq, now),
            CloseReason.Lapsed,
            TestClosings.prose("The refi page's byline moves to the bottom."),
            now
          )
        )
      )
    }
    engine.inbox.hear(
      origin,
      SourceId(s"$thread:1"),
      s"where does the refi page's byline go? ${if (named) "" else "~0.4 "}~back:asks",
      Account.Local,
      now,
      Reach(Some(s"C1/$thread/$thread:1"), Set.empty)
    ) ==> Right(())
    TurnRef(c.id, TurnSeq(1))
  }

  /** What was kept of `turn`'s draft: its stage in the speech ledger. */
  private def stage(engine: Engine^, turn: TurnRef): Option[Stage] =
    right(engine.db.read(Subject.Public)(engine.speech.spoken(Instant.EPOCH)))
      .find(_.turn == turn)
      .map(_.stage)

  val tests = Tests {
    test(
      "within its limits, a question heard in a thread with a record is answered there, and its turn's line told once, its draft and reply one round"
    ) {
      val d = deployment(Speaking.Within(limits))
      val engine = LiveEngine.open(config, Turn.Epoch)
      val told = new java.util.concurrent.ConcurrentLinkedQueue[String]
      try {
        Launch(
          engine,
          d,
          secrets(d, config),
          Launch.Run.Served,
          Clock.system(),
          sweeping = false,
          line => told.add(line): Unit
        )
        val turn = converse(engine, "10.0")
        assert(eventually(stage(engine, turn) match {
          case Some(Stage.Posted(_)) => true
          case _ => false
        }))
        val pending = right(engine.jot.write(Subject.Public)(engine.deliveries.pending()))
          .filter(_.turn == turn)
        val reply = right(engine.db.read(Subject.Public)(engine.entries.get(turn.replyId)))
        (pending.map(_.to), reply.nonEmpty) ==> (Vector("C1/10.0/10.0:1"), true)
        // Told inside the workflow as its body returns, before DBOS records the result: once
        // the result is in, the body never runs again, so no later line can come.
        val wf = grit.core.id.WorkflowId.value(turn.workflowId)
        val _ = engine.awaitTurn(turn)
        val lines =
          scala.jdk.CollectionConverters
            .ListHasAsScala(java.util.List.copyOf(told))
            .asScala
            .toVector
            .filter(_.startsWith(s"turn $wf "))
        lines.map(l =>
          (l.takeWhile(_ != ';'), l.endsWith(s"; posted: reply:$wf; summarised: summary:$wf"))
        ) ==>
          Vector((s"turn $wf at slack:T1/C1/10.0: 1 round, no tools", true))
      } finally engine.close()
    }

    test("in shadow, the same question is drafted and judged, and nothing is posted") {
      val d = deployment(Speaking.Shadow(limits))
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        Launch(
          engine,
          d,
          secrets(d, config),
          Launch.Run.Served,
          Clock.system(),
          sweeping = false,
          _ => ()
        )
        val turn = converse(engine, "20.0")
        assert(eventually(stage(engine, turn).contains(Stage.Settled)))
        val pending = right(engine.jot.write(Subject.Public)(engine.deliveries.pending()))
          .filter(_.turn == turn)
        val reply = right(engine.db.read(Subject.Public)(engine.entries.get(turn.replyId)))
        (pending, reply) ==> (Vector.empty, None)
        // Judged, not passed over: the judge's call is in the ledger.
        val ledger = right(engine.db.read(Subject.Public)(engine.ledger.of(turn.workflowId)))
        assert(ledger.exists(_.entry == grit.turn.TurnJudge.id(turn)))
      } finally engine.close()
    }

    test(
      "a question put to grit by name is answered as said to grit, in shadow too: its reply awaited where it was heard, nothing judged, nothing counted as speech"
    ) {
      val d = deployment(Speaking.Shadow(limits))
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        Launch(
          engine,
          d,
          secrets(d, config),
          Launch.Run.Served,
          Clock.system(),
          sweeping = false,
          _ => ()
        )
        val day = grit.core.spend.Budget(java.time.ZoneOffset.UTC, None).today(Instant.now())
        val spent = right(engine.db.read(Subject.Public)(engine.speech.spentOn(day)))
        val turn = converse(engine, "30.0", named = true)
        assert(
          eventually(
            right(engine.db.read(Subject.Public)(engine.entries.get(turn.replyId))).nonEmpty
          )
        )
        val _ = engine.awaitTurn(turn)
        val pending = right(engine.jot.write(Subject.Public)(engine.deliveries.pending()))
          .filter(_.turn == turn)
        val ledger = right(engine.db.read(Subject.Public)(engine.ledger.of(turn.workflowId)))
        (
          pending.map(_.to),
          ledger.nonEmpty,
          ledger.exists(_.entry == grit.turn.TurnJudge.id(turn)),
          stage(engine, turn),
          right(engine.db.read(Subject.Public)(engine.speech.answering(turn))),
          right(engine.db.read(Subject.Public)(engine.speech.spentOn(day)))
        ) ==> (Vector("C1/30.0/30.0:1"), true, false, None, true, spent)
      } finally engine.close()
    }
  }
}
