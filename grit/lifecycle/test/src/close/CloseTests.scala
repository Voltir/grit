package grit.lifecycle.close

import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, PeriodRef, PeriodSeq, WorkflowId}
import grit.core.message.StopReason
import grit.core.period.TestClosings.{balance, line}
import grit.core.period.{
  Balance,
  Change,
  CloseReason,
  Closing,
  Flows,
  Ground,
  Judgement,
  LifecycleSettings,
  PeriodState,
  Probability,
  Section,
  TestClosings,
  Verdict
}
import grit.core.provider.ProviderError
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{Payload, Speakers, UsageLedger}
import grit.core.topic.{Placement, TopicEvent, TopicId, Weights}
import grit.dbos.sql.TestTx
import grit.lifecycle.transcript.PeriodTranscript

import utest.*

object CloseTests extends TestSuite {
  import CloseFixtures.*

  /** A day and a minute: past the default idle window after minute 1. */
  private val Lapsed = 24 * 60 + 1L

  private val written =
    """Summary: We chose a deploy target.
      |**Outcome:** staging deploys from main
      |Standing:
      |- deploy with make stage
      |Open:
      |- prod""".stripMargin

  private val deploy = line(Section.Standing, "deploy with make stage", 1, 1)
  private val prod = line(Section.Open, "prod", 1, 1)

  /** What [[written]] closes an empty balance's period 1 with. */
  private val closing: Closing =
    Closing(
      Flows
        .of(
          "We chose a deploy target.",
          Some("staging deploys from main"),
          Vector(Change.Added(prod), Change.Added(deploy))
        )
        .getOrElse(throw new java.lang.AssertionError("flows")),
      balance(deploy, prod)
    )

  private def answering(text: String) = new Summariser(_ => Right(replyOf(text)))

  /** A gate that says: answered, something standing, something open, nothing settled. */
  private def gate = new Gate(Some(Vector(0.9, 0.9, 0.8, 0.1)))

  /** A gate that says: answered, and nothing new. */
  private def nothingNew = new Gate(Some(Vector(0.9, 0.1, 0.1, 0.1)))

  val tests = Tests {

    test(
      "what the period's windows showed from elsewhere, turns and records, is known to the gate and the writer, once each"
    ) {
      val w = new World
      val api = grit.core.id.ConversationId("api")
      given grit.core.store.Tx = TestTx.fake
      w.entries.insert(
        grit.core.store.Entry(
          EntryId("api:u"),
          api,
          grit.core.id.TurnSeq(0),
          None,
          0,
          Payload.Message(grit.core.message.Message.User("the invoice test is flaky")),
          at(0)
        )
      )
      w.entries.insert(
        grit.core.store.Entry(
          EntryId("api:r"),
          api,
          grit.core.id.TurnSeq(0),
          None,
          1,
          Payload.Message(replyOf("Pin TZ=UTC in the test JVM.")),
          at(0)
        )
      )
      val ops = grit.core.id.ConversationId("ops")
      val freeze =
        line(Section.Standing, "Deploys freeze Friday 17:00", 1, 1, ground = Ground.Person)
      w.entries.insert(
        grit.core.store.Entry(
          EntryId("ops:closing"),
          ops,
          grit.core.id.TurnSeq(3),
          None,
          4,
          Payload.Closed(
            PeriodSeq.First,
            CloseReason.Lapsed,
            Closing(
              Flows.of("Froze deploys.", None, Vector.empty).getOrElse(sys.error("flows")),
              balance(freeze)
            )
          ),
          at(0)
        )
      )
      val place = grit.core.place.Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val thread = grit.core.place.Place.read("slack:T1/C1/2.0").fold(e => sys.error(e), identity)
      def shown(ids: String*) =
        Payload.Window(
          Vector.empty,
          Vector.empty,
          Vector(
            grit.core.store.Nearby.Open(api, place, ids.toVector.map(EntryId(_))),
            grit.core.store.Nearby.Closed(ops, thread, EntryId("ops:closing"))
          )
        )
      val t0 = w.turn(
        "which fix for the flaky test?",
        "TZ=UTC, as settled in api.",
        "Recalled the fix.",
        0
      )
      w.add(t0, shown("api:u", "api:r", "api:gone"), 0, "window:0")
      val t1 = w.turn("and why?", "the CI runs in UTC", "Why UTC.", 1)
      w.add(t1, shown("api:r"), 1, "window:1")
      val summary = answering("Summary: Recalled the fix from api.")
      val g = gate
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(g, summary, new SetClock(at(Lapsed)))
      ) ==>
        "closed: closing:c1:1"
      val lines = Vector(
        "[fs:/home/nick/api] User: the invoice test is flaky",
        "[fs:/home/nick/api] Assistant: Pin TZ=UTC in the test JVM.",
        "[slack:T1/C1/2.0] Record: Froze deploys.",
        "[slack:T1/C1/2.0] Standing: Deploys freeze Friday 17:00"
      )
      g.states.map(_.obj.get("known_elsewhere")) ==> Vector(Some(ujson.Arr.from(lines)))
      summary.requests
        .flatMap(_.messages)
        .map(_.toString)
        .exists(
          _.contains(
            "Known elsewhere (shown from other places; never record it here):\n" + lines
              .mkString("\n") +
              "\n\nTranscript:"
          )
        ) ==> true
    }

    test("a due period is sealed, with its closing entry and the summary's cost") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      val last = w.say("and prod?", 1)
      val summary = answering(written)
      val durable = new InMemoryDurable
      val id = w.attempt.workflowId
      val clock = new SetClock(at(Lapsed))
      durable.run(id)(w.body(gate, summary, clock)) ==> "closed: closing:c1:1"
      summary.requests.size ==> 1
      w.closingEntry.map(e => (e.turnSeq, e.payload, e.createdAt)) ==>
        Some(
          (last.turnSeq, Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, closing), at(Lapsed))
        )
      w.ledger
        .of(id)(using TestTx.fake)
        .map(_.map(r => (EntryId.value(r.entry), r.model, r.usage, r.estimatedInput))) ==>
        Right(
          Vector(("closing:c1:1", "summariser", summaryUsage, Chars.request(summary.requests(0))))
        )
      durable.recordedSteps(id) ==> Vector("check", "gate", "summarise", "seal")
    }

    test("a close grounds Standing from the lines its writer cites, among those it was shown") {
      // u1 the question, t2 a read that showed the port, a3 the reply; then u4 a decision.
      val w = new World
      val t = w.say("what port does the api use?", 0)
      w.add(
        t,
        Payload.Result(
          grit.core.message.Message.ToolResult(grit.core.id.ToolCallId("c1"), "port: 3000", false),
          "read config.yml"
        ),
        0,
        "result:0"
      )
      w.add(t, Payload.Message(replyOf("It uses 3000.")), 0, "reply:0")
      w.say("we keep 3000.", 1)
      val summary = answering(
        "Summary: We checked the port.\nStanding:\n- config.yml sets port 3000 [t2] by tool\n" +
          "- The api stays on 3000 [u4] by person\n- Port 3000 is the usual choice [a3] by assistant"
      )
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, summary, new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      w.closingEntry.map(_.payload) match {
        case Some(Payload.Closed(_, _, c)) =>
          c.balance.in(Section.Standing).map(l => (l.text, l.ground)) ==> Vector(
            ("config.yml sets port 3000", Some(Ground.Tool)),
            ("The api stays on 3000", Some(Ground.Person)),
            ("Port 3000 is the usual choice", Some(Ground.Claimed))
          )
        case other => throw new java.lang.AssertionError(s"no closing: $other")
      }
    }

    test("a period judged finished closes resolved, with the verdict's probability") {
      val w = new World
      val last = w.say("done?", 0)
      val finished = Probability.of(0.9).getOrElse(throw new java.lang.AssertionError("p"))
      val weighed =
        Judgement.Weighed(finished, Probability.Zero, Probability.Zero, "jev")
      w.periods.judged(p1, Verdict(at(61), last.turnSeq, weighed))(using TestTx.fake) ==>
        Right(true)
      val durable = new InMemoryDurable
      durable.run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(62)))
      ) ==>
        "closed: closing:c1:1"
      w.closingEntry.map(_.payload) ==> Some(
        Payload.Closed(PeriodSeq.First, CloseReason.Resolved(finished), closing)
      )
    }

    test(
      "an attempt whose deadline a turn's entries moved is abandoned at the check, however late"
    ) {
      val w = new World
      val t = w.say("hello", 0)
      val made = w.attempt
      w.add(t, Payload.Summary("said hello"), 30, "late")
      val summary = answering(written)
      val g = gate
      new InMemoryDurable().run(made.workflowId)(
        w.body(g, summary, new SetClock(at(3 * Lapsed)))
      ) ==>
        s"abandoned: its deadline moved to ${at(30 + 24 * 60)}"
      (summary.requests.size, g.calls, w.closingEntry) ==> (0, 0, None)
    }

    test("an attempt made before the newest turn came in is abandoned at the check") {
      val w = new World
      w.say("one", 0)
      val made = w.attempt
      w.say("two", 1)
      new InMemoryDurable().run(made.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "abandoned: turn 1 came in"
      w.closingEntry ==> None
    }

    test(
      "a turn that comes in while the summary is written abandons the seal: nothing is written"
    ) {
      val w = new World
      w.say("hello", 0)
      val summary =
        new Summariser(_ => Right(replyOf(written)), () => { w.say("wait, one more", 30); () })
      val id = w.attempt.workflowId
      new InMemoryDurable().run(id)(w.body(gate, summary, new SetClock(at(Lapsed)))) ==>
        "abandoned: a turn came in while it was summarised"
      w.closingEntry ==> None
      w.periods.get(p1)(using TestTx.fake).map(_.map(_.state)) ==> Right(
        Some(PeriodState.Open)
      )
      w.ledger.of(id)(using TestTx.fake) ==> Right(Vector.empty[UsageLedger.Row])
    }

    test("a model that fails leaves the per-turn summaries as the prose, and no section") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      w.turn("and prod?", "later", "Prod is later.", 1)
      val failing = new Summariser(_ => Left(ProviderError.Unavailable("HTTP 503")))
      val id = w.attempt.workflowId
      new InMemoryDurable().run(id)(w.body(gate, failing, new SetClock(at(Lapsed)))) ==>
        "closed: closing:c1:1; no summary: HTTP 503"
      w.closingEntry.map(_.payload) ==> Some(
        Payload.Closed(
          PeriodSeq.First,
          CloseReason.Lapsed,
          TestClosings.prose("Chose staging. Prod is later.")
        )
      )
      w.ledger.of(id)(using TestTx.fake) ==> Right(Vector.empty[UsageLedger.Row])
    }

    test("a close cut short while sealing runs again without calling the summary model again") {
      val w = new World
      w.say("hello", 0)
      val summary = answering(written)
      val durable = new InMemoryDurable
      val id = w.attempt.workflowId
      val crashing = new CrashOnSeal(w.periods)
      val clock = new SetClock(at(Lapsed))
      try { durable.run(id)(w.body(gate, summary, clock, crashing)); () }
      catch { case _: InMemoryDurable.Crash => () }
      w.closingEntry ==> None
      durable.run(id)(w.body(gate, summary, clock, crashing)) ==> "closed: closing:c1:1"
      summary.requests.size ==> 1
    }

    test("the gate's answers choose the parts asked for; without a classifier, every one") {
      val w = new World
      w.say("hello", 0)
      val some = answering("Summary: small talk.")
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(new Gate(Some(Vector(0.1, 0.2, 0.3, 0.6))), some, new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      some.requests.map(_.system) ==> Vector(
        ClosingSummary
          .request(
            PeriodTranscript.labelled(Vector.empty, Speakers.none),
            Balance.empty,
            Vector.empty,
            Asked(false, false, false, true)
          )
          .system
      )

      val v = new World
      v.say("hello", 0)
      val every = answering("Summary: small talk.")
      new InMemoryDurable().run(v.attempt.workflowId)(
        v.body(new Gate(None), every, new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1; gate unavailable: no classifier"
      every.requests.map(_.system) ==>
        Vector(
          ClosingSummary
            .request(
              PeriodTranscript.labelled(Vector.empty, Speakers.none),
              Balance.empty,
              Vector.empty,
              Asked.Every
            )
            .system
        )
    }

    test(
      "nothing new: the balance is carried with no summary, its lines untouched, topics as spoken"
    ) {
      val w = new World
      val t0 = w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      val deployTopic = TopicId("topic:c1:0")
      w.add(
        t0,
        Payload.Topic(
          Vector(
            TopicEvent.Opened(deployTopic),
            TopicEvent.Placed(t0.turnSeq, Weights.whole(deployTopic), Placement.First),
            TopicEvent.Described(deployTopic, "Deploy Target", "where to deploy")
          )
        ),
        0,
        "topic:0"
      )
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      val topic = line(Section.Topics, "Deploy Target", 1, 1)
      val t1 = w.turn("where did we say we deploy?", "staging", "Recalled staging.", Lapsed + 1)
      w.add(
        t1,
        Payload.Topic(
          Vector(
            TopicEvent.Placed(t1.turnSeq, Weights.whole(TopicId.carried(topic.id)), Placement.First)
          )
        ),
        Lapsed + 1,
        "topic:1"
      )
      val p2 = PeriodRef(c, PeriodSeq.First.next)
      val summary = answering("Summary: a recap.")
      new InMemoryDurable().run(w.attemptOn(p2).workflowId)(
        w.body(nothingNew, summary, new SetClock(at(3 * Lapsed)))
      ) ==> "closed: closing:c1:2; nothing new: carried"
      summary.requests.size ==> 0
      w.closingOf(p2).map(_.payload) ==> Some(
        Payload.Closed(
          p2.seq,
          CloseReason.Lapsed,
          Closing(
            Flows
              .of("Recalled staging.", None, Vector())
              .getOrElse(throw new java.lang.AssertionError("f")),
            balance(
              prod,
              deploy,
              line(Section.Topics, "Deploy Target", 1, 2, Some("where to deploy"))
            )
          )
        )
      )
    }

    test("an edit naming no known line is listed as ignored, and the line kept") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      w.turn("prod done?", "yes", "Prod is done.", Lapsed + 1)
      val p2 = PeriodRef(c, PeriodSeq.First.next)
      new InMemoryDurable().run(w.attemptOn(p2).workflowId)(
        w.body(
          new Gate(Some(Vector(0.9, 0.1, 0.1, 0.9))),
          answering("Summary: Prod is done.\nResolved:\n- o7: done\n- o1: shipped on Friday"),
          new SetClock(at(3 * Lapsed))
        )
      ) ==> "closed: closing:c1:2"
      w.closingOf(p2).map(_.payload).collect { case Payload.Closed(_, _, closing) =>
        (closing.flows.changes, closing.balance)
      } ==> Some(
        (
          Vector(
            Change.Ignored("o7: done", "names no line"),
            Change.Resolved(prod, "shipped on Friday")
          ),
          balance(deploy)
        )
      )
    }

    test("a close carries the balance its period opened with, the writer's lines added to it") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      w.turn("what about prod?", "needs a key", "Prod needs a key.", Lapsed + 1)
      val p2 = PeriodRef(c, PeriodSeq.First.next)
      new InMemoryDurable().run(w.attemptOn(p2).workflowId)(
        w.body(
          gate,
          answering("Summary: We looked at prod.\nOpen:\n- prod needs a signing key"),
          new SetClock(at(3 * Lapsed))
        )
      ) ==> "closed: closing:c1:2"
      val key = line(Section.Open, "prod needs a signing key", 2, 2)
      w.closingOf(p2).map(_.payload) ==> Some(
        Payload.Closed(
          p2.seq,
          CloseReason.Lapsed,
          Closing(
            Flows
              .of("We looked at prod.", None, Vector(Change.Added(key)))
              .getOrElse(throw new java.lang.AssertionError("flows")),
            balance(deploy, prod, key)
          )
        )
      )
    }

    test(
      "a seal marks its raw entries, the closing it replaces and its conversation going quiet"
    ) {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      w.tombstones.pending ==>
        Vector(Target.Raw(p1), Target.Quiet(p1)).map(Tombstone(_, at(Lapsed)))
      w.turn("what about prod?", "needs a key", "Prod needs a key.", Lapsed + 1)
      val p2 = PeriodRef(c, PeriodSeq.First.next)
      new InMemoryDurable().run(w.attemptOn(p2).workflowId)(
        w.body(gate, answering(written), new SetClock(at(3 * Lapsed)))
      ) ==> "closed: closing:c1:2"
      w.tombstones.pending.toSet ==> Set(
        Tombstone(Target.Raw(p1), at(Lapsed)),
        Tombstone(Target.Quiet(p1), at(Lapsed)),
        Tombstone(Target.Raw(p2), at(3 * Lapsed)),
        Tombstone(Target.Superseded(p1), at(3 * Lapsed)),
        Tombstone(Target.Quiet(p2), at(3 * Lapsed))
      )
    }

    test("a close holds its balance to the cap in force, refusing its own adds by id") {
      val w = new World
      val d = LifecycleSettings.Default
      // 22 bytes: room for "deploy with make stage" alone. "prod" has the lower id
      // (a7542da0… against d627e55e…), so it is refused first, and that is enough.
      w.lifecycle.set(
        LifecycleSettings
          .of(d.windows, 22, d.settle, d.resolveAt, d.asks, d.locality)
          .getOrElse(throw new java.lang.AssertionError("settings"))
      )(using TestTx.fake)
      w.say("where do we deploy?", 0)
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      w.closingEntry.map(_.payload) ==> Some(
        Payload.Closed(
          PeriodSeq.First,
          CloseReason.Lapsed,
          Closing(
            Flows
              .of(
                "We chose a deploy target.",
                Some("staging deploys from main"),
                Vector(Change.Added(prod), Change.Added(deploy), Change.Refused(prod))
              )
              .getOrElse(throw new java.lang.AssertionError("flows")),
            balance(deploy)
          )
        )
      )
    }

    test(
      "a close adds the topics named in its period with their summaries, and touches carried ones spoken in"
    ) {
      val w = new World
      val t0 = w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      val deployTopic = TopicId("topic:c1:0")
      w.add(
        t0,
        Payload.Topic(
          Vector(
            TopicEvent.Opened(deployTopic),
            TopicEvent.Placed(t0.turnSeq, Weights.whole(deployTopic), Placement.First),
            TopicEvent.Described(deployTopic, "Deploy Target", "where to deploy")
          )
        ),
        0,
        "topic:0"
      )
      new InMemoryDurable().run(w.attempt.workflowId)(
        w.body(gate, answering("Summary: We chose staging."), new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      val topic = line(Section.Topics, "Deploy Target", 1, 1)
      w.closingEntry.map(_.payload).collect { case Payload.Closed(_, _, c) => c.balance } ==>
        Some(balance(line(Section.Topics, "Deploy Target", 1, 1, Some("where to deploy"))))

      val t1 = w.turn("staging again?", "yes", "Staging again.", Lapsed + 1)
      w.add(
        t1,
        Payload.Topic(
          Vector(
            TopicEvent.Placed(t1.turnSeq, Weights.whole(TopicId.carried(topic.id)), Placement.First)
          )
        ),
        Lapsed + 1,
        "topic:1"
      )
      val p2 = PeriodRef(c, PeriodSeq.First.next)
      new InMemoryDurable().run(w.attemptOn(p2).workflowId)(
        w.body(gate, answering("Summary: Staging again."), new SetClock(at(3 * Lapsed)))
      ) ==> "closed: closing:c1:2"
      w.closingOf(p2).map(_.payload).collect { case Payload.Closed(_, _, c) => c.balance } ==>
        Some(balance(line(Section.Topics, "Deploy Target", 1, 2, Some("where to deploy"))))
    }

    test(
      "a summary cut off at its token limit is not read: the fallback prose, the balance carried, its cost kept"
    ) {
      val w = new World
      w.turn("png too?", "yes", "Added PNG.", 0)
      val cut = new Summariser(_ =>
        Right(
          replyOf("Summary: We added PNG.\nStanding:\n- The command now takes `*.").copy(
            stop = StopReason.MaxTokens
          )
        )
      )
      val id = w.attempt.workflowId
      new InMemoryDurable().run(id)(w.body(gate, cut, new SetClock(at(Lapsed)))) ==>
        "closed: closing:c1:1; no summary: cut off at its token limit"
      w.closingEntry.map(_.payload) ==> Some(
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("Added PNG."))
      )
      w.ledger.of(id)(using TestTx.fake).map(_.map(r => (EntryId.value(r.entry), r.usage))) ==>
        Right(Vector(("closing:c1:1", summaryUsage)))
    }

    test("a closed period's attempt says so, and writes nothing") {
      val w = new World
      w.say("hello", 0)
      val made = w.attempt
      val durable = new InMemoryDurable
      durable.run(made.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      )
      val before = w.all
      val second = new InMemoryDurable
      second.run(made.workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed + 5)))
      ) ==>
        "already closed"
      w.all ==> before
    }

    test("an id that is not a close's runs nothing") {
      new InMemoryDurable().run(WorkflowId("c1:0"))(
        new World().body(gate, answering(written), new SetClock(at(0)))
      ) ==> "not a close: c1:0"
    }
  }
}
