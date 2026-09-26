package grit.lifecycle.close

import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, PeriodSeq, WorkflowId}
import grit.core.period.{CloseReason, Closing, PeriodState}
import grit.core.provider.ProviderError
import grit.core.store.{Payload, UsageLedger}
import grit.dbos.sql.TestTx

import utest.*

object CloseTests extends TestSuite {
  import CloseFixtures.*

  /** A day and a minute: past the default idle window after minute 1. */
  private val Lapsed = 24 * 60 + 1L

  private val written =
    """Summary: We chose a deploy target.
      |**Outcome:** staging deploys from main
      |Decisions:
      |- deploy with make stage
      |Open:
      |- prod
      |Sources:
      |- none""".stripMargin

  private val closing: Closing =
    Closing
      .of(
        "We chose a deploy target.",
        Some("staging deploys from main"),
        Vector("deploy with make stage"),
        Vector(),
        Vector("prod"),
        Vector()
      )
      .getOrElse(throw new java.lang.AssertionError("closing"))

  private def answering(text: String) = new Summariser(_ => Right(replyOf(text)))

  /** A gate that says: answered, decided, no fact, something open. */
  private def gate = new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8)))

  val tests = Tests {

    test("a due period is sealed once, with its closing entry and the summary's cost") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      val last = w.say("and prod?", 1)
      val summary = answering(written)
      val durable = new InMemoryDurable
      val id = attempt(last).workflowId
      val clock = new SetClock(at(Lapsed))
      durable.run(id)(w.body(gate, summary, clock)) ==> "closed: closing:c1:1"
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

    test("a signalled period is resolved once the grace window has passed") {
      val w = new World
      val last = w.say("done?", 0)
      w.periods.signal(c, at(5))(using TestTx.fake)
      val durable = new InMemoryDurable
      durable.run(attempt(last).workflowId)(
        w.body(gate, answering(written), new SetClock(at(20)))
      ) ==>
        "closed: closing:c1:1"
      w.closingEntry.map(_.payload) ==> Some(
        Payload.Closed(PeriodSeq.First, CloseReason.Resolved, closing)
      )
    }

    test("a period not yet due is abandoned at the check: nothing is written or called") {
      val w = new World
      val last = w.say("hello", 0)
      val summary = answering(written)
      val g = gate
      new InMemoryDurable().run(attempt(last).workflowId)(
        w.body(g, summary, new SetClock(at(60)))
      ) ==>
        s"abandoned: not due until ${at(24 * 60)}"
      (summary.requests.size, g.calls, w.closingEntry) ==> (0, 0, None)
    }

    test("an attempt made before the newest turn came in is abandoned at the check") {
      val w = new World
      val first = w.say("one", 0)
      w.say("two", 1)
      new InMemoryDurable().run(attempt(first).workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      ) ==> "abandoned: turn 1 came in"
      w.closingEntry ==> None
    }

    test(
      "a turn that comes in while the summary is written abandons the seal: nothing is written"
    ) {
      val w = new World
      val last = w.say("hello", 0)
      val summary =
        new Summariser(_ => Right(replyOf(written)), () => { w.say("wait, one more", 30); () })
      val id = attempt(last).workflowId
      new InMemoryDurable().run(id)(w.body(gate, summary, new SetClock(at(Lapsed)))) ==>
        "abandoned: a turn came in while it was summarised"
      w.closingEntry ==> None
      w.periods.get(p1)(using TestTx.fake).map(_.map(_.state)) ==> Right(
        Some(PeriodState.Open(None))
      )
      w.ledger.of(id)(using TestTx.fake) ==> Right(Vector.empty[UsageLedger.Row])
    }

    test("a model that fails leaves the per-turn summaries as the prose, and no section") {
      val w = new World
      w.turn("where do we deploy?", "staging", "Chose staging.", 0)
      val last = w.turn("and prod?", "later", "Prod is later.", 1)
      val failing = new Summariser(_ => Left(ProviderError.Unavailable("HTTP 503")))
      val id = attempt(last).workflowId
      new InMemoryDurable().run(id)(w.body(gate, failing, new SetClock(at(Lapsed)))) ==>
        "closed: closing:c1:1; no summary: HTTP 503"
      w.closingEntry.map(_.payload) ==> Closing
        .of("Chose staging. Prod is later.", None, Vector(), Vector(), Vector(), Vector())
        .map(Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, _))
      w.ledger.of(id)(using TestTx.fake) ==> Right(Vector.empty[UsageLedger.Row])
    }

    test("a close cut short while sealing runs again without calling the summary model again") {
      val w = new World
      val last = w.say("hello", 0)
      val summary = answering(written)
      val durable = new InMemoryDurable
      val id = attempt(last).workflowId
      val crashing = new CrashOnSeal(w.periods)
      val clock = new SetClock(at(Lapsed))
      try { durable.run(id)(w.body(gate, summary, clock, crashing)); () }
      catch { case _: InMemoryDurable.Crash => () }
      w.closingEntry ==> None
      durable.run(id)(w.body(gate, summary, clock, crashing)) ==> "closed: closing:c1:1"
      summary.requests.size ==> 1
    }

    test("the gate's answers choose the sections asked for; without a classifier, every one") {
      val w = new World
      val last = w.say("hello", 0)
      val none = answering("Summary: small talk.")
      new InMemoryDurable().run(attempt(last).workflowId)(
        w.body(new Gate(Some(Vector(0.1, 0.2, 0.3, 0.4))), none, new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1"
      none.requests.map(_.system) ==> Vector(
        ClosingSummary.request("", Asked(false, false, false, false, false)).system
      )

      val v = new World
      val again = v.say("hello", 0)
      val every = answering("Summary: small talk.")
      new InMemoryDurable().run(attempt(again).workflowId)(
        v.body(new Gate(None), every, new SetClock(at(Lapsed)))
      ) ==> "closed: closing:c1:1; gate unavailable: no classifier"
      every.requests.map(_.system) ==> Vector(ClosingSummary.request("", Asked.Every).system)
    }

    test("a closed period's attempt says so, and writes nothing") {
      val w = new World
      val last = w.say("hello", 0)
      val durable = new InMemoryDurable
      durable.run(attempt(last).workflowId)(
        w.body(gate, answering(written), new SetClock(at(Lapsed)))
      )
      val before = w.all
      val second = new InMemoryDurable
      second.run(attempt(last).workflowId)(
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
