package grit.lifecycle.settle

import grit.core.classify.{Question, TestClassifiers}
import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{Judgement, LifecycleSettings, Probability, Verdict}
import grit.core.spend.DailyCap
import grit.dbos.sql.TestTx

import utest.*

object SettleTests extends TestSuite {
  import SettleFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  /** Nobody waiting 0.85, the person 0.1, something else 0.05. */
  private def finished = new Weigher(Some(Vector(0.85, 0.1, 0.05)))

  val tests = Tests {
    test("a quiet period is asked once, and its verdict kept") {
      val w = new World
      val t = w.turn("deploy staging?", 0)
      val weigher = finished
      val durable = new InMemoryDurable
      val id = w.question.workflowId
      durable.run(id)(w.body(weigher, 90)) ==> "judged: nobody 0.85 (jev)"
      weigher.calls ==> 1
      durable.recordedSteps(id) ==> Vector("check", "ask:settle", "ask:settle:record", "record")
      w.activity.map(a => (a.verdict, a.asked)) ==> Some(
        (
          Some(
            Verdict(at(90), t.turnSeq, Judgement.Weighed(p(0.85), p(0.1), p(0.05), "jev"))
          ),
          1
        )
      )
    }

    test("a question about a period active since it was made is abandoned, asking nothing") {
      val w = new World
      w.turn("one", 0)
      val made = w.question
      w.turn("two", 5)
      val weigher = finished
      new InMemoryDurable().run(made.workflowId)(w.body(weigher, 90)) ==>
        "abandoned: turn 1 came in"
      (weigher.calls, w.activity.flatMap(_.verdict)) ==> (0, None)
      // Written to in the same turn: the turn is the question's, the quiet stretch is not.
      val same = new World
      val t0 = same.turn("one", 0)
      val asked = same.question
      same.more(t0, "and another thing", 5)
      new InMemoryDurable().run(asked.workflowId)(same.body(weigher, 90)) ==>
        s"abandoned: active since, at ${at(5)}"
      (weigher.calls, same.activity.flatMap(_.verdict)) ==> (0, None)
    }

    // A settle's ask is a move (ADR 0034): admitted under the deployment's daily cap against
    // what every conversation spent that day, as a job's is.
    test("a settle over the day's cap asks no classifier: its verdict is unanswered, for the cap") {
      val w = new World
      val t = w.turn("hello", 0)
      w.ledger.now = at(90)
      w.ledger.record(
        EntryId("elsewhere"),
        TurnRef(ConversationId("c2"), TurnSeq(0)),
        WorkflowId("c2:0"),
        "m",
        None,
        None,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(1))),
        Tokens(1)
      )(using TestTx.fake)
      val weigher = finished
      val capped = "unavailable: The day's spend reached its cap, so no model was asked."
      new InMemoryDurable().run(w.question.workflowId)(
        w.body(weigher, 90, cap = DailyCap.of("1").toOption)
      ) ==> s"judged: unanswered: $capped"
      weigher.calls ==> 0
      w.activity.flatMap(_.verdict) ==>
        Some(Verdict(at(90), t.turnSeq, Judgement.Unanswered(capped)))
    }

    test("a settle's classifier call is kept in the ledger once, under its own workflow") {
      val w = new World
      val t = w.turn("deploy staging?", 0)
      val id = w.question.workflowId
      val durable = new InMemoryDurable
      durable.run(id)(w.body(finished, 90))
      def rows(of: WorkflowId) =
        w.ledger.of(of)(using TestTx.fake).map(_.map(r => (r.entry, r.model, r.upstream, r.usage)))
      rows(id) ==> Right(
        Vector(
          (
            EntryId(s"move:${WorkflowId.value(id)}:settle"),
            "jev",
            Some(TestClassifiers.Service.place.written),
            Usage(Tokens(1), Tokens(1), Tokens.Zero, None)
          )
        )
      )
      rows(t.workflowId) ==> Right(Vector())
    }

    test("an absent classifier is a verdict too, and the same quiet stretch is not asked again") {
      val w = new World
      val t = w.turn("hello", 0)
      val absent = new Weigher(None)
      new InMemoryDurable().run(w.question.workflowId)(w.body(absent, 90)) ==>
        "judged: unanswered: unavailable: no classifier"
      w.activity.map(a => (a.verdict, a.asked)) ==> Some(
        (Some(Verdict(at(90), t.turnSeq, Judgement.Unanswered("unavailable: no classifier"))), 1)
      )
      w.activity.flatMap(_.asks(LifecycleSettings.Default)) ==> None
      new InMemoryDurable().run(w.question.workflowId)(w.body(absent, 95)) ==>
        "abandoned: not to be asked: judged already, out of asks, or asking is off"
      absent.calls ==> 1
    }

    test(
      "a classifier whose service is not cleared for what the settle read is not asked: unanswered"
    ) {
      val w = new World
      val _ = w.turn("hello", 0)
      val weigher = new Weigher(Some(Vector(0.9, 0.1, 0.0)))
      new InMemoryDurable().run(w.question.workflowId)(
        w.body(weigher, 90, grit.core.classify.TestClassifiers.above())
      ) ==>
        s"judged: unanswered: unavailable: ${grit.core.classify.TestClassifiers.Uncleared}"
      weigher.calls ==> 0
    }

    test("a turn that comes in while the classifier is asked leaves no verdict") {
      val w = new World
      w.turn("hello", 0)
      val interrupted =
        new Weigher(Some(Vector(0.9, 0.1, 0.0)), () => { w.turn("wait", 80); () })
      new InMemoryDurable().run(w.question.workflowId)(w.body(interrupted, 90)) ==>
        "ignored: a turn came in while it was asked"
      w.activity.map(a => (a.verdict, a.asked)) ==> Some((None, 0))
    }

    test("the question is whether anyone is waiting: nobody, the person, or something else") {
      val w = new World
      w.turn("what format did we pick?", 0)
      val weigher = finished
      new InMemoryDurable().run(w.question.workflowId)(w.body(weigher, 90))
      weigher.asked.collect { case q: Question.Choice => (q.instructions, q.keys.map(_.name)) } ==>
        Vector(
          (
            "Read transcript. The conversation has gone quiet. Is anyone waiting on anything?",
            Vector("nobody", "waiting_on_person", "waiting_on_other")
          )
        )
    }

    test("an id that is not a settle's runs nothing") {
      new InMemoryDurable().run(WorkflowId("c1:0"))(new World().body(finished, 0)) ==>
        "not a settle: c1:0"
    }
  }
}
