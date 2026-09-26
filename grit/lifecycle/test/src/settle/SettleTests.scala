package grit.lifecycle.settle

import grit.core.classify.Question
import grit.core.durable.InMemoryDurable
import grit.core.id.WorkflowId
import grit.core.period.{Judgement, LifecycleSettings, Probability, Verdict}

import utest.*

object SettleTests extends TestSuite {
  import SettleFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  /** Nobody waiting 0.85, the person 0.1, something else 0.05. */
  private def finished = new Weigher(Some(Vector(0.85, 0.1, 0.05)))

  val tests = Tests {
    test("a quiet period is asked once, and its verdict kept; a rerun asks nothing") {
      val w = new World
      val t = w.turn("deploy staging?", 0)
      val weigher = finished
      val durable = new InMemoryDurable
      val id = w.question.workflowId
      durable.run(id)(w.body(weigher, 90)) ==> "judged: nobody 0.85 (jev)"
      durable.run(id)(w.body(weigher, 95)) ==> "judged: nobody 0.85 (jev)"
      weigher.calls ==> 1
      durable.recordedSteps(id) ==> Vector("check", "ask", "record")
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
