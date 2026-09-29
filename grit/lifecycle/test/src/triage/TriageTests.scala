package grit.lifecycle.triage

import grit.core.classify.Question
import grit.core.durable.InMemoryDurable
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.period.Probability
import grit.core.triage.{Kind, Tags}

import utest.*

object TriageTests extends TestSuite {
  import TriageFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  /** A decision at 0.75; waiting 0.125, durable 0.875, helps 0.25. */
  private def decided =
    new Scripted(Vector(0.125, 0.0, 0.75, 0.125, 0.0), Vector(0.125, 0.875, 0.25))

  private val decision =
    Tags.Weighed(Kind.Decision, p(0.75), p(0.125), p(0.875), p(0.25), "jev-1.13.0", Spent)

  val tests = Tests {
    test("a heard message is asked once, in one call, and its tags kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val classifier = decided
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 5)) ==>
        "tagged: decision 0.75, durable 0.875 (jev-1.13.0)"
      (classifier.calls, durable.recordedSteps(t.workflowId)) ==> (1, Vector("ask", "record"))
      w.tags(t) ==> Some(decision)
    }

    test("a resumed triage asks nothing again: the recorded answer is kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val first = new InMemoryDurable
      first.run(t.workflowId)(w.body(decided, 5))
      val history = first.history(t.workflowId).take(1)
      // Resumed after the ask was recorded: a classifier that now answers otherwise is not called.
      val again = new World
      val t2 = again.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val other = new Scripted(Vector(0, 0, 0, 0, 1), Vector(0, 0, 0))
      new InMemoryDurable().replay(t2.workflowId, history)(again.body(other, 5)) ==>
        Right("tagged: decision 0.75, durable 0.875 (jev-1.13.0)")
      (other.calls, again.tags(t2)) ==> (0, Some(decision))
    }

    test("the classifier is shown the message, who said it, and the thread before it") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      w.hear("after it", "Ben", 3)
      val classifier = decided
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5))
      classifier.states ==> Vector(
        ujson.Obj(
          "new_message" -> "standup moves to 10:00",
          "author" -> "Ana",
          "thread" -> "Ben: when is standup?\n\nUser: unrelated"
        )
      )
    }

    test("an absent classifier's tags are unanswered, and kept") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==>
        "tagged: unanswered: unavailable: no classifier"
      w.tags(t) ==> Some(Tags.Unanswered("unavailable: no classifier"))
    }

    test("a message gone while it was asked keeps no tags") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val purging = new Scripted(
        Vector(0, 0, 0, 0, 1),
        Vector(0.1, 0.1, 0.1),
        () => w.purge(TurnRef(c, t.turn))
      )
      new InMemoryDurable().run(t.workflowId)(w.body(purging, 5)) ==>
        "ignored: the message is gone or tagged already"
      w.tags(t) ==> None
    }

    test("an id that is not a triage's, or names no heard message, asks nothing") {
      val w = new World
      val said = w.say("hello", 0)
      val classifier = decided
      new InMemoryDurable().run(WorkflowId("c1:0"))(w.body(classifier, 0)) ==>
        "not a triage: c1:0"
      new InMemoryDurable().run(grit.core.id.TriageRef(p1, said.turnSeq).workflowId)(
        w.body(classifier, 0)
      ) ==> "failed: no heard message at turn 0"
      classifier.calls ==> 0
    }

    test("the questions: kind among five, then waiting, durable and helps") {
      val w = new World
      val t = w.hear("standup moves to 10:00", "Ana", 0)
      val classifier = decided
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5))
      classifier.asked.map {
        case q: Question.Choice => q.keys.map(_.name).mkString("|")
        case _: Question.YesNo => "yes/no"
      } ==> Vector("question|answer|decision|announcement|chatter", "yes/no", "yes/no", "yes/no")
    }
  }
}
