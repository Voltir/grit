package grit.lifecycle.triage

import grit.core.classify.Question
import grit.core.durable.InMemoryDurable
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Decision, Limits, Silence, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{Stitching, Tuning}
import grit.core.triage.{Kind, Tags}
import grit.dbos.sql.TestTx

import utest.*

object TriageTests extends TestSuite {
  import TriageFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  /** A decision at 0.75; waiting 0.125, durable 0.875, helps 0.25. */
  private def decided =
    new Scripted(Vector(0.125, 0.0, 0.75, 0.125, 0.0), Vector(0.125, 0.875, 0.25))

  /** A question at 1.0; waiting 0.5, durable 0.5, helps 0.9. */
  private def asking = new Scripted(Vector(1, 0, 0, 0, 0), Vector(0.5, 0.5, 0.9))

  private val within =
    Speaking.Within(Limits.suggested(DailyCap.of("0.25").getOrElse(sys.error("a cap"))))

  private val decision =
    Tags.Weighed(Kind.Decision, p(0.75), p(0.125), p(0.875), p(0.25), "jev-1.13.0", Spent)

  /** One heard message, as the question is shown it: fixed, so its request's digest is too. */
  private val fixed = TriageQuestion.State(
    "standup moves to 10:00 from Monday",
    "Ana",
    "Ben: when is standup?"
  )

  val tests = Tests {
    test("in the shipped words, judge sends the request it sent before they were a value") {
      // Taken from the request judge sent before its words moved into Wording.Shipped.
      val before = "8ec33547bce9c3e3a9c64edef8eecae7f216738a1407cc8088957d609cb78edd"
      val requests = new Requests
      TriageQuestion.judge(recording(requests), TriageQuestion.Wording.Shipped, fixed)
      requests.sent.map(_.digest) ==> Vector(before)
      TriageQuestion.request(TriageQuestion.Wording.Shipped, fixed) ==> requests.sent.headOption
    }

    test("the state a triage asks about is the one TriageInput.build makes") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      val requests = new Requests
      new InMemoryDurable().run(t.workflowId)(w.body(recording(requests), 5))
      val built = TriageInput.build(w.reads, FakeDb, t, Tuning.Default)
      built.map(_._2.author) ==> Right("Ana")
      requests.sent ==> built.toOption.toVector.flatMap { (_, state) =>
        TriageQuestion.request(TriageQuestion.Wording.Shipped, state)
      }
    }

    test("a heard message is asked once, in one call, and its tags kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val classifier = decided
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 5)) ==>
        """tagged: decision 0.75, durable 0.875 (jev-1.13.0); held: {"kind":"off"}"""
      (classifier.calls, durable.recordedSteps(t.workflowId)) ==>
        (1, Vector("stitch", "ask", "record", "consider"))
      w.tags(t) ==> Some(decision)
    }

    test("a resumed triage asks nothing again: the recorded answer is kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val first = new InMemoryDurable
      first.run(t.workflowId)(w.body(decided, 5))
      val history = first.history(t.workflowId).take(2)
      // Resumed after the ask was recorded: a classifier that now answers otherwise is not called.
      val again = new World
      val t2 = again.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val other = new Scripted(Vector(0, 0, 0, 0, 1), Vector(0, 0, 0))
      new InMemoryDurable().replay(t2.workflowId, history)(again.body(other, 5)) ==>
        Right("""tagged: decision 0.75, durable 0.875 (jev-1.13.0); held: {"kind":"off"}""")
      (other.calls, again.tags(t2)) ==> (0, Some(decision))
    }

    test("a thread too long to show whole keeps what it cut, which joins what it shows") {
      val w = new World
      (0 until 40).foreach(i => w.hear(f"message $i%02d " + "x" * 80, "Ben", i.toLong))
      val t = w.hear("the last", "Ana", 41)
      val read = TriageInput.read(w.reads, FakeDb, t, Tuning.Default)
      read.map(r => (r.state.thread.length, r.state.thread == r.thread.text, r.thread.at)) ==>
        Right((TriageQuestion.ThreadChars, true, 0))
      read.map(r => (r.thread.cut + r.thread.own).take(17)) ==> Right("Ben: message 00 x")
      // Forty lines of "Ben: message nn " and 80 x's, a blank line between each two.
      read.map(r => (r.thread.cut + r.thread.own).length) ==> Right(40 * 96 + 39 * 2)
      read.map(r => (r.entry, r.state)) ==> TriageInput.build(w.reads, FakeDb, t, Tuning.Default)
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

    test("a top-level reply is triaged with the message it follows, and its stitch kept") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      // Exchange 1 at 0.9, then a question at 0.9: the one scripted weight answers both.
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 2))
      durable.recordedSteps(t.workflowId).take(3) ==> Vector("stitch", "record-stitch", "ask")
      classifier.states.lastOption.map(_("thread").str) ==>
        Some("Nick: where did we land on the Engine contract term?")
      w.stitches.links(Vector(b))(using grit.dbos.sql.TestTx.fake) ==>
        Right(Vector(grit.core.stitch.Link(b, c)))
    }

    test("a kept placement's seen state is what offered shows for its message now") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 2))
      val turn = TurnRef(b, t.turn)
      val first = w.entries.list(b)(using TestTx.fake).toOption.flatMap(_.headOption).map(_.id)
      val kept = first.flatMap(id =>
        w.stitches.placed(id)(using TestTx.fake).toOption.flatten.map(_.seen.state)
      )
      val now = Stitching.offered(w.reads, FakeDb, turn, Tuning.Default).map(_.map(Stitching.shown))
      assert(kept.isDefined)
      now ==> Right(kept)
    }

    test("a message that is not its conversation's opening is offered nothing, and not asked") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      w.hear("Is this a real question", "David", 1, in = b)
      val second = w.hear("and another", "David", 2, in = b)
      val turn = TurnRef(b, second.turn)
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      Stitching.offered(w.reads, FakeDb, turn, Tuning.Default) ==> Right(None)
      Stitching.turn(classifier, w.reads, FakeDb, turn, Tuning.Default) ==> Right(None)
      classifier.calls ==> 0
    }

    test("turn asks nothing for a message placed already, which offered still offers") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      new InMemoryDurable().run(t.workflowId)(
        w.body(new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2)), 2)
      )
      val turn = TurnRef(b, t.turn)
      val again = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      Stitching.turn(again, w.reads, FakeDb, turn, Tuning.Default) ==> Right(None)
      again.calls ==> 0
      assert(Stitching.offered(w.reads, FakeDb, turn, Tuning.Default).exists(_.nonEmpty))
    }

    test("an absent classifier's tags are unanswered, and kept") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==>
        """tagged: unanswered: unavailable: no classifier; held: {"kind":"off"}"""
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

    test("a candidate is kept drafting, and its own turn started once, even across a replay") {
      val w = new World
      val t = w.hear("what did we decide about the refi page?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(asking, 1, within)) ==>
        "tagged: question 1.0, durable 0.5 (jev-1.13.0); drafting: c1:0"
      val turn = TurnRef(c, t.turn)
      (w.started, w.speech.decisions.map(_._2)) ==> (Vector(turn), Vector(Decision.Drafting(turn)))
      val history = durable.history(t.workflowId)
      new InMemoryDurable().replay(t.workflowId, history)(w.body(asking, 1, within))
      w.started ==> Vector(turn)
      history.map(_.name) ==> Vector("stitch", "ask", "record", "consider", "start")
    }

    test("a message under helpsAt is kept held, and no turn starts") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(
        w.body(new Scripted(Vector(1, 0, 0, 0, 0), Vector(0.5, 0.5, 0.25)), 1, within)
      )
      (w.started, w.speech.decisions.map(_._2), durable.recordedSteps(t.workflowId)) ==> (
        Vector.empty,
        Vector(Decision.Held(Silence.Below(p(0.25), p(0.6)))),
        Vector("stitch", "ask", "record", "consider")
      )
    }

    test("a deployment that does not speak keeps no decision and starts nothing") {
      val w = new World
      val t = w.hear("what did we decide?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(w.body(asking, 1))
      (w.started, w.speech.decisions) ==> (Vector.empty, Vector.empty)
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
