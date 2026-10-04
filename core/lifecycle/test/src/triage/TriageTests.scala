package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Question}
import grit.core.durable.InMemoryDurable
import grit.core.id.{QuestionName, TurnRef, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Decision, Limits, Silence, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{Stitching, Tuning}
import grit.core.triage.{Bound, Earning, Gate, KnowledgeSources, Reading, Tags}
import grit.dbos.sql.TestTx

import utest.*

object TriageTests extends TestSuite {
  import grit.lifecycle.shadow.ShadowFixtures.{Catalog, Github}

  import TriageFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  private def name(text: String) = QuestionName.read(text).getOrElse(sys.error(text))

  /** v4 answered: gap closes at 0.75; open 0.125, to 0.25, to-grit 0.125, durable 0.875,
    * anchor 0.25, anchor-record 0.375; any source 0.5.
    */
  private def decided =
    new Scripted(
      Vector(0.0, 0.125, 0.75, 0.125),
      Vector(0.125, 0.25, 0.125, 0.875, 0.25, 0.375, 0.5)
    )

  /** v4 answered: gap asks at 1.0; open 0.9, to 0.125, to-grit 0.125, durable 0.5, anchor
    * 0.125, anchor-record 0.125: past its gate.
    */
  private def asking =
    new Scripted(Vector(1, 0, 0, 0), Vector(0.9, 0.125, 0.125, 0.5, 0.125, 0.125))

  private val within =
    Speaking.Within(
      Limits.suggested(
        DailyCap.of("0.25").getOrElse(sys.error("a cap")),
        TriageQuestions.ShippedSpeak
      )
    )

  /** What [[decided]] answers v4 with no source: gap a choice weighing every key, then its
    * yes/nos.
    */
  private val decision: Tags.Weighed = Tags.Weighed(
    VectorMap(
      name("gap") -> Answer
        .choice(
          Vector(
            Answer.Weight("asks", 0.0),
            Answer.Weight("owes", 0.125),
            Answer.Weight("closes", 0.75),
            Answer.Weight("nothing", 0.125)
          )
        )
        .getOrElse(sys.error("a choice")),
      name("open") -> Answer.YesNo(0.125),
      name("to") -> Answer.YesNo(0.25),
      name("to-grit") -> Answer.YesNo(0.125),
      Earning.Durable -> Answer.YesNo(0.875),
      name("anchor") -> Answer.YesNo(0.25),
      name("anchor-record") -> Answer.YesNo(0.375)
    ),
    "jev-1.13.0",
    Spent
  )

  /** What a triage logs of [[decision]]. */
  private val Decided =
    "tagged: gap closes 0.75, open 0.125, to 0.25, to-grit 0.125, durable 0.875, anchor 0.25, anchor-record 0.375 (jev-1.13.0)"

  /** The marker a triage records as it takes the room-order patch. */
  private val Marker = InMemoryDurable.patchMarker(Triage.Patches.StitchInRoomOrder)

  /** What a triage logs of the placement of a room's first opening, which nothing is offered. */
  private val Unoffered = "placed: nothing asked; "

  val tests = Tests {
    test(
      "a triage asks live's question set about the state TriageInput.build makes, with the sources covering its conversation"
    ) {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      val requests = new Requests
      new InMemoryDurable().run(t.workflowId)(w.body(recording(requests), 5, sources = Catalog))
      val read =
        TriageInput.read(w.reads, w.rooms, FakeDb, t.message, Tuning.Default, TriageRecipe.Shipped)
      read.map(_.state.author) ==> Right("Ana")
      requests.sent ==> read.toOption.toVector.map { r =>
        TriageQuestions
          .shipped(grit.core.persona.Persona.Grit)
          .request(
            r.state,
            KnowledgeSources.of(Vector(Github)).getOrElse(sys.error("one"))
          )
      }
    }

    test(
      "live asks v4: gap among four, open, to, to-grit, durable, anchor and anchor-record, then one yes/no per source covering the conversation"
    ) {
      val w = new World
      val t = w.hear("can someone send me the Q3 deck?", "Ana", 0)
      val classifier = decided
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5, sources = Catalog))
      classifier.asked.map {
        case q: Question.Choice => q.keys.map(_.name).mkString("|")
        case Question.YesNo(words, _, _) if words.contains(Github.line) => "github"
        case _: Question.YesNo => "yes/no"
      } ==> Vector(
        "asks|owes|closes|nothing",
        "yes/no",
        "yes/no",
        "yes/no",
        "yes/no",
        "yes/no",
        "yes/no",
        "github"
      )
      w.tags(t).collect { case Tags.Weighed(answers, _, _) =>
        answers.keys.toVector.map(QuestionName.value)
      } ==> Some(
        Vector(
          "gap",
          "open",
          "to",
          "to-grit",
          "durable",
          "anchor",
          "anchor-record",
          "source:github"
        )
      )
    }

    test("a heard message is asked once, in one call, and its tags kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val classifier = decided
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 5)) ==>
        Unoffered + Decided + """; held: {"kind":"off"}"""
      (classifier.calls, durable.recordedSteps(t.workflowId)) ==>
        (1, Vector(Marker, "stitched", "ask", "record", "consider"))
      w.tags(t) ==> Some(decision)
    }

    test("a resumed triage asks nothing again: the recorded answer is kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val first = new InMemoryDurable
      first.run(t.workflowId)(w.body(decided, 5))
      val history = first.history(t.workflowId).take(3)
      // Resumed after the ask was recorded: a classifier that now answers otherwise is not called.
      val again = new World
      val t2 = again.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val other = new Scripted(Vector(0, 0, 0, 0, 1), Vector(0, 0, 0))
      new InMemoryDurable().replay(t2.workflowId, history)(again.body(other, 5)) ==>
        Right(
          Unoffered + Decided + """; held: {"kind":"off"}"""
        )
      (other.calls, again.tags(t2)) ==> (0, Some(decision))
    }

    test("a thread too long to show whole keeps what it cut, which joins what it shows") {
      val w = new World
      (0 until 40).foreach(i => w.hear(f"message $i%02d " + "x" * 80, "Ben", i.toLong))
      val t = w.hear("the last", "Ana", 41)
      val read =
        TriageInput.read(w.reads, w.rooms, FakeDb, t.message, Tuning.Default, TriageRecipe.Shipped)
      read.map(r => (r.state.thread.length, r.state.thread == r.thread.text, r.thread.at)) ==>
        Right((TriageQuestion.ThreadChars, true, 0))
      read.map(r => (r.thread.cut + r.thread.own).take(17)) ==> Right("Ben: message 00 x")
      // Forty lines of "Ben: message nn " and 80 x's, a blank line between each two.
      read.map(r => (r.thread.cut + r.thread.own).length) ==> Right(40 * 96 + 39 * 2)
      read.map(r => (r.entry, r.state)) ==> TriageInput.build(
        w.reads,
        w.rooms,
        FakeDb,
        t.message,
        Tuning.Default,
        TriageRecipe.Shipped
      )
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

    test(
      "a first message's triage waits for its placement, then is asked with the message it follows"
    ) {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      // Exchange 1 at 0.9, then a question at 0.9: the one scripted weight answers both.
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 2))
      durable.recordedSteps(t.workflowId).take(3) ==>
        Vector(InMemoryDurable.patchMarker(Triage.Patches.StitchInRoomOrder), "stitched", "ask")
      w.awaited.map(_.ref.turn) ==> Vector(TurnRef(b, t.turn))
      classifier.states.lastOption.map(_("thread").str) ==>
        Some("Nick: where did we land on the Engine contract term?")
      w.stitches.links(Vector(b))(using grit.dbos.sql.TestTx.fake) ==>
        Right(Vector(grit.core.stitch.Link(b, c)))
    }

    test("a reply's triage waits for no placement") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val reply = w.hear("the twelve-month one", "Ana", 1)
      val durable = new InMemoryDurable
      durable.run(reply.workflowId)(
        w.body(new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2)), 2)
      )
      durable.recordedSteps(reply.workflowId).take(3) ==>
        Vector(InMemoryDurable.patchMarker(Triage.Patches.StitchInRoomOrder), "stitched", "ask")
      w.awaited ==> Vector.empty
    }

    test("a triage that passed the change before it shipped places its first message itself") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      val durable = new InMemoryDurable(Set(Triage.Patches.StitchInRoomOrder))
      durable.run(t.workflowId)(w.body(classifier, 2))
      durable.recordedSteps(t.workflowId).take(3) ==> Vector("stitch", "record-stitch", "ask")
      w.awaited ==> Vector.empty
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

    test("an absent classifier's tags are unanswered, and kept") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==>
        Unoffered + """tagged: unanswered: unavailable: no classifier; held: {"kind":"off"}"""
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
        Unoffered + "ignored: the message is gone or tagged already"
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
      ) ==> Unoffered + "failed: no heard message at turn 0"
      classifier.calls ==> 0
    }

    test("a candidate is kept drafting, and its own turn started once, even across a replay") {
      val w = new World
      val t = w.hear("what did we decide about the refi page?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(asking, 1, within)) ==>
        Unoffered + "tagged: gap asks 1.0, open 0.9, to 0.125, to-grit 0.125, durable 0.5, anchor 0.125, anchor-record 0.125 (jev-1.13.0); drafting: c1:0"
      val turn = TurnRef(c, t.turn)
      (w.started, w.speech.decisions.map(_._2)) ==> (Vector(turn), Vector(Decision.Drafting(turn)))
      val history = durable.history(t.workflowId)
      new InMemoryDurable().replay(t.workflowId, history)(w.body(asking, 1, within))
      w.started ==> Vector(turn)
      history.map(_.name) ==> Vector(Marker, "stitched", "ask", "record", "consider", "start")
    }

    test("a message failing the gate is kept held on every bound it failed, and no turn starts") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(
        w.body(
          new Scripted(Vector(0, 0, 0, 1), Vector(0.25, 0.125, 0.125, 0.5, 0.125, 0.125)),
          1,
          within
        )
      )
      (w.started, w.speech.decisions.map(_._2), durable.recordedSteps(t.workflowId)) ==> (
        Vector.empty,
        Vector(
          Decision.Held(
            Silence.Gated(
              Gate.Failed(Bound.AtLeast(Reading.Key(name("gap"), "asks"), p(0.5)), p(0.0)),
              Vector(Gate.Failed(Bound.AtLeast(Reading.Yes(name("open")), p(0.5)), p(0.25)))
            )
          )
        ),
        Vector(Marker, "stitched", "ask", "record", "consider")
      )
    }

    test("a deployment that does not speak keeps no decision and starts nothing") {
      val w = new World
      val t = w.hear("what did we decide?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(w.body(asking, 1))
      (w.started, w.speech.decisions) ==> (Vector.empty, Vector.empty)
    }
  }
}
