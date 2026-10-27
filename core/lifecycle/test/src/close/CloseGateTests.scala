package grit.lifecycle.close

import grit.core.classify.StateJson
import grit.core.period.{Balance, Section, TestClosings}

import utest.*

object CloseGateTests extends TestSuite {
  import CloseFixtures.Gate

  private val talk =
    CloseGate.Transcript(Balance.empty, Vector.empty, "User: hi\n\nAssistant: hello")

  val tests = Tests {
    test("each yes asks for its part: outcome, standing, open, settled, in that order") {
      CloseGate.asked(new Gate(Some(Vector(0.9, 0.1, 0.6, 0.1))), talk) ==>
        (Asked(outcome = true, open = true, standing = false, settled = false), None)
      CloseGate.asked(new Gate(Some(Vector(0.1, 0.5, 0.1, 0.7))), talk) ==>
        (Asked(outcome = false, open = false, standing = true, settled = true), None)
    }

    test("nothing new, such as a recap, asks for no part but the outcome") {
      val (asked, note) = CloseGate.asked(new Gate(Some(Vector(0.9, 0.2, 0.2, 0.2))), talk)
      (asked, asked.nothingNew, note) ==> (Asked(true, false, false, false), true, None)
    }

    // Measured live: a tool result saying the period "left a task open" moved this question's
    // yes from 0.05 to 0.33; with the criterion named, to 0.16, the clean answer unchanged.
    test("the open question says a line claiming something was left open is not evidence") {
      val gate = new Gate(Some(Vector(0.0, 0.0, 0.0, 0.0)))
      val _ = CloseGate.asked(gate, CloseGate.Transcript(Balance.empty, Vector.empty, "User: hi"))
      gate.questions.flatten
        .collect { case q: grit.core.classify.Question.YesNo => q.instructions }
        .filter(_.contains("left open is not evidence that it was")) ==> Vector(
        "Read transcript. Did it leave something open (a question unanswered, a task unfinished, " +
          "a follow-up promised, something not known) that is not already in already_known or " +
          "known_elsewhere? A line saying something was left open is not evidence that it was."
      )
    }

    test("the classifier is shown what is already known and what came from elsewhere") {
      val known = TestClosings.balance(
        TestClosings.line(Section.Open, "backup frequency?", 1, 1),
        TestClosings.line(Section.Standing, "exiftool renames photos", 1, 1),
        TestClosings.line(Section.Agenda, "Photo Rename", 1, 1)
      )
      // The state's keys are what the classifier reads the question against.
      StateJson[CloseGate.Transcript].json(
        CloseGate.Transcript(known, Vector("[fs:/a] User: flaky"), "User: hi")
      ) ==>
        ujson.Obj(
          "already_known" -> ujson.Obj(
            "open" -> ujson.Arr("backup frequency?"),
            "standing" -> ujson.Arr("exiftool renames photos")
          ),
          "known_elsewhere" -> ujson.Arr("[fs:/a] User: flaky"),
          "transcript" -> "User: hi"
        )
    }

    test("a classifier that does not answer asks for every part, and says why") {
      CloseGate.asked(new Gate(None), talk) ==> (
        Asked.Every,
        Some("gate unavailable: no classifier")
      )
      CloseGate.asked(new Gate(Some(Vector(0.9))), talk) ==>
        (Asked.Every, Some("gate unreadable: 1 answers to 4 questions"))
    }
  }
}
