package grit.lifecycle.close

import grit.core.classify.StateJson
import grit.core.period.{Balance, Section, TestClosings}

import utest.*

object CloseGateTests extends TestSuite {
  import CloseFixtures.Gate

  private val talk = CloseGate.Transcript(Balance.empty, "User: hi\n\nAssistant: hello")

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

    test("the classifier is shown what is already known beside the transcript") {
      val known = TestClosings.balance(
        TestClosings.line(Section.Open, "backup frequency?", 1, 1),
        TestClosings.line(Section.Standing, "exiftool renames photos", 1, 1),
        TestClosings.line(Section.Topics, "Photo Rename", 1, 1)
      )
      StateJson[CloseGate.Transcript].json(CloseGate.Transcript(known, "User: hi")) ==>
        ujson.Obj(
          "already_known" -> ujson.Obj(
            "open" -> ujson.Arr("backup frequency?"),
            "standing" -> ujson.Arr("exiftool renames photos")
          ),
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
