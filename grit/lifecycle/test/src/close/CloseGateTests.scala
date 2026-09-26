package grit.lifecycle.close

import utest.*

object CloseGateTests extends TestSuite {
  import CloseFixtures.Gate

  private val talk = CloseGate.Transcript("User: hi\n\nAssistant: hello")

  val tests = Tests {
    test("each yes asks for its section, sources with decisions or facts") {
      CloseGate.asked(new Gate(Some(Vector(0.9, 0.1, 0.1, 0.6))), talk) ==>
        (
          Asked(outcome = true, decisions = false, facts = false, open = true, sources = false),
          None
        )
      CloseGate.asked(new Gate(Some(Vector(0.1, 0.1, 0.5, 0.1))), talk) ==>
        (
          Asked(outcome = false, decisions = false, facts = true, open = false, sources = true),
          None
        )
    }

    test("small talk asks for no section") {
      CloseGate.asked(new Gate(Some(Vector(0.2, 0.2, 0.2, 0.2))), talk) ==>
        (Asked(false, false, false, false, false), None)
    }

    test("a classifier that does not answer asks for every section, and says why") {
      CloseGate.asked(new Gate(None), talk) ==> (
        Asked.Every,
        Some("gate unavailable: no classifier")
      )
      CloseGate.asked(new Gate(Some(Vector(0.9))), talk) ==>
        (Asked.Every, Some("gate unreadable: 1 answers to 4 questions"))
    }
  }
}
