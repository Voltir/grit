package grit.assembly.eval

import grit.core.id.EntryId
import grit.core.message.Tokens

import utest.*

/** The eval's harness: the case format, the store a case is written into, and the score.
  * The scores themselves are the report's business, not a test's.
  */
object EvalTests extends TestSuite {

  private val sample =
    """# what it tests
      |turn
      |you: one
      |grit: two [must]
      |turn
      |you: three [must]
      |ask
      |you: four?
      |""".stripMargin

  private def parsed(text: String): Case = Case.parse("sample", text).fold(sys.error, identity)

  val tests = Tests {
    test("a case: its turns, its labels, its ask") {
      parsed(sample) ==> Case(
        "sample",
        "what it tests",
        Vector(
          Vector(Case.Line(true, "one", false), Case.Line(false, "two", true)),
          Vector(Case.Line(true, "three", true))
        ),
        "four?"
      )
    }

    test("a malformed case is named by line") {
      Case.parse("x", "you: early\n") ==> Left("x line 1: a message before the first turn")
      Case.parse("x", "turn\nme: hi\n") ==> Left("x line 2: expected turn, ask, you: or grit:")
      Case.parse("x", "turn\nask\nturn\n") ==> Left("x line 3: a turn after the ask")
      Case.parse("x", "turn\nyou: a\nask\n") ==> Left("x: the ask must be one unlabelled you: line")
      Case.parse("x", "turn\nask\nyou: a [must]\n") ==> Left(
        "x: the ask must be one unlabelled you: line"
      )
    }

    test("a loaded case: one entry per line, the ask recorded as the turn assembled for") {
      val loaded = Eval.load(parsed(sample))
      loaded.must.map(EntryId.value) ==> Vector("t0:1", "t1:2")
      loaded.messages.keySet.map(EntryId.value) ==> Set("t0:0", "t0:1", "t1:2", "t2:3")
    }

    test("the score counts labelled entries held, the window's tokens, and what was missed") {
      val loaded = Eval.load(parsed(sample))
      // "two" is 4 + 1 tokens, "one" 4 + 1 (CharEstimate).
      Eval.score(loaded, Vector(EntryId("t0:0"), EntryId("t0:1"))) ==>
        Eval.Score(1, 2, Tokens(10), Vector(EntryId("t1:2")))
      Eval.run(loaded, Eval.Strategy.Oracle).map(_.got) ==> Right(2)
    }

    test("every case parses, labels something before its ask, and linear sees it all at 24k") {
      Cases.all.foreach { (name, text) =>
        val c = Case.parse(name, text).fold(e => sys.error(e), identity)
        assert(c.turns.flatten.exists(_.must))
        val all = Eval.run(Eval.load(c), Eval.Strategy.Linear(Tokens(24_000)))
        assert(all.exists(s => s.got == s.of))
      }
    }
  }
}
