package grit.eval

import utest.*

/** The case format, the cases as written, and the filler: no database. */
object CaseTests extends TestSuite {

  private val sample =
    """# what it tests
      |query: one two
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
    test("a case: its turns, its labels, its ask and its query") {
      parsed(sample) ==> Case(
        "sample",
        "what it tests",
        Vector(
          Vector(Case.Line(true, "one", false), Case.Line(false, "two", true)),
          Vector(Case.Line(true, "three", true))
        ),
        "four?",
        Some("one two")
      )
    }

    test("a malformed case is named by line") {
      Case.parse("x", "you: early\n") ==> Left("x line 1: a message before the first turn")
      Case.parse("x", "turn\nme: hi\n") ==>
        Left("x line 2: expected turn, ask, query:, place, here, you: or grit:")
      Case.parse("x", "turn\nask\nturn\n") ==> Left("x line 3: a turn after the ask")
      Case.parse("x", "query: a\nquery: b\n") ==> Left("x line 2: a second query")
      Case.parse("x", "scope fs:/a\nscope fs:/b\n") ==> Left("x line 2: a second scope")
      Case.parse("x", "turn\nyou: a\nask\n") ==> Left("x: the ask must be one unlabelled you: line")
    }

    test(
      "a cross-place case: its other conversations, closed, carried and reopened, and its scope"
    ) {
      val text =
        """query: q
          |scope fs:/home
          |turn
          |you: mine
          |place fs:/home/api
          |turn
          |you: theirs [must]
          |place task:nightly closed
          |turn
          |grit: gone [never]
          |carried: kept line
          |reopen
          |turn
          |you: again
          |here
          |turn
          |you: mine too
          |ask
          |you: now?
          |""".stripMargin
      val c = parsed(text)
      (c.turns.map(_.map(_.text)), c.scope) ==> (
        Vector(Vector("mine"), Vector("mine too")),
        Some("fs:/home")
      )
      c.elsewhere ==> Vector(
        Case.Elsewhere("fs:/home/api", Vector(Vector(Case.Line(true, "theirs", true)))),
        Case.Elsewhere(
          "task:nightly",
          Vector(Vector(Case.Line(false, "gone", false, never = true))),
          closed = true,
          carried = Vector("kept line"),
          reopened = Vector(Vector(Case.Line(true, "again", false)))
        )
      )
    }

    test("every cross-place case parses, has a query, and labels an entry elsewhere") {
      Cases.crossPlace.foreach { (name, text) =>
        val c = Case.parse(name, text).fold(e => sys.error(e), identity)
        val labelled =
          c.elsewhere.flatMap(e => e.turns ++ e.reopened).flatten.exists(l => l.must || l.never)
        assert(c.query.nonEmpty, c.elsewhere.nonEmpty, labelled)
      }
    }

    test("filler is the same for the same seed, and a question and an answer per turn") {
      Filler.turns(7, 5) ==> Filler.turns(7, 5)
      assert(Filler.turns(7, 5) != Filler.turns(8, 5))
      assert(Filler.turns(7, 5).forall(t => t.map(_.you) == Vector(true, false)))
    }
  }
}
