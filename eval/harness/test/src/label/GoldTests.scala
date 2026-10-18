package grit.eval.harness.label

import utest.*

/** A reply's text matched against a question's gold. */
object GoldTests extends TestSuite {

  private def gold(never: Vector[String], facts: ::[String]*): Gold =
    facts.toList match {
      case first :: rest => Gold(::(Gold.Fact(first), rest.map(Gold.Fact(_))), never)
      case Nil => sys.error("a gold needs a fact")
    }

  val tests = Tests {
    test(
      "a fact is held when any alternative is, ignoring case, whitespace, marks, quotes and dashes"
    ) {
      val g = gold(
        Vector.empty,
        ::("Start.command", Nil),
        ::("append-only", List("never edited")),
        ::("the lead's row", Nil)
      )
      g.matched(
        "Double-click **`start.COMMAND`**; migrations are\n  append–only, and the lead’s  row stays."
      ) ==> Gold.Matched(3, 3, 0)
    }

    test("an alternative continued by a letter or digit is not held, and $ alone is held in $500") {
      val g = gold(Vector("$"), ::("30 days", Nil), ::("$31", Nil), ::("pt", Nil))
      Vector(
        g.matched("after 130 days, $310 a month, in PTO"),
        g.matched("after 30 days, $31 a month, 3:30 PT"),
        g.matched("budget: $500")
      ) ==> Vector(Gold.Matched(0, 3, 1), Gold.Matched(3, 3, 1), Gold.Matched(0, 3, 1))
    }

    test("a reply holding a never-string is not correct though every fact is held") {
      val g = gold(Vector("per month"), ::("no budget", List("doesn't specify")))
      Vector(
        g.matched("There is no budget.").correct,
        g.matched("No budget is named; I'd guess $200 per month.").correct
      ) ==> Vector(true, false)
    }
  }
}
