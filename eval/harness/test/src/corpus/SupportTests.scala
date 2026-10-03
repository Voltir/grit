package grit.eval.harness.corpus

import utest.*

/** How much of a reply a shown part carries: its distinctive words, less the asked message's. */
object SupportTests extends TestSuite {

  val tests = Tests {
    test(
      "a numeral, a name and a long word the part shows count, and a stop word or a short one does not"
    ) {
      // Distinctive: moved, friday, 10, agreed ("The" and "the" are stop words, "team" short).
      Support
        .of(
          "The deploy moved to Friday at 10 and the team agreed",
          "when is the deploy?",
          "Friday standup: the team agreed on 10"
        )
        .value ==> 0.75
    }

    test("the asked message's words are not the reply's to be supported") {
      Vector("when is it, Friday?", "when is it?").map(asked =>
        Support.of("Friday", asked, "moved to Friday").value
      ) ==> Vector(0.0, 1.0)
    }

    test("words are compared case-folded, and a reply with no distinctive word has none") {
      Vector(
        Support.of("BUDGET approved", "", "the budget was approved").value,
        Support.of("ok, sure, yes", "", "ok sure yes").value
      ) ==> Vector(1.0, 0.0)
    }

    test("support is read only within [0, 1]") {
      Vector(-0.1, 0.0, 1.0, 1.1).map(Support.read(_).map(_.value)) ==>
        Vector(None, Some(0.0), Some(1.0), None)
    }
  }
}
