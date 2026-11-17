package grit.projects.artifact

import utest.*

object BodyTests extends TestSuite {

  private def body(markdown: String): Body =
    Body.read(markdown).fold(why => throw new IllegalArgumentException(why), identity)

  val tests: Tests = Tests {

    test(
      "a section is its `##` heading and what follows, up to the next of rank 2 or less, as markdown"
    ) {
      val b = body(
        "Lead.\n\n## Decision: adopt\n\n- By: Ana\n\n### Why\n\nCheaper.\n\n## Contrast\n\nMore."
      )
      (
        Body.section(b, "Decision: adopt"),
        Body.section(b, "Contrast"),
        Body.section(b, "Why"),
        Body.section(body("No headings."), "Why")
      ) ==> (
        Right("## Decision: adopt\n\n- By: Ana\n\n### Why\n\nCheaper."),
        Right("## Contrast\n\nMore."),
        Left("it has no section titled \"Why\"; its sections: Decision: adopt; Contrast"),
        Left("it has no section titled \"Why\"; it has no sections")
      )
    }

    test("a heading several sections share is refused, naming which of its sections they are") {
      val b = body("## Goal\n\nOne.\n\n## Reads\n\nA.\n\n## Commits\n\nB.\n\n## Reads\n\nC.")
      val refusal =
        "it has 2 sections titled \"Reads\": its sections 2 and 4 of 4"
      (
        Body.section(b, "Reads"),
        Body.edited(b, Body.Edit.Section("Reads", "D."))
      ) ==> (Left(refusal), Left(refusal))
    }

    test("an edit replaces an exact text held once, refused when held twice or not at all") {
      val b = body("One [two](artifact:p/b) three.\n\n## Goal\n\nThree.")
      (
        Body.edited(b, Body.Edit.Text("[two](artifact:p/b) three", "four")),
        Body.edited(b, Body.Edit.Text("Three", "Four")),
        Body.edited(b, Body.Edit.Text("hree", "x")),
        Body.edited(b, Body.Edit.Text("five", "x"))
      ) ==> (
        Right("One four.\n\n## Goal\n\nThree."),
        Right("One [two](artifact:p/b) three.\n\n## Goal\n\nFour."),
        Left("it holds the old text 2 times; give more of the text around it, so it is held once"),
        Left("it does not hold the old text; show it again and copy the text exactly")
      )
    }

    test("an edit replaces a section's blocks whole, its heading kept, its neighbours untouched") {
      val b = body("Lead.\n\n## Goal\n\nOld.\n\n### Detail\n\nGone.\n\n## Reads\n\nKept.")
      (
        Body.edited(b, Body.Edit.Section("Goal", "New, citing [b](artifact:p/b).")),
        Body.edited(b, Body.Edit.Section("Goal", "")),
        Body.edited(b, Body.Edit.Section("Goal", "New.\n\n## Smuggled\n\nX.")),
        Body.edited(b, Body.Edit.Section("Goal", "[b](artifact:nokey)"))
      ) ==> (
        Right("Lead.\n\n## Goal\n\nNew, citing [b](artifact:p/b).\n\n## Reads\n\nKept."),
        Right("Lead.\n\n## Goal\n\n## Reads\n\nKept."),
        Left(
          "a section's new body holds a heading of rank 2 or less, which would start another " +
            "section: \"Smuggled\""
        ),
        Left("a reference is no artifact's key: nokey")
      )
    }

    test("a body is cut at its first rule outside a code block, each side as its markdown") {
      val b = body(
        "**Cap: 9 characters**\n\n```\n***\n```\n\n***\n\nBelow it.\n\n***\n\nAnd more."
      )
      assert(
        Body.aroundRule(b) == Some(
          (
            above = "**Cap: 9 characters**\n\n```\n***\n```",
            below = "Below it.\n\n***\n\nAnd more."
          )
        ),
        Body.aroundRule(body("No rule.\n\n```\n***\n```")) == None
      )
    }
  }
}
