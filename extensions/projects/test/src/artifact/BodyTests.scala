package grit.projects.artifact

import grit.prose.form.{Block, Text}

import utest.*

object BodyTests extends TestSuite {

  private def body(markdown: String): Body =
    Body.read(markdown).fold(why => throw new IllegalArgumentException(why), identity)

  val tests: Tests = Tests {

    test("a section is the blocks under its `##` heading, up to the next of rank 2 or less") {
      val b = body(
        "Lead.\n\n## Decision: adopt\n\n- By: Ana\n\n### Why\n\nCheaper.\n\n## Contrast\n\nMore."
      )
      val decided = Body.section(b, "Decision: adopt")
      val contrast = Body.section(b, "Contrast")
      assert(
        decided.map(_.size) == Some(3),
        decided.flatMap(_.lift(2)) == Some(Block.Paragraph(Text.plain("Cheaper."))),
        contrast == Some(Vector(Block.Paragraph(Text.plain("More.")))),
        Body.section(b, "Why") == None,
        Body.section(b, "Verdict") == None
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
