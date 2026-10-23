package grit.prose.index

import grit.prose.form.{Block, Cell, Column, Doc, Text}
import grit.prose.markdown.Markdown

import utest.*

/** The text an index ranks for a doc: its blocks' words a line each, under their heading paths. */
object IndexTextTests extends TestSuite {

  val tests = Tests {

    test("a heading is written as its path, the headings above it joined by ' > '") {
      val doc = Markdown.parse(
        """# Access Control
          |
          |Scope of the family.
          |
          |## AC-2 Account **Management**
          |
          |Manage *accounts*.
          |
          |### AC-2(3) Disable Accounts
          |
          |## AC-3 Access Enforcement
          |
          |# Audit
          |
          |Logs.""".stripMargin
      )
      IndexText.render(doc) ==>
        """Access Control
          |Scope of the family.
          |Access Control > AC-2 Account Management
          |Manage accounts.
          |Access Control > AC-2 Account Management > AC-2(3) Disable Accounts
          |Access Control > AC-3 Access Enforcement
          |Audit
          |Logs.""".stripMargin
    }

    test("a table row is written as column: value pairs, an empty cell left out") {
      val doc = Doc(
        Vector(
          Block.table(
            Vector(Column(Text.plain("Control")), Column(Text.plain("Owner"))),
            Vector(
              Vector(Cell.Words(Text.plain("AC-2")), Cell.Words(Text.plain("IT"))),
              Vector(Cell.Words(Text.plain("AC-3")), Cell.Empty())
            )
          )
        )
      )
      IndexText.render(doc) ==> "Control: AC-2; Owner: IT\nControl: AC-3"
    }

    test("code is its lines without fences, a list item and a quoted block a line each") {
      val doc = Markdown.parse(
        """- first
          |- second
          |
          |> quoted
          |
          |```scala
          |val a = 1
          |val b = 2
          |```
          |
          |---""".stripMargin
      )
      IndexText.render(doc) ==> "first\nsecond\nquoted\nval a = 1 val b = 2"
    }
  }
}
