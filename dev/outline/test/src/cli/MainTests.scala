package grit.outline.cli

import grit.outline.query.Help

import utest.*

object MainTests extends TestSuite {

  def tests = Tests {

    test("tests --help prints the tests help, not show's usage, and exits 0") {
      val (code, text) = Main.run(Vector("tests", "--help"), os.pwd)
      assert(code == 0)
      assert(text.contains("To add or change a test in a suite"))
      assert(!text.startsWith("usage: show"))
    }

    test("no arguments print one line per query with its purpose, and exit 0") {
      val (code, text) = Main.run(Vector.empty, os.pwd)
      assert(code == 0)
      val lines = text.linesIterator.toVector
      Help.entries.foreach { e =>
        assert(lines.exists(l => l.startsWith(s"  ${e.query} ") && l.endsWith(e.purpose)))
      }
    }

    test("show --help prints its usage, purpose, examples and flags, and exits 0") {
      val (code, text) = Main.run(Vector("show", "--help"), os.pwd)
      assert(code == 0)
      assert(text.startsWith("usage: show Sym[,Sym…] [--depth 0|1|2]"))
      assert(text.contains("scripts/outline show Moves.ask"))
      assert(text.contains("there is no `all` or `full`"))
    }
  }
}
