package grit.outline.query

import grit.outline.locate.{Layout, MillLayout, Root}
import grit.outline.testing.FixtureLayout

import utest.*

object TestsTests extends TestSuite {

  /** The worktree root: the nearest directory at or above the test's working directory that holds `build.mill`. */
  private def repoRoot: os.Path = {
    def up(dir: os.Path): os.Path =
      if (os.exists(dir / "build.mill")) dir
      else {
        val parent = dir / os.up
        if (parent == dir) throw new Exception("no build.mill above the test's working directory")
        else up(parent)
      }
    up(os.pwd)
  }

  private lazy val root: Root = Root(repoRoot)

  private val suite = "grit.outline.fixture.store.StoreSuite"

  /** The `tests` answer for `name`, over the fixture's classes unless `layout` says otherwise. */
  private def answer(name: String, test: Option[String], layout: Layout = FixtureLayout): Answer =
    Query.tests(root, layout, Config.empty, Roots.empty(6000), name, test, 80000)._1

  def tests = Tests {
    test(
      "the suite's helper is fresh with its doc's line, and its two tests are listed with their ranges; run, which holds tests, is not a helper"
    ) {
      val lines = answer(suite, None).text.linesIterator.toVector
      assert(lines.contains("  65-66 def fresh(): Store"))
      assert(lines.contains("  tests (2):"))
      assert(lines.contains("    69-71 put then get returns the value"))
      assert(lines.contains("    72-74 get of an absent key is None"))
      assert(!lines.exists(_.contains("def run")))
      assert(lines.contains("  63-63 private def test(name: String)(body: => Unit): Unit"))
    }

    test("a test prefix prints the verbatim body of the tests it starts, and no other") {
      val text = answer(suite, Some("get of")).text
      assert(text.linesIterator.contains("      assert(fresh().get(\"x\").isEmpty)"))
      assert(!text.contains("assert(fresh().put("))
      assert(!text.contains("-- no test starting"))
    }

    test("a test prefix that starts no test is named in a note") {
      assert(answer(suite, Some("nope")).text.contains("-- no test starting \"nope\""))
    }

    test("a name no class or object declares is NoMatch") {
      assert(answer("grit.outline.fixture.store.Missing", None).status == Status.NoMatch)
    }

    test("an ambiguous suite name is NoMatch, names every suite it matches, and prints none") {
      val found = answer("Twin", None)
      assert(found.status == Status.NoMatch)
      assert(
        found.text.contains(
          "-- ambiguous Twin: grit.outline.fixture.Twin, grit.outline.fixture.store.Twin (name one fully)"
        )
      )
      assert(!found.text.contains("== "))
    }

    test("tests lists the suite's private helpers and its file's other top-level definitions") {
      val lines =
        answer("grit.outline.render.OneLineTests", None, MillLayout).text.linesIterator.toVector
      assert(lines.exists(_.trim.matches("""\d+-\d+ private val posed.*""")))
      assert(lines.exists(_.trim.matches("""\d+-\d+ private val companion.*""")))
    }

    test("tests --test prints the test's own lines") {
      val name =
        "a trait's one-liner lists each implementor as its bare constructor, without modifiers, extends clause or doc"
      val lines =
        answer(
          "grit.outline.render.OneLineTests",
          Some("a trait's one-liner lists each"),
          MillLayout
        ).text
          .split("\n", -1)
          .toVector
      val header = lines.find(l => l.startsWith("    ") && l.endsWith(s" $name")).getOrElse("")
      val Span = raw"    (\d+)-(\d+) .*".r
      val (from, to) = header match {
        case Span(a, b) => (a.toInt, b.toInt)
        case _ => (0, 0)
      }
      val printed = lines
        .dropWhile(_ != header)
        .drop(1)
        .takeWhile(l => !(l.startsWith("    ") && l.trim.headOption.exists(_.isDigit)))
        .takeWhile(!_.startsWith("["))
      val file = os.read
        .lines(root.dir / "dev" / "outline" / "test" / "src" / "render" / "OneLineTests.scala")
        .toVector
      assert(from > 0)
      assert(printed == file.slice(from - 1, to))
    }
  }
}
