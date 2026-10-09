package grit.outline.query

import grit.outline.locate.{MillLayout, Root}

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

  private def answer(name: String, test: Option[String]): Answer =
    Query.tests(root, MillLayout, Config.empty, Roots.empty(6000), name, test, 80000)._1

  def tests = Tests {
    test(
      "the suite's helper is fresh with its doc's line, and its two tests are listed with their ranges; run and the private test are not helpers"
    ) {
      val lines = answer(suite, None).text.linesIterator.toVector
      assert(lines.contains("  65-66 def fresh(): Store"))
      assert(lines.contains("  tests (2):"))
      assert(lines.contains("    69-71 put then get returns the value"))
      assert(lines.contains("    72-74 get of an absent key is None"))
      assert(!lines.exists(_.contains("def run")))
      assert(!lines.exists(_.contains("def test")))
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
  }
}
