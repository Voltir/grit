package grit.outline.query

import grit.outline.locate.{MillLayout, Root}

import utest.*

object AreaTests extends TestSuite {

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

  private val chain: Config = Config(
    hidden = Vector.empty,
    areas = Vector(
      Area(
        "chain",
        Vector("grit.outline.fixture.A", "grit.outline.fixture.Colour"),
        Vector("grit.outline.fixture.store.Store")
      )
    )
  )

  private def area(config: Config, names: Vector[String], level: Int): Answer =
    Query.area(root, MillLayout, config, Roots.empty(6000), names, level, 80000)._1

  def tests = Tests {
    test(
      "a .outline.conf's area line reads its symbols and its family items, and a repeated name is named by its line"
    ) {
      val dir = os.temp.dir()
      os.write(dir / ".outline.conf", "hide a/b\narea chain: X.y family:Z.W\n")
      assert(
        Config.read(Root(dir)) == Right(
          Config(Vector("a/b"), Vector(Area("chain", Vector("X.y"), Vector("Z.W"))))
        )
      )
      val repeated = os.temp.dir()
      os.write(repeated / ".outline.conf", "area chain: X.y\narea chain: Q.r\n")
      assert(Config.read(Root(repeated)).left.toOption.exists(_.contains("line 2")))
    }

    test("level 0 lists one line for each package the chain's definitions are in") {
      val text = area(chain, Vector("chain"), 0).text
      assert(text.linesIterator.exists(_.startsWith("grit.outline.fixture  ")))
      assert(text.linesIterator.exists(_.startsWith("grit.outline.fixture.store  ")))
    }

    test(
      "level 1 gives the trait its first doc sentence, and marks its implementations and contracts by role"
    ) {
      val text = area(chain, Vector("chain"), 1).text
      assert(text.linesIterator.contains("6-14 trait A — Where a chain starts: it takes a [[B]]."))
      assert(text.linesIterator.exists(l => l.contains(" impl case class MemStore")))
      assert(text.linesIterator.exists(l => l.contains(" contract class StoreContract")))
    }

    test("an undeclared area name is NoMatch, and the answer names the declared areas") {
      val answer = area(chain, Vector("nope"), 1)
      assert(answer.status == Status.NoMatch)
      assert(answer.text.contains("no area nope; declared: chain"))
    }
  }
}
