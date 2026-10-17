package grit.outline.query

import grit.outline.locate.Root
import grit.outline.testing.FixtureLayout

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
        Vector("grit.outline.fixture.store.Store"),
        Some("A chain of types, and a store.")
      ),
      Area("colours", Vector("grit.outline.fixture.Colour"), Vector.empty)
    )
  )

  private def area(config: Config, names: Vector[String], level: Int): Answer =
    Query.area(root, FixtureLayout, config, Roots.empty(6000), names, level, 80000)._1

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

    test("an area line's quoted text between its name and its colon is its purpose") {
      val dir = os.temp.dir()
      os.write(dir / ".outline.conf", "area chain \"Where a chain starts.\": X.y\n")
      assert(
        Config.read(Root(dir)) == Right(
          Config(
            Vector.empty,
            Vector(Area("chain", Vector("X.y"), Vector.empty, Some("Where a chain starts.")))
          )
        )
      )
    }

    test("area with no name lists each declared area with its purpose") {
      val answer = area(chain, Vector.empty, 1)
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.drop(1).toVector == Vector(
          "chain  A chain of types, and a store.",
          "colours"
        )
      )
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

    test("an undeclared name that one area's symbols have as a package segment maps to that area") {
      val answer = area(chain, Vector("store"), 0)
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.contains("-- no area store; area chain names store"))
      assert(answer.text.linesIterator.exists(_.startsWith("grit.outline.fixture  ")))
    }

    test("an undeclared name that several areas' symbols have is NoMatch naming those areas") {
      val answer = area(chain, Vector("fixture"), 0)
      assert(answer.status == Status.NoMatch)
      assert(answer.text.contains("no area fixture; areas naming fixture: chain, colours"))
    }

    test(
      "an undeclared name no area has, but a package segment does, lists that package's definitions"
    ) {
      val coloursOnly = Config(Vector.empty, chain.areas.filter(_.name == "colours"))
      val answer = area(coloursOnly, Vector("store"), 0)
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains(
          "-- no area store declared; packages: grit.outline.fixture.store"
        )
      )
      assert(answer.text.linesIterator.exists(_.startsWith("grit.outline.fixture.store  ")))
      assert(!answer.text.linesIterator.exists(_.startsWith("grit.outline.fixture  ")))
    }

    test(
      "an undeclared area name is NoMatch, and the answer names the declared areas under the root line"
    ) {
      val answer = area(chain, Vector("nope"), 1)
      assert(answer.status == Status.NoMatch)
      assert(answer.text.startsWith(s"## root ${root.dir} ("))
      assert(answer.text.contains("no area nope; declared: chain, colours"))
    }
  }
}
