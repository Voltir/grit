package grit.outline.query

import grit.outline.cli.Main
import grit.outline.locate.{Layout, Locate, MillLayout, Root}

import utest.*

object QueryTests extends TestSuite {

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

  def tests = Tests {
    test("show A.keep at depth 1 is Found, with its signature and the B it traces to") {
      val (answer, _) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("A.keep"),
          1,
          Set.empty,
          false,
          80000
        )
      assert(answer.status == Status.Found)
      assert(answer.text.contains("9-10 def A.keep"))
      assert(answer.text.contains("→ 16-17 final case class B(c: C, note: String)"))
    }

    test("show of a name nothing defines is NoMatch, saying so and nothing else") {
      val (answer, _) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("Nope"),
          1,
          Set.empty,
          false,
          80000
        )
      assert(answer.status == Status.NoMatch)
      assert(answer.text.startsWith(s"## root ${root.dir} ("))
      assert(answer.text.endsWith("\nno match for Nope"))
    }

    test("a second read of unchanged tasty reads no files") {
      val (_, first) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("A.keep"),
          1,
          Set.empty,
          false,
          80000
        )
      val tasty = Locate.forTopLevel(root, MillLayout, "A.keep")
      Loaded.defns(root, MillLayout, tasty, Roots.of(first, root)) match {
        case Right((_, _, read)) => assert(read == 0)
        case Left(message) => throw new Exception(message)
      }
    }

    test("the CLI exits 2 for a depth outside 0..2, and 1 for a name nothing defines") {
      val (tooDeep, _) = Main.run(Vector("show", "A.keep", "--depth", "5"), root.dir)
      assert(tooDeep == 2)
      val (noMatch, _) = Main.run(Vector("show", "Nope"), root.dir)
      assert(noMatch == 1)
    }

    test("a corrupt .tasty is named alone by its path, and the definitions beside it still load") {
      val classes = os.temp.dir(prefix = "outline-bisect-") / "classes"
      os.copy(
        root.dir / "out" / "grit" / "outline" / "fixture" / "compile.dest" / "classes",
        classes,
        createFolders = true
      )
      os.write(classes / "grit" / "outline" / "fixture" / "Bad.tasty", "not a tasty file")
      val layout = new Layout {
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val tasty = os.walk(classes).filter(_.ext == "tasty").toVector
      val (defns, _, notes) = Query.loadAll(root, layout, tasty, Loaded.empty)
      assert(defns.exists(_.fullName == "grit.outline.fixture.A"))
      assert(notes.size == 1)
      assert(notes.head == "-- not loaded: grit/outline/fixture/Bad.tasty")
    }
  }
}
