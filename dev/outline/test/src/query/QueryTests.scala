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
      assert(answer.text.endsWith("\n-- no match for Nope"))
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

    test("a --body name matching nothing shown is named in a note, with the names shown") {
      val (answer, _) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("A.keep"),
          1,
          Set("nope", "keep"),
          false,
          80000
        )
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains("-- --body nope matches nothing shown; names here: keep")
      )
      assert(!answer.text.contains("--body keep matches"))
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

    test(
      "a file the inspector failed on is remembered, so a second load reads no files and still names it"
    ) {
      val classes = os.temp.dir(prefix = "outline-remember-") / "classes"
      os.copy(
        root.dir / "out" / "grit" / "outline" / "fixture" / "compile.dest" / "classes",
        classes,
        createFolders = true
      )
      val bad = classes / "grit" / "outline" / "fixture" / "Bad.tasty"
      os.write(bad, "not a tasty file")
      val layout = new Layout {
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val tasty = os.walk(classes).filter(_.ext == "tasty").toVector
      val (_, first, firstNotes) = Query.loadAll(root, layout, tasty, Loaded.empty)
      assert(firstNotes == Vector("-- not loaded: grit/outline/fixture/Bad.tasty"))
      assert(first.failed.keySet == Set(bad))
      val (defns, second, secondNotes) = Query.loadAll(root, layout, tasty, first)
      assert(Loaded.defns(root, layout, tasty, second).map(_._3) == Right(0))
      assert(secondNotes == Vector("-- not loaded: grit/outline/fixture/Bad.tasty"))
      assert(defns.exists(_.fullName == "grit.outline.fixture.A"))
    }

    test("a remembered failure is tried again once the file's mtime changes") {
      val classes = os.temp.dir(prefix = "outline-retry-") / "classes"
      os.copy(
        root.dir / "out" / "grit" / "outline" / "fixture" / "compile.dest" / "classes",
        classes,
        createFolders = true
      )
      val bad = classes / "grit" / "outline" / "fixture" / "Bad.tasty"
      os.write(bad, "not a tasty file")
      val layout = new Layout {
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val tasty = os.walk(classes).filter(_.ext == "tasty").toVector
      val (_, first, _) = Query.loadAll(root, layout, tasty, Loaded.empty)
      val touched = os.mtime(bad) + 60000L
      java.nio.file.Files.setLastModifiedTime(
        bad.toNIO,
        java.nio.file.attribute.FileTime.fromMillis(touched)
      )
      val (_, next, notes) = Query.loadAll(root, layout, tasty, first)
      assert(next.failed.get(bad) == Some(touched))
      assert(notes == Vector("-- not loaded: grit/outline/fixture/Bad.tasty"))
    }

    test("show of a test suite's name names the tests query") {
      val (answer, _) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("OneLineTests"),
          1,
          Set.empty,
          false,
          80000
        )
      assert(answer.status == Status.NoMatch)
      assert(
        answer.text.endsWith(
          "\n-- OneLineTests is in test sources: tests grit.outline.render.OneLineTests"
        )
      )
    }

    test("show of a helper inside a suite names its suite") {
      val (answer, _) =
        Query.show(
          root,
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("OneLineTests.posed"),
          1,
          Set.empty,
          false,
          80000
        )
      assert(answer.status == Status.NoMatch)
      assert(
        answer.text.endsWith(
          "\n-- OneLineTests.posed is in test sources: tests grit.outline.render.OneLineTests"
        )
      )
    }
  }
}
