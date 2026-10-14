package grit.outline.query

import grit.outline.cli.Main
import grit.outline.locate.{Layout, Locate, MillLayout, Root}
import grit.outline.testing.{FixtureLayout, FixtureSuitesLayout, OutlineLayout}

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
          FixtureLayout,
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

    test("show of a name nothing defines is NoMatch, saying so and suggesting find with the name") {
      val (answer, _) =
        Query.show(
          root,
          FixtureLayout,
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
      assert(answer.text.endsWith("\n-- no match for Nope; try find *Nope*"))
    }

    test("a second read of unchanged tasty reads no files") {
      val (_, first) =
        Query.show(
          root,
          FixtureLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("A.keep"),
          1,
          Set.empty,
          false,
          80000
        )
      val tasty = Locate.forTopLevel(root, FixtureLayout, "A.keep")
      Loaded.defns(root, FixtureLayout, tasty, Roots.of(first, root)) match {
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
          FixtureLayout,
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

    test("a --body name that is a private member is named as private, and says to add --private") {
      val (answer, _) =
        Query.show(
          root,
          FixtureLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("grit.outline.fixture.Limits"),
          0,
          Set("hidden"),
          false,
          80000
        )
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.contains("-- --body hidden is private: add --private"))
      assert(!answer.text.contains("matches nothing shown"))
      val (withPrivate, _) =
        Query.show(
          root,
          FixtureLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("grit.outline.fixture.Limits"),
          0,
          Set("hidden"),
          true,
          80000
        )
      assert(!withPrivate.text.contains("--body hidden is private"))
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
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val tasty = os.walk(classes).filter(_.ext == "tasty").toVector
      val (defns, _, notes) = Query.loadAll(root, layout, tasty, Loaded.empty)
      assert(defns.exists(_.fullName == "grit.outline.fixture.A"))
      assert(notes.size == 1)
      assert(notes.head == "-- not loaded: grit/outline/fixture/Bad.tasty")
    }

    test(
      "a source cut shorter than its tasty's positions is shown stale, each definition past its end as its TASTy signature under a line saying it is not file text, with no size"
    ) {
      val dir = os.temp.dir(prefix = "outline-shortened-")
      val src = dir / "src" / "q" / "Shelf.scala"
      os.write(
        src,
        "package q\n\n/** A shelf. */\ntrait Shelf {\n\n  /** The first. */\n  def first: Int\n\n  /** The last, far down. */\n  def last(x: Int): String\n}\n",
        createFolders = true
      )
      val classes = dir / "classes"
      os.makeDir.all(classes)
      val compiled = dotty.tools.dotc.Main.process(
        Array(
          "-d",
          classes.toString,
          "-classpath",
          System.getProperty("java.class.path"),
          src.toString
        )
      )
      assert(!compiled.hasErrors)
      os.write.over(src, "package q\n\n/** A shelf. */\ntrait Shelf {\n}\n")
      os.mtime.set(src, os.mtime(classes / "q" / "Shelf.tasty") + 3600 * 1000)
      val layout = new Layout {
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val (_, _, notes) =
        Query.loadAll(Root(dir), layout, Vector(classes / "q" / "Shelf.tasty"), Loaded.empty)
      assert(notes.isEmpty)
      val (answer, _) = Query.show(
        Root(dir),
        layout,
        Config.empty,
        Roots.empty(6000),
        Vector("q.Shelf"),
        depth = 0,
        bodies = Set.empty,
        withPrivate = false,
        cap = 80000,
        withTests = false
      )
      assert(!answer.text.contains("not loaded"))
      assert(answer.status == Status.Found)
      val lines = answer.text.linesIterator.toVector
      assert(lines.contains("== src/q/Shelf.scala  q  [stale: source newer than .tasty]"))
      assert(
        lines.contains("-- not file text, past the end of the source: def last(x: Int): String")
      )
      assert(lines.contains("-- not file text, past the end of the source: trait Shelf"))
      assert(lines.contains("-- not file text, past the end of the source: def first: Int"))
      assert(!lines.exists(l => !l.startsWith("[") && l.endsWith(" KB]")))
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
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
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
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector(classes)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
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
          OutlineLayout,
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
          OutlineLayout,
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
          "\n-- OneLineTests.posed is in test sources: tests grit.outline.render.OneLineTests --body posed"
        )
      )
    }

    test("show lists the tests that exercise a definition, by suite, name and lines") {
      val (answer, _) =
        Query.show(
          root,
          FixtureSuitesLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("Limits"),
          0,
          Set.empty,
          false,
          80000
        )
      val section = answer.text.linesIterator.dropWhile(!_.startsWith("## tests")).toVector
      assert(
        section == Vector(
          "## tests exercising Limits",
          "== dev/outline/fixture/suites/src/MoreTests.scala  grit.outline.fixture.suites",
          "  9-11 MoreTests: limits of nothing",
          "== dev/outline/fixture/suites/src/SampleTests.scala  grit.outline.fixture.suites",
          "  18-20 SampleTests: limits are built through of, with two limits or three",
          section.last
        )
      )
    }

    test(
      "the tests section reads no test tasty of a package whose test sources do not mention the name"
    ) {
      val dir = os.temp.dir(prefix = "outline-test-packages-")
      os.write(dir / "build.mill", "")
      os.write(
        dir / "m" / "test" / "src" / "a" / "ATests.scala",
        "package a\nobject ATests { val x = S.ask }\n",
        createFolders = true
      )
      os.write(
        dir / "m" / "test" / "src" / "b" / "BTests.scala",
        "package b\nobject BTests { val y = 1 }\n",
        createFolders = true
      )
      val classes = dir / "out" / "m" / "test" / "compile.dest" / "classes"
      os.write(classes / "a" / "ATests.tasty", "", createFolders = true)
      os.write(classes / "b" / "BTests.tasty", "", createFolders = true)
      val (tasty, unsearched) = Query.toRead(
        Root(dir),
        Config.empty,
        Vector(classes),
        Vector(Query.Search("ask", None)),
        MillLayout.isTestSource
      )
      assert(tasty == Vector(classes / "a" / "ATests.tasty"))
      assert(unsearched.isEmpty)
    }

    test(
      "show B.note shows the constructor val on its class's line, and show B lists no val entry for it"
    ) {
      def shown(sym: String): Answer =
        Query
          .show(
            root,
            FixtureLayout,
            Config.empty,
            Roots.empty(6000),
            Vector(sym),
            0,
            Set.empty,
            false,
            80000
          )
          ._1
      val field = shown("B.note")
      assert(field.status == Status.Found)
      assert(
        field.text.contains("17-17 val B.note  [0.2 KB]\nfinal case class B(c: C, note: String)")
      )
      assert(!shown("grit.outline.fixture.B").text.contains("val B."))
    }

    test("show --no-tests prints no tests section") {
      val (answer, _) =
        Query.show(
          root,
          OutlineLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("Render.oneLine"),
          1,
          Set.empty,
          false,
          80000,
          withTests = false
        )
      assert(answer.status == Status.Found)
      assert(!answer.text.contains("## tests exercising"))
    }

    test("a site in a suite's helper is listed as that helper") {
      val (answer, _) =
        Query.show(
          root,
          FixtureSuitesLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("grit.outline.fixture.C"),
          0,
          Set.empty,
          false,
          80000
        )
      assert(answer.text.contains("\n  11-12 SampleTests: helper made\n"))
    }
  }
}
