package grit.outline.query

import grit.outline.cli.Main
import grit.outline.locate.{Layout, Locate, MillLayout, Root, SourceWords}
import grit.outline.read.Read
import grit.outline.render.TestsShown
import grit.outline.testing.{Dotc, FixtureLayout, FixtureSuitesLayout, OutlineLayout}

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

  /** The `show` answer's text for `sym` over the fixture, with `bodies` and private members. */
  private def shownWith(sym: String, bodies: Set[String]): String =
    Query
      .show(
        root,
        FixtureLayout,
        Config.empty,
        Roots.empty(6000),
        Vector(sym),
        0,
        bodies,
        true,
        80000,
        TestsShown.Dropped
      )
      ._1
      .text

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

    test("--body takes a path to a nested member and prints its body: Outer --body Inner.deep") {
      val text = shownWith("Outer", Set("Inner.deep"))
      assert(text.linesIterator.contains("      val doubled = x * 2"))
      assert(!text.contains("matches nothing shown"))
    }

    test("a bare --body name only one nested owner defines prints from there: Outer --body deep") {
      val text = shownWith("Outer", Set("deep"))
      assert(text.linesIterator.contains("      val doubled = x * 2"))
      assert(!text.contains("matches nothing shown"))
    }

    test(
      "a bare --body name several nested owners define is named with each owner, and none prints"
    ) {
      val text = shownWith("Nests", Set("same"))
      assert(
        text.linesIterator.contains(
          "-- --body same is in several owners: Nests.Left.same, Nests.Right.same; name one as Owner.same"
        )
      )
      assert(!text.contains("def Nests.Left.same"), !text.contains("def Nests.Right.same"))
    }

    test(
      "--body on a member type prints its own text from its head to its end: Holder --body Nested"
    ) {
      val text = shownWith("Holder", Set("Nested"))
      assert(text.linesIterator.contains("    /** A count, or why not: a negative `n`. */"))
    }

    test(
      "a --body name a val's anonymous class defines is named with its line, and the --body printing it whole"
    ) {
      val text = shownWith("Greeters.polite", Set("greet"))
      assert(
        text.linesIterator.contains(
          "-- --body greet is defined in Greeters.polite's body, in an anonymous class or as a local, at line 21: --body polite prints it whole"
        )
      )
    }

    test(
      "a --body name an anonymous class in a shown object's val defines is named with its line, and the --body printing that val"
    ) {
      val text = shownWith("Greeters", Set("greet"))
      assert(
        text.linesIterator.contains(
          "-- --body greet is defined in Greeters.polite's body, in an anonymous class or as a local, at line 21: --body polite prints it whole"
        )
      )
      assert(!text.contains("matches nothing shown"))
    }

    test(
      "a --body name the shown type lacks, defined elsewhere taking it, is named with its place and the show printing it"
    ) {
      val text = shownWith("Spot", Set("isNamed"))
      assert(
        text.linesIterator.contains(
          "-- --body isNamed is not Spot's; defined elsewhere naming Spot: grit.outline.fixture.Spots.isNamed  dev/outline/fixture/src/Foreign.scala:9-10  def isNamed(spot: Spot): Boolean  [0.2 KB]: show grit.outline.fixture.Spots --body isNamed prints it"
        )
      )
      assert(!text.contains("matches nothing shown"))
    }

    test(
      "a --body name no signature elsewhere names the shown type in says so, and names the find listing every definition of it"
    ) {
      val line = shownWith("Spot", Set("greet")).linesIterator
        .find(_.startsWith("-- --body greet matches nothing shown; names here: "))
      assert(
        line.exists(
          _.endsWith(
            "; no greet elsewhere names Spot in its signature: find '*.greet' lists every greet"
          )
        )
      )
    }

    test(
      "a .tasty deleted between its listing and its read is skipped and named, and the rest load"
    ) {
      val classes = FixtureLayout.classesDirs(root)
      val listed = classes.flatMap(c => os.walk(c).filter(_.ext == "tasty"))
      val gone = classes.map(_ / "grit" / "outline" / "fixture" / "Gone.tasty")
      val (defns, _, notes) = Query.loadAll(root, FixtureLayout, listed ++ gone, Loaded.empty)
      assert(defns.exists(_.fullName == "grit.outline.fixture.A"))
      assert(
        notes == Vector(
          "-- not loaded: grit/outline/fixture/Gone.tasty (deleted since it was listed); compile to read them: ./mill grit.outline.fixture.compile"
        )
      )
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
      assert(
        notes.head == "-- not loaded: grit/outline/fixture/Bad.tasty (not a TASTy file); compile to read them: compile"
      )
    }

    test(
      "a .tasty naming a class its module's failed compile removed is named with the compiler's error and the command compiling it"
    ) {
      val dir = Dotc.millRoot("outline-removed-")
      val world = dir / "m" / "src" / "p" / "World.scala"
      val user = dir / "m" / "src" / "p" / "User.scala"
      os.write(world, "package p\nclass World\n", createFolders = true)
      os.write(user, "package p\nobject User { def w = new World }\n")
      val classes = Dotc.classesOf(dir, os.sub / "m")
      Dotc.compile(classes, Vector(world, user))
      // What a failed incremental compile of World.scala leaves: its classes gone, User's kept.
      os.list(classes / "p").filter(_.last.startsWith("World")).foreach(os.remove)
      val (_, _, notes) =
        Query.loadAll(Root(dir), MillLayout, Vector(classes / "p" / "User.tasty"), Loaded.empty)
      assert(
        notes == Vector(
          "-- not loaded: p/User.tasty (User.scala:2: undefined: new p.World); compile to read them: ./mill m.compile"
        )
      )
    }

    test(
      "a .tasty the inspector cannot read fails no query: uses answers from the rest, naming it in one line"
    ) {
      val classes = os.temp.dir(prefix = "outline-unreadable-") / "classes"
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
      val (answer, _) = Query.uses(
        root,
        layout,
        Config.empty,
        Roots.empty(6000),
        Vector("grit.outline.fixture.Limits.of"),
        None,
        None,
        80000
      )
      assert(answer.status == Status.Found)
      val notes = answer.text.linesIterator.filter(_.startsWith("-- not loaded:")).toVector
      assert(
        notes.size == 1,
        notes.headOption.exists(
          _.startsWith("-- not loaded: grit/outline/fixture/Bad.tasty (not a TASTy file)")
        )
      )
      assert(!answer.text.contains("\u001b"))
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
      Dotc.compile(classes, Vector(src))
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
        tests = TestsShown.Dropped
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
      assert(
        firstNotes == Vector(
          "-- not loaded: grit/outline/fixture/Bad.tasty (not a TASTy file); compile to read them: compile"
        )
      )
      assert(first.failed.keySet == Set(bad))
      val (defns, second, secondNotes) = Query.loadAll(root, layout, tasty, first)
      assert(Loaded.defns(root, layout, tasty, second).map(_._3) == Right(0))
      assert(
        secondNotes == Vector(
          "-- not loaded: grit/outline/fixture/Bad.tasty (not a TASTy file); compile to read them: compile"
        )
      )
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
      assert(
        notes == Vector(
          "-- not loaded: grit/outline/fixture/Bad.tasty (not a TASTy file); compile to read them: compile"
        )
      )
    }

    test("show of a test suite's name shows it from test sources, under a note naming it") {
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
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains("-- in test sources: grit.outline.render.OneLineTests")
      )
      assert(answer.text.linesIterator.contains("object OneLineTests extends TestSuite {"))
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
          80000,
          tests = TestsShown.Listed
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
      val (tasty, unsearched, _) = Query.toRead(
        Root(dir),
        Config.empty,
        Vector(classes),
        Vector(Query.Search("ask", None)),
        MillLayout.isTestSource,
        SourceWords.empty
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

    test(
      "show ends with one tests line per named symbol, its tests, suites and nearest suites, and lists none"
    ) {
      val (answer, _) =
        Query.show(
          root,
          FixtureSuitesLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("Limits", "B"),
          0,
          Set.empty,
          false,
          80000
        )
      val lines = answer.text.linesIterator.toVector
      assert(
        lines.filter(_.startsWith("-- tests")) == Vector(
          "-- tests of Limits: 2 in 2 suites (nearest: SampleTests, MoreTests); --tests to list",
          "-- tests of B: 1 in 1 suite (nearest: SampleTests); --tests to list"
        )
      )
      assert(!lines.exists(l => l.startsWith("## tests") || l.contains("SampleTests: ")))
    }

    test(
      "the nearest suites are those in the symbol's own module first, then those holding the most references"
    ) {
      val ranked = Query.nearest(
        "m/src/a/A.scala",
        Vector(
          Query.SuiteHits("Far", "n/test/src/FarTests.scala", 9),
          Query.SuiteHits("Own", "m/test/src/OwnTests.scala", 1),
          Query.SuiteHits("Busy", "m/test/src/BusyTests.scala", 3),
          Query.SuiteHits("Farther", "x/test/src/FartherTests.scala", 2)
        )
      )
      assert(ranked == Vector("Busy", "Own", "Far"))
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
          tests = TestsShown.Dropped
        )
      assert(answer.status == Status.Found)
      assert(!answer.text.contains("## tests exercising"))
      assert(!answer.text.contains("-- tests"))
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
          80000,
          tests = TestsShown.Listed
        )
      assert(answer.text.contains("\n  11-12 SampleTests: helper made\n"))
    }

    test("every query's answer over an uncompiled root says so under the root line") {
      val uncompiled = new Layout {
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector.empty
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val config =
        Config(Vector.empty, Vector(Area("a", Vector("grit.outline.fixture.A"), Vector.empty)))
      val sym = "grit.outline.fixture.A"
      val answers = Map(
        "show" -> Query
          .show(
            root,
            uncompiled,
            config,
            Roots.empty(6000),
            Vector(sym),
            0,
            Set.empty,
            false,
            80000
          )
          ._1,
        "find" -> Query.find(root, uncompiled, config, Roots.empty(6000), "A", false, 80000)._1,
        "uses" -> Query
          .uses(root, uncompiled, config, Roots.empty(6000), Vector(sym), None, None, 80000)
          ._1,
        "area" -> Query.area(root, uncompiled, config, Roots.empty(6000), Vector("a"), 1, 80000)._1,
        "file" -> Query.file(root, uncompiled, config, Roots.empty(6000), sym, 80000)._1,
        "tests" -> Query
          .tests(root, uncompiled, config, Roots.empty(6000), sym, None, Set.empty, 80000)
          ._1,
        "family" -> Query
          .family(root, uncompiled, config, Roots.empty(6000), sym, None, false, 80000)
          ._1
      )
      val wrong = answers.collect {
        case (query, answer)
            if answer.status != Status.Failed ||
              !answer.text.startsWith(s"## root ${root.dir} (") ||
              answer.text.linesIterator.drop(1).toVector != Vector("not compiled") =>
          s"$query: $answer"
      }
      assert(wrong.isEmpty)
    }

    test(
      "a read refused as built in another checkout says so on the line under the root line, naming the .tasty and the source it records"
    ) {
      val elsewhere = Root(os.temp.dir(prefix = "outline-elsewhere-"))
      val sym = "grit.outline.fixture.A"
      val config = Config(Vector.empty, Vector(Area("a", Vector(sym), Vector.empty)))
      val answers = Map(
        "show" -> Query
          .show(
            elsewhere,
            FixtureLayout,
            config,
            Roots.empty(6000),
            Vector(sym),
            0,
            Set.empty,
            false,
            80000
          )
          ._1,
        "uses" -> Query
          .uses(elsewhere, FixtureLayout, config, Roots.empty(6000), Vector(sym), None, None, 80000)
          ._1,
        "area" -> Query
          .area(elsewhere, FixtureLayout, config, Roots.empty(6000), Vector("a"), 1, 80000)
          ._1,
        "file" -> Query.file(elsewhere, FixtureLayout, config, Roots.empty(6000), sym, 80000)._1,
        "tests" -> Query
          .tests(elsewhere, FixtureLayout, config, Roots.empty(6000), sym, None, Set.empty, 80000)
          ._1,
        "family" -> Query
          .family(elsewhere, FixtureLayout, config, Roots.empty(6000), sym, None, false, 80000)
          ._1
      )
      // find is left out: it reads only the .tasty of packages whose sources under the root hold the name, and this
      // root has no sources, so it reads none and has nothing to refuse.
      val refusal = Vector(
        FixtureLayout
          .classesDirs(elsewhere)
          .map(_ / os.RelPath("grit/outline/fixture/A.tasty"))
          .mkString +
          s" records its source at ${root.dir / "dev/outline/fixture/src/Sample.scala"}, outside ${elsewhere.dir}: " +
          Read.builtElsewhere
      )
      val wrong = answers.collect {
        case (query, answer)
            if answer.status != Status.Failed ||
              !answer.text.startsWith(s"## root ${elsewhere.dir} (") ||
              answer.text.linesIterator.drop(1).toVector != refusal =>
          s"$query: $answer"
      }
      assert(wrong.isEmpty)
    }
  }
}
