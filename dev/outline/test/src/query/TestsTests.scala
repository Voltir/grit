package grit.outline.query

import grit.outline.locate.{Layout, MillLayout, Root}
import grit.outline.testing.{Dotc, FixtureLayout, FixtureSuitesLayout, OutlineLayout}

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
    Query.tests(root, layout, Config.empty, Roots.empty(6000), name, test, Set.empty, 80000)._1

  private val useSuite = "grit.outline.fixture.suites.FixtureUseTests"
  private val useFile = "dev/outline/fixture/suites/src/FixtureUseTests.scala"

  /** The `tests` answer's lines for FixtureUseTests, whose fixtures are in SuiteFixtures. */
  private def useLines(
      test: Option[String] = None,
      bodies: Set[String] = Set.empty,
      fixtures: Boolean = false,
      cap: Int = 80000
  ): Vector[String] =
    Query
      .tests(
        root,
        FixtureSuitesLayout,
        Config.empty,
        Roots.empty(6000),
        useSuite,
        test,
        bodies,
        cap,
        fixtures
      )
      ._1
      .text
      .linesIterator
      .toVector

  /** The lines of the source at `file`, 0-based. */
  private def source(file: String): Vector[String] =
    os.read(root.dir / os.RelPath(file)).split("\n").toVector

  def tests = Tests {
    test("a suite's file's package and imports print first, as the file's own lines") {
      val lines = useLines()
      assert(lines.lift(1).exists(_.startsWith(s"== $useFile  ")))
      assert(lines.lift(2) == Some("1-3"))
      assert(lines.slice(3, 6) == source(useFile).take(3))
    }

    test("the suite's in-body import prints under its line, as the file's own line") {
      val lines = useLines()
      val at = lines.indexOf("  7-7")
      assert(at > 0)
      assert(lines.lift(at + 1) == Some("  import SuiteFixtures.*"))
    }

    test(
      "the suite's head and closing lines are the file's own lines, and its one-line form is gone"
    ) {
      val lines = useLines()
      val at = lines.indexWhere(_.startsWith("5-21  ["))
      assert(at > 0)
      assert(lines.slice(at + 1, at + 3) == source(useFile).slice(4, 6))
      val end = lines.indexOf("20-21 end of FixtureUseTests")
      assert(end > at)
      assert(lines.slice(end + 1, end + 3) == source(useFile).slice(19, 21))
      assert(!lines.exists(_.contains("object FixtureUseTests  object")))
    }

    test(
      "the fixtures the suite names print under one header, a line each with the members it names, one name's definitions on one line"
    ) {
      val lines = useLines()
      val at = lines.indexOf("  fixtures (--fixtures for their signatures):")
      assert(at > 0)
      assert(
        lines.slice(at + 1, at + 3) == Vector(
          "    grit.outline.fixture.suites.Paired  dev/outline/fixture/suites/src/Paired.scala:3-6, 8-11  twice, one",
          "    grit.outline.fixture.suites.SuiteFixtures  dev/outline/fixture/suites/src/SuiteFixtures.scala:3-19  seeded, label"
        )
      )
      assert(!lines.exists(_.contains("unnamed")))
    }
    test(
      "--body prints a used fixture member's own lines, and a suite helper of the same name wins"
    ) {
      val lines = useLines(bodies = Set("seeded", "label"))
      val fixtures = "dev/outline/fixture/suites/src/SuiteFixtures.scala"
      assert(lines.containsSlice(source(fixtures).slice(5, 7)))
      assert(lines.containsSlice(source(useFile).slice(10, 12)))
      assert(!lines.exists(_.contains("def label: String = \"fixture\"")))
    }

    test("--fixtures lists the used members' collapsed signatures, and no other member") {
      val lines = useLines(fixtures = true)
      val at = lines.indexOf("  fixtures:")
      assert(at > 0)
      assert(
        lines.slice(at + 4, at + 7) == Vector(
          "    grit.outline.fixture.suites.SuiteFixtures  dev/outline/fixture/suites/src/SuiteFixtures.scala:3-19",
          "      6-7 def seeded: Int  [0.2 KB]",
          "      9-10 def label: String  [0.2 KB]"
        )
      )
      assert(!lines.exists(_.contains("unnamed")))
    }

    test(
      "under a budget smaller than the suite, a --test match and a --body helper print, and the cut counts the rest"
    ) {
      val lines = useLines(test = Some("a label"), bodies = Set("label"), cap = 200)
      assert(lines.contains("    SuiteFixtures.label"))
      assert(lines.contains("  def label: String = \"own\""))
      assert(lines.exists(_.startsWith("-- cut at 0.2 KB: ")))
    }

    test(
      "the suite's helper is fresh with its doc's line, and its two tests are listed with their ranges; run, which holds tests, is not a helper"
    ) {
      val lines = answer(suite, None).text.linesIterator.toVector
      assert(lines.contains("  65-66 def fresh(): Store  [0.2 KB]"))
      assert(lines.contains("  tests (2):"))
      assert(lines.contains("    69-71 put then get returns the value  [0.2 KB]"))
      assert(lines.contains("    72-74 get of an absent key is None  [0.2 KB]"))
      assert(!lines.exists(_.contains("def run")))
      assert(
        lines.contains("  63-63 private def test(name: String)(body: => Unit): Unit  [0.2 KB]")
      )
    }

    test("the suite's line, each helper's line and each test's line end with its size") {
      val lines = answer(suite, None).text.linesIterator.toVector
      assert(lines.contains("61-76  [0.4 KB]"))
      assert(lines.contains("  65-66 def fresh(): Store  [0.2 KB]"))
      assert(lines.contains("    69-71 put then get returns the value  [0.2 KB]"))
    }

    test("a test prefix prints the verbatim body of the tests it starts, and no other") {
      val text = answer(suite, Some("get of")).text
      assert(text.linesIterator.contains("      assert(fresh().get(\"x\").isEmpty)"))
      assert(!text.contains("assert(fresh().put("))
      assert(!text.contains("-- no test starting"))
    }

    test("--test takes comma-separated prefixes, printing each test any of them starts") {
      val text = answer(suite, Some("put then,get of")).text
      assert(text.linesIterator.contains("      assert(fresh().get(\"x\").isEmpty)"))
      assert(text.contains("assert(fresh().put("))
      assert(!text.contains("-- no test starting"))
    }

    test("a test prefix that starts no test is named in a note") {
      assert(answer(suite, Some("nope")).text.contains("-- no test starting \"nope\""))
    }

    test("a name no class or object declares is NoMatch, saying so under the root line") {
      val missing = answer("grit.outline.fixture.store.Missing", None)
      assert(missing.status == Status.NoMatch)
      assert(missing.text.startsWith(s"## root ${root.dir} ("))
      assert(
        missing.text.linesIterator.toVector.lastOption.contains(
          "no class or object grit.outline.fixture.store.Missing"
        )
      )
    }

    test("an ambiguous suite name is NoMatch, names every suite it matches, and prints none") {
      val found = answer("Twin", None)
      assert(found.status == Status.NoMatch)
      assert(
        found.text.contains(
          "-- ambiguous Twin, name one fully:\n--   grit.outline.fixture.Twin  dev/outline/fixture/src/Sample.scala:"
        ) && found.text.contains(
          "\n--   grit.outline.fixture.store.Twin  dev/outline/fixture/src/store/Store.scala:"
        )
      )
      assert(!found.text.contains("== "))
    }

    test("a source newer than its tasty is tagged stale, and its file line says so") {
      // A temp root holds the source and a tasty file with set mtimes; the checkout is not touched.
      val tmp = os.temp.dir(prefix = "outline-stale-")
      try {
        val rel = "dev/outline/src/Store.scala"
        val source = tmp / os.RelPath(rel)
        val tasty =
          tmp / "out" / "classes" / "grit" / "outline" / "fixture" / "store" / "Store.tasty"
        os.write(
          source,
          "package grit.outline.fixture.store\n\nobject Store\n",
          createFolders = true
        )
        os.write(tasty, "", createFolders = true)
        val now = System.currentTimeMillis()
        java.nio.file.Files.setLastModifiedTime(
          source.toNIO,
          java.nio.file.attribute.FileTime.fromMillis(now + 3600000L)
        )
        java.nio.file.Files
          .setLastModifiedTime(tasty.toNIO, java.nio.file.attribute.FileTime.fromMillis(now))
        val store = grit.outline.model.Defn(
          kind = grit.outline.model.Kind.Object,
          name = "Store",
          fullName = "grit.outline.fixture.store.Store",
          file = rel,
          lines = grit.outline.model.Lines(3, 3),
          doc = None,
          signature = "object Store",
          head = "object Store",
          body = None,
          size = None,
          closing = None,
          members = Vector.empty,
          refs = Vector.empty,
          parents = Vector.empty,
          isPrivate = false,
          isAbstract = false
        )
        // As a query that read Store from this .tasty leaves its cache.
        val loaded = Loaded(
          byTasty = Map(tasty -> Cached(now, Vector(store))),
          consulted = Set(tasty)
        )
        val stale = Query.staleFiles(Root(tmp), loaded, Vector(rel))
        assert(stale == Set(rel))
        assert(
          grit.outline.render.Render.fileLine(rel, "grit.outline.fixture.store", stale = true) ==
            s"== $rel  grit.outline.fixture.store  [stale: source newer than .tasty]"
        )
      } finally os.remove.all(tmp)
    }

    test("tests --body prints a helper's own lines, byte-identical to the file") {
      val file = os.read.lines(root.dir / "dev/outline/fixture/src/store/Store.scala")
      val helper = file.slice(64, 66).mkString("\n")
      val text =
        Query
          .tests(
            root,
            FixtureLayout,
            Config.empty,
            Roots.empty(6000),
            suite,
            None,
            Set("fresh"),
            80000
          )
          ._1
          .text
      assert(text.contains(helper))
    }

    test("tests --body names a name that matches no helper, and the helpers it has") {
      val text =
        Query
          .tests(
            root,
            FixtureLayout,
            Config.empty,
            Roots.empty(6000),
            suite,
            None,
            Set("nope"),
            80000
          )
          ._1
          .text
      assert(text.contains("-- --body nope matches nothing shown; names here: "))
      assert(text.contains("fresh"))
    }

    test("show of a name main sources lack answers from test sources, under a note naming it") {
      val (answer, _) = Query.show(
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
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains(
          "-- in test sources: grit.outline.render.OneLineTests.posed"
        )
      )
      assert(answer.text.linesIterator.exists(_.contains("private val posed")))
      assert(!answer.text.contains("-- tests of posed"))
    }

    test("tests --body on a helper class prints its own text from its head to its end") {
      val text = Query
        .tests(
          root,
          FixtureSuitesLayout,
          Config.empty,
          Roots.empty(6000),
          "grit.outline.fixture.suites.SuiteFixtures",
          None,
          Set("Box"),
          80000
        )
        ._1
        .text
      assert(text.linesIterator.contains("    def doubled: Int = n * 2"))
    }

    test("tests lists the suite's private helpers and its file's other top-level definitions") {
      val lines =
        answer("grit.outline.render.OneLineTests", None, OutlineLayout).text.linesIterator.toVector
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
          OutlineLayout
        ).text
          .split("\n", -1)
          .toVector
      val header = lines.find(l => l.startsWith("    ") && l.contains(s" $name  [")).getOrElse("")
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

    test(
      "a suite whose source no .tasty records is named by its file and the command compiling its module, and not a file declaring a longer name through `$`"
    ) {
      // A temp Mill root: module m's test sources p/OtherTests.scala, compiled, and p/GoneTests.scala, never compiled.
      val d = Dotc.millRoot("outline-tests-gone-")
      val other = d / "m" / "test" / "src" / "p" / "OtherTests.scala"
      os.write(other, "package p\nobject OtherTests\n", createFolders = true)
      Dotc.compile(Dotc.classesOf(d, os.sub / "m" / "test"), Vector(other))
      os.write(
        d / "m" / "test" / "src" / "p" / "GoneTests.scala",
        "package p\nobject GoneTests {\n}\n"
      )
      os.write(d / "m" / "test" / "src" / "p" / "Kept.scala", "package p\nobject GoneTests$Kept\n")
      val (gone, _) =
        Query.tests(
          Root(d),
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          "GoneTests",
          None,
          Set.empty,
          80000
        )
      assert(
        gone.text.linesIterator.toVector.drop(1) == Vector(
          "no class or object GoneTests compiled: m/test/src/p/GoneTests.scala defines it, but no .tasty records that file, so its last compile failed or never ran; compile it first: ./mill m.test.compile"
        )
      )
    }
  }
}
