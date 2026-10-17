package grit.outline.query

import java.nio.file.Files
import java.nio.file.attribute.FileTime

import grit.outline.locate.{Layout, Locate, MillLayout, Root, SourceWords}
import grit.outline.model.Use
import grit.outline.read.Read
import grit.outline.render.Render
import grit.outline.testing.{FixtureLayout, FixtureTestLayout}

import utest.*

object UsesTests extends TestSuite {

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

  private val storePackage = "grit.outline.fixture.store"

  private def uses(
      sym: String,
      in: Option[String] = None,
      outside: Option[String] = None
  ): (Answer, Roots) =
    Query.uses(
      root,
      FixtureLayout,
      Config.empty,
      Roots.empty(6000),
      Vector(sym),
      in,
      outside,
      80000
    )

  /** `Loaded.uses` over `tasty` from `in`; a failure throws, so the test shows it. */
  private def loadedUses(tasty: Vector[os.Path], in: Loaded): (Vector[Use], Loaded, Int) =
    Loaded.uses(root, FixtureLayout, tasty, in) match {
      case Right(read) => read
      case Left(message) => throw new Exception(message)
    }

  def tests = Tests {
    test(
      "the uses of Store.get include StoreContract.keepsWhatItIsGiven and Caller.all, named by their enclosing definitions, and not MemStore.get's Map.get"
    ) {
      val storeTasty = Locate.inPackageOf(
        root,
        FixtureLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val sites =
        Read.uses(root, FixtureLayout, storeTasty, Set(s"$storePackage.Store.get")) match {
          case Right(us) => us
          case Left(message) => throw new Exception(message)
        }
      val enclosing = sites.map(_.enclosing).toSet
      assert(enclosing.contains(s"$storePackage.StoreContract.keepsWhatItIsGiven"))
      assert(enclosing.contains(s"$storePackage.Caller.all"))
      assert(!enclosing.contains(s"$storePackage.MemStore.get"))
    }

    test(
      "uses names an owner once, then each of its sites: StoreSuite.run's two gets under one line"
    ) {
      val (answer, _) = uses(s"$storePackage.Store.get")
      val lines = answer.text.linesIterator.toVector
      assert(lines.count(_ == "  in StoreSuite.run  [0.2 KB]") == 1)
      val at = lines.indexOf("  in StoreSuite.run  [0.2 KB]")
      assert(
        lines.slice(at + 1, at + 3) == Vector(
          "    70  assert(fresh().put(\"k\", \"v\").get(\"k\") == Some(\"v\"))",
          "    73  assert(fresh().get(\"x\").isEmpty)"
        )
      )
    }

    test("an owner's line ends with its definition's size: StoreSuite.run's 222 bytes") {
      val (answer, _) = uses(s"$storePackage.Store.get")
      assert(answer.text.linesIterator.contains("  in StoreSuite.run  [0.2 KB]"))
    }

    test("a site in a local val's right-hand side is owned by the member around it, Local.read") {
      val (answer, _) = uses(s"$storePackage.Store.get")
      val lines = answer.text.linesIterator.toVector
      val at = lines.indexWhere(_.contains("val held = s.get"))
      assert(at > 0)
      assert(lines(at - 1) == "  in Local.read  [0.2 KB]")
    }

    test("a site inside a test call in a test source is owned by its suite and the test's name") {
      val (answer, _) = Query.uses(
        root,
        FixtureTestLayout,
        Config.empty,
        Roots.empty(6000),
        Vector(s"$storePackage.Store.get"),
        None,
        None,
        80000
      )
      val lines = answer.text.linesIterator.toVector
      assert(
        lines.containsSlice(
          Vector(
            "  in StoreSuite: put then get returns the value  [0.2 KB]",
            "    70  assert(fresh().put(\"k\", \"v\").get(\"k\") == Some(\"v\"))",
            "  in StoreSuite: get of an absent key is None  [0.2 KB]",
            "    73  assert(fresh().get(\"x\").isEmpty)"
          )
        )
      )
      assert(lines.contains("  in Caller.all  [0.2 KB]"))
    }

    test(
      "a call split across lines is a site on its name's line: its text is that line, and its enclosing definition is Caller.split"
    ) {
      val storeTasty = Locate.inPackageOf(
        root,
        FixtureLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val sites =
        Read.uses(root, FixtureLayout, storeTasty, Set(s"$storePackage.Store.get")) match {
          case Right(us) => us
          case Left(message) => throw new Exception(message)
        }
      assert(
        sites.exists(u => u.enclosing.endsWith("Caller.split") && u.text == ".get(\"k\")")
      )
    }

    test(
      "a candidate file with no compiled tasty is named as not searched, with its path under src"
    ) {
      val dir = os.temp.dir(prefix = "outline-not-compiled-")
      os.write(dir / "build.mill", "")
      val src = dir / "src" / "p" / "S.scala"
      os.write(src, "package p\nobject S { def f(a: Int): Int = a }\n", createFolders = true)
      val classes = dir / "out" / "m" / "compile.dest" / "classes"
      os.makeDir.all(classes)
      val reporter = dotty.tools.dotc.Main.process(
        Array(
          "-d",
          classes.toString,
          "-classpath",
          System.getProperty("java.class.path"),
          src.toString
        )
      )
      assert(!reporter.hasErrors)
      os.write(
        dir / "src" / "q" / "T.scala",
        "package q\nobject T { def g: Int = p.S.f(1) }\n",
        createFolders = true
      )
      val (answer, _) = Query.uses(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        Vector("p.S.f"),
        None,
        None,
        80000
      )
      assert(
        answer.text.linesIterator.exists(line =>
          line.startsWith("-- not compiled, not searched:") && line.contains("src/q/T.scala")
        )
      )
    }

    test(
      "a case class's constructor val resolves: its read in Fields.noteOf is its one site, and the class's synthetic members are none"
    ) {
      val (answer, _) = uses("grit.outline.fixture.B.note")
      assert(answer.status == Status.Found)
      assert(answer.text.contains("Fields.noteOf"))
      assert(answer.text.contains("[1 sites in 1 files]"))
    }

    test(
      "a constructor val read in its case class's own explicit method is a site: MemStore.values in MemStore.get"
    ) {
      val (answer, _) = uses(s"$storePackage.MemStore.values")
      val lines = answer.text.linesIterator.toVector
      assert(
        lines.containsSlice(
          Vector(
            "  in MemStore.get  [0.2 KB]",
            "    15  def get(key: String): Option[String] = values.get(key)"
          )
        )
      )
    }

    test(
      "the uses of a case class's synthetic apply are its construction sites: Nested.apply lists Nested(n) in Nested.of"
    ) {
      val (answer, _) = uses("Nested.apply")
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.exists(_.contains("Right(Nested(n))")))
      assert(answer.text.contains("Holder.Nested.of"))
      assert(
        answer.text.contains(
          "-- Nested.apply is grit.outline.fixture.Holder.Nested's constructor: its apply and new sites"
        )
      )
    }

    test(
      "B.unapply lists B's extractor patterns and not its constructions: case B(c, _) in Matches.cOf"
    ) {
      val (answer, _) = uses("B.unapply")
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.exists(_.contains("case B(c, _) => c")))
      assert(!answer.text.contains("B(made, \"n\")"))
      assert(
        answer.text.contains(
          "-- B.unapply is grit.outline.fixture.B's pattern sites: case B(…) and case _: B"
        )
      )
    }
    test("C.unapply lists C's typed patterns: case _: C in Matches.isC") {
      val (answer, _) = uses("C.unapply")
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.exists(_.contains("case _: C => true")))
      assert(!answer.text.contains("new C(1)"))
    }
    test(
      "an enum case's unapply lists its extractor patterns, and its apply its constructions: Colour.Mix"
    ) {
      val (patterns, _) = uses("Colour.Mix.unapply")
      assert(patterns.text.linesIterator.exists(_.contains("case Colour.Mix(w) => w")))
      assert(!patterns.text.contains("Colour.Mix(1.0)"))
      val (built, _) = uses("Colour.Mix.apply")
      assert(built.status == Status.Found)
      assert(built.text.linesIterator.exists(_.contains("Colour.Mix(1.0)")))
      assert(!built.text.contains("case Colour.Mix(w)"))
    }
    test(
      "a class with no pattern sites has no uses of its unapply, under its one note line: Limits.unapply"
    ) {
      val (answer, _) = uses("Limits.unapply")
      assert(answer.status == Status.NoMatch)
      // A search names the files it could not read, such as this suite's own source; any other note is this one.
      val notes =
        answer.text.linesIterator
          .filter(l => l.startsWith("--") && !l.startsWith("-- not compiled"))
          .toVector
      assert(
        notes == Vector(
          "-- Limits.unapply is grit.outline.fixture.Limits's pattern sites: case Limits(…) and case _: Limits"
        )
      )
    }
    test(
      "a name X.apply where X is no class or enum case is no match under one note line: fixture.Quiver.apply"
    ) {
      val (answer, _) = uses("grit.outline.fixture.Quiver.apply")
      assert(answer.status == Status.NoMatch)
      // A search names the files it could not read, such as this suite's own source; any other note is this one.
      val notes =
        answer.text.linesIterator
          .filter(l => l.startsWith("--") && !l.startsWith("-- not compiled"))
          .toVector
      assert(
        notes == Vector(
          "-- grit.outline.fixture.Quiver.apply: grit.outline.fixture.Quiver is a trait; X.apply and X.unapply find a class's or an enum case's construction and pattern sites"
        )
      )
    }
    test("a case class's apply counts its new sites: Limits.apply lists new Limits in Limits.of") {
      val (answer, _) = uses("grit.outline.fixture.Limits.apply")
      assert(answer.status == Status.Found)
      assert(answer.text.linesIterator.exists(_.contains("new Limits(asks, calls + hidden)")))
    }

    test("a site's file edited since its tasty is tagged stale on its == line") {
      val dir = os.temp.dir(prefix = "outline-uses-stale-")
      os.write(dir / "build.mill", "")
      val src = dir / "src" / "p" / "S.scala"
      os.write(
        src,
        "package p\nobject S { def f(a: Int): Int = a }\nobject T { def g: Int = S.f(1) }\n",
        createFolders = true
      )
      val classes = dir / "out" / "m" / "compile.dest" / "classes"
      os.makeDir.all(classes)
      val reporter = dotty.tools.dotc.Main.process(
        Array(
          "-d",
          classes.toString,
          "-classpath",
          System.getProperty("java.class.path"),
          src.toString
        )
      )
      assert(!reporter.hasErrors)
      def query(): Answer =
        Query
          .uses(
            Root(dir),
            MillLayout,
            Config.empty,
            Roots.empty(6000),
            Vector("p.S.f"),
            None,
            None,
            80000
          )
          ._1
      assert(query().text.linesIterator.contains("== src/p/S.scala"))
      os.write.over(
        src,
        "package p\nobject S { def f(a: Int): Int = a }\nobject T { def g: Int = S.f(10) }\n"
      )
      Files.setLastModifiedTime(
        src.toNIO,
        FileTime.fromMillis(os.mtime(classes / "p" / "S.tasty") + 10000)
      )
      assert(
        query().text.linesIterator.contains("== src/p/S.scala  [stale: source newer than .tasty]")
      )
    }

    test(
      "the uses of Limits.of include the 2-argument overload's call on line 28, which targets the 3-argument of on line 31"
    ) {
      val (answer, _) = uses("grit.outline.fixture.Limits.of")
      val lines = answer.text.linesIterator.toVector
      assert(
        lines.containsSlice(
          Vector(
            "  in Limits.of  [0.2 KB]",
            "    28 [→ of :31]  def of(asks: Int, calls: Int): Either[String, Limits] = of(asks, calls, 0)"
          )
        )
      )
    }

    test(
      "a member of an object is searched for only in files that name the object: Limits.of's fixture candidates are Sample.scala alone"
    ) {
      val searches = Resolve
        .resolve(
          root,
          FixtureLayout,
          Config.empty,
          Scope.WithTests,
          "grit.outline.fixture.Limits.of",
          Loaded.empty
        )
        .map(r => Query.searchesFor(r.defns, r.seen))
      assert(searches == Right(Vector(Query.Search("of", Some("Limits")))))
      val inFixture = Query
        .candidates(root, Vector(Query.Search("of", Some("Limits"))), SourceWords.empty)
        ._1
        .filter(_.startsWith(os.RelPath("dev/outline/fixture/src")))
      val mentioningOf =
        Locate
          .mentioning(root, "of", SourceWords.empty)
          ._1
          .filter(_.startsWith(os.RelPath("dev/outline/fixture/src")))
      assert(inFixture == Vector(os.RelPath("dev/outline/fixture/src/Sample.scala")))
      assert(mentioningOf.size > inFixture.size)
    }

    test(
      "a member of a trait is searched for in every file naming it, since a call through a value need not name the trait"
    ) {
      val searches = Resolve
        .resolve(
          root,
          FixtureLayout,
          Config.empty,
          Scope.WithTests,
          "grit.outline.fixture.store.Store.get",
          Loaded.empty
        )
        .map(r => Query.searchesFor(r.defns, r.seen))
      assert(searches == Right(Vector(Query.Search("get", None))))
    }

    test(
      "a bare name shorter than four characters is refused as NoMatch, reading no tasty, and the note says to qualify it"
    ) {
      val (answer, after) = Query.uses(
        root,
        FixtureLayout,
        Config.empty,
        Roots.empty(6000),
        Vector("of"),
        None,
        None,
        80000
      )
      assert(answer.status == Status.NoMatch)
      assert(
        answer.text.linesIterator.contains(
          "-- uses refuses of: a name with no qualifier must be at least 4 characters; qualify it with at least its owner, as in MoveLimits.of"
        )
      )
      assert(Roots.of(after, root).entries == 0)
    }

    test("the uses of Store.get outside the fixture are no match") {
      val (answer, _) = uses(s"$storePackage.Store.get", outside = Some("dev/outline/fixture"))
      assert(answer.status == Status.NoMatch)
    }

    test("a second uses call over unchanged tasty reads no file") {
      val storeTasty = Locate.inPackageOf(
        root,
        FixtureLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val first = loadedUses(storeTasty, Loaded.empty)
      val second = loadedUses(storeTasty, first._2)
      assert(first._3 == storeTasty.size)
      assert(second._3 == 0)
    }

    test(
      "a uses read caches the definitions of the files it read, so a defns read after it reads no file"
    ) {
      val storeTasty = Locate.inPackageOf(
        root,
        FixtureLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val afterUses = loadedUses(storeTasty, Loaded.empty)
      val afterDefns = Loaded.defns(root, FixtureLayout, storeTasty, afterUses._2)
      val fresh = Loaded.defns(root, FixtureLayout, storeTasty, Loaded.empty)
      assert(afterUses._3 == storeTasty.size)
      assert(afterDefns.map(_._3) == Right(0))
      assert(afterDefns.map(_._1) == fresh.map(_._1))
    }

    test("a uses call after a tasty file's mtime changes reads that file again") {
      val storeTasty = Locate.inPackageOf(
        root,
        FixtureLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val dir = os.temp.dir(prefix = "outline-uses-mtime-")
      val copy = dir / storeTasty.head.last
      os.copy(storeTasty.head, copy)
      val first = loadedUses(Vector(copy), Loaded.empty)
      assert(first._3 == 1)
      val later = os.mtime(copy) + 5000L
      Files.setLastModifiedTime(copy.toNIO, FileTime.fromMillis(later))
      val changed = loadedUses(Vector(copy), first._2)
      assert(changed._3 == 1)
      val unchanged = loadedUses(Vector(copy), changed._2)
      assert(unchanged._3 == 0)
    }

    test("a uses call looks up the classes dirs fewer times than it has candidate files") {
      var lookups = 0
      val counting = new Layout {
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = {
          lookups += 1
          FixtureLayout.classesDirs(root)
        }
        def libraryJars(root: Root): Vector[os.Path] = FixtureLayout.libraryJars(root)
        def notCompiled(root: Root): String = FixtureLayout.notCompiled(root)
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
        def isTest(classesDir: os.Path): Boolean = FixtureLayout.isTest(classesDir)
      }
      val candidates = Locate.mentioning(root, "get", SourceWords.empty)._1.size
      val (answer, _) =
        Query.uses(
          root,
          counting,
          Config.empty,
          Roots.empty(6000),
          Vector("Store.get"),
          None,
          None,
          80000
        )
      assert(answer.status == Status.Found)
      assert(lookups < candidates)
    }

    /** `n` sites in `file`, owned by `owner`, from line `from`. */
    def sitesIn(file: String, owner: String, n: Int, from: Int): Vector[Use] =
      Vector.tabulate(n)(i => Use("p.T.t", 3, file, from + i, owner, "T.t()"))

    val many = sitesIn("a/A.scala", "p.A.f", 15, 1) ++ sitesIn("b/B.scala", "p.B.g", 4, 1) ++
      sitesIn("b/B.scala", "p.B.h", 2, 10)

    test(
      "over 20 sites, uses prints each file's site count and each owner once with its count, and a line naming --sites"
    ) {
      val out = Render.uses(many, Set.empty, 80000, Map.empty, Set.empty, Map(many(0) -> 300))
      assert(
        out.linesIterator.toVector.dropRight(1) == Vector(
          "== a/A.scala  15 sites",
          "  in A.f  15 sites  [0.3 KB]",
          "== b/B.scala  6 sites",
          "  in B.g  4 sites",
          "  in B.h  2 sites",
          "-- 21 sites in 2 files; --sites to list them all, or --in/--outside to narrow"
        )
      )
    }

    test("at 20 sites, or with --sites, uses prints every site under its owner") {
      val twenty = Render.uses(many.drop(1), Set.empty, 80000, Map.empty, Set.empty, Map.empty)
      val all =
        Render.uses(many, Set.empty, 80000, Map.empty, Set.empty, Map.empty, allSites = true)
      assert(twenty.linesIterator.count(_.startsWith("    ")) == 20)
      assert(all.linesIterator.count(_.startsWith("    ")) == 21)
      assert(all.contains("\n  in B.h\n    10  T.t()\n    11  T.t()\n"))
    }

    test("an object's definition is not a use of it: uses of O lists P.b's site alone") {
      val d = os.temp.dir(prefix = "outline-uses-self-")
      os.write(d / "build.mill", "")
      val sources = Vector(
        "O" -> "object O {\n\n  /** A. */\n  val a: Vector[Int] = Vector(1)\n}",
        "P" -> "object P { def b: Vector[Int] = O.a }"
      ).map { case (file, text) =>
        val src = d / "m" / "src" / "p" / s"$file.scala"
        os.write(src, s"package p\n$text\n", createFolders = true)
        src
      }
      val classes = d / "out" / "m" / "compile.dest" / "classes"
      os.makeDir.all(classes)
      val reporter = dotty.tools.dotc.Main.process(
        Array("-d", classes.toString, "-classpath", System.getProperty("java.class.path")) ++
          sources.map(_.toString)
      )
      assert(!reporter.hasErrors)
      val (answer, _) =
        Query.uses(
          Root(d),
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("p.O"),
          None,
          None,
          80000
        )
      val lines = answer.text.linesIterator.toVector.drop(1).dropRight(2)
      assert(
        lines == Vector(
          "== m/src/p/P.scala",
          "  in P.b  [0.2 KB]",
          "    2  object P { def b: Vector[Int] = O.a }"
        )
      )
    }
  }
}
