package grit.outline.query

import java.nio.file.Files
import java.nio.file.attribute.FileTime

import grit.outline.locate.{Layout, Locate, MillLayout, Root}
import grit.outline.model.Use
import grit.outline.read.Read

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
    Query.uses(root, MillLayout, Config.empty, Roots.empty(6000), Vector(sym), in, outside, 80000)

  /** `Loaded.uses` over `tasty` from `in`; a failure throws, so the test shows it. */
  private def loadedUses(tasty: Vector[os.Path], in: Loaded): (Vector[Use], Loaded, Int) =
    Loaded.uses(root, MillLayout, tasty, in) match {
      case Right(read) => read
      case Left(message) => throw new Exception(message)
    }

  def tests = Tests {
    test(
      "the uses of Store.get include StoreContract.keepsWhatItIsGiven and Caller.all, named by their enclosing definitions, and not MemStore.get's Map.get"
    ) {
      val storeTasty = Locate.inPackageOf(
        root,
        MillLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val sites = Read.uses(root, MillLayout, storeTasty, Set(s"$storePackage.Store.get")) match {
        case Right(us) => us
        case Left(message) => throw new Exception(message)
      }
      val enclosing = sites.map(_.enclosing).toSet
      assert(enclosing.contains(s"$storePackage.StoreContract.keepsWhatItIsGiven"))
      assert(enclosing.contains(s"$storePackage.Caller.all"))
      assert(!enclosing.contains(s"$storePackage.MemStore.get"))
    }

    test(
      "a call split across lines is a site on its name's line: its text is that line, and its enclosing definition is Caller.split"
    ) {
      val storeTasty = Locate.inPackageOf(
        root,
        MillLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val sites = Read.uses(root, MillLayout, storeTasty, Set(s"$storePackage.Store.get")) match {
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
      "the uses of Limits.of include the 2-argument overload's call on line 28, which targets the 3-argument of on line 31"
    ) {
      val (answer, _) = uses("grit.outline.fixture.Limits.of")
      assert(answer.text.linesIterator.exists(_.contains("28  in Limits.of")))
      assert(answer.text.contains("[→ of :31]"))
    }

    test(
      "a member of an object is searched for only in files that name the object: Limits.of's fixture candidates are Sample.scala alone"
    ) {
      val searches = Resolve
        .resolve(
          root,
          MillLayout,
          Config.empty,
          Scope.WithTests,
          "grit.outline.fixture.Limits.of",
          Loaded.empty
        )
        .map(r => Query.searchesFor(r.defns, r.seen))
      assert(searches == Right(Vector(Query.Search("of", Some("Limits")))))
      val inFixture = Query
        .candidates(root, Vector(Query.Search("of", Some("Limits"))))
        .filter(_.startsWith(os.RelPath("dev/outline/fixture")))
      val mentioningOf =
        Locate.mentioning(root, "of").filter(_.startsWith(os.RelPath("dev/outline/fixture")))
      assert(inFixture == Vector(os.RelPath("dev/outline/fixture/src/Sample.scala")))
      assert(mentioningOf.size > inFixture.size)
    }

    test(
      "a member of a trait is searched for in every file naming it, since a call through a value need not name the trait"
    ) {
      val searches = Resolve
        .resolve(
          root,
          MillLayout,
          Config.empty,
          Scope.WithTests,
          "grit.outline.fixture.store.Store.get",
          Loaded.empty
        )
        .map(r => Query.searchesFor(r.defns, r.seen))
      assert(searches == Right(Vector(Query.Search("get", None))))
    }

    test("the uses of Store.get outside the fixture are no match") {
      val (answer, _) = uses(s"$storePackage.Store.get", outside = Some("dev/outline/fixture"))
      assert(answer.status == Status.NoMatch)
    }

    test("a second uses call over unchanged tasty reads no file") {
      val storeTasty = Locate.inPackageOf(
        root,
        MillLayout,
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
        MillLayout,
        os.RelPath("dev/outline/fixture/src/store/Store.scala")
      )
      val afterUses = loadedUses(storeTasty, Loaded.empty)
      val afterDefns = Loaded.defns(root, MillLayout, storeTasty, afterUses._2)
      val fresh = Loaded.defns(root, MillLayout, storeTasty, Loaded.empty)
      assert(afterUses._3 == storeTasty.size)
      assert(afterDefns.map(_._3) == Right(0))
      assert(afterDefns.map(_._1) == fresh.map(_._1))
    }

    test("a uses call after a tasty file's mtime changes reads that file again") {
      val storeTasty = Locate.inPackageOf(
        root,
        MillLayout,
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
          MillLayout.classesDirs(root)
        }
        def libraryJars(root: Root): Vector[os.Path] = MillLayout.libraryJars(root)
        def notCompiled(root: Root): String = MillLayout.notCompiled(root)
        def isTest(classesDir: os.Path): Boolean = MillLayout.isTest(classesDir)
      }
      val candidates = Locate.mentioning(root, "get").size
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
  }
}
