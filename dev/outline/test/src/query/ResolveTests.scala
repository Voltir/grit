package grit.outline.query

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.testing.{FixtureLayout, FixtureSuitesLayout}

import utest.*

object ResolveTests extends TestSuite {

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

  private def resolve(
      sym: String,
      config: Config = Config.empty,
      scope: Scope = Scope.Main,
      layout: Layout = FixtureLayout
  ): Resolved =
    Resolve.resolve(root, layout, config, scope, sym, Loaded.empty) match {
      case Right(r) => r
      case Left(message) => throw new Exception(message)
    }

  def tests = Tests {
    test("a fully named member resolves to exactly that one definition, with no note") {
      val r = resolve("grit.outline.fixture.A.keep")
      assert(r.defns.map(_.fullName) == Vector("grit.outline.fixture.A.keep"))
      assert(r.notes.isEmpty)
    }

    test(
      "a fully named member of a companion nested in an object resolves to exactly that definition, with no note"
    ) {
      val r = resolve("grit.outline.fixture.Holder.Nested.of")
      assert(r.defns.map(_.fullName) == Vector("grit.outline.fixture.Holder.Nested.of"))
      assert(r.notes.isEmpty)
    }

    test(
      "a short name shared by two packages resolves to both, and the notes give each one's file, lines and signature"
    ) {
      val r = resolve("Tx")
      assert(
        r.defns.map(_.fullName).distinct.sorted == Vector(
          "grit.outline.fixture.Tx",
          "grit.outline.fixture.store.Tx"
        )
      )
      assert(
        r.notes == Vector(
          "-- ambiguous Tx, name one fully:",
          "--   grit.outline.fixture.Tx  dev/outline/fixture/src/Sample.scala:3-4  trait Tx extends caps.SharedCapability  [0.2 KB]",
          "--   grit.outline.fixture.store.Tx  dev/outline/fixture/src/store/Store.scala:53-54  trait Tx  [0.2 KB]"
        )
      )
    }

    test(
      "a short name one main source and one test source define resolves to the main one, and one note names the other"
    ) {
      val r = resolve("Pairs", scope = Scope.WithTests, layout = FixtureSuitesLayout)
      assert(r.defns.map(_.fullName).distinct == Vector("grit.outline.fixture.store.Pairs"))
      assert(
        r.notes == Vector(
          "-- Pairs taken as grit.outline.fixture.store.Pairs, the only one outside test and secondary sources; also grit.outline.fixture.suites.Pairs (test sources): name it fully to read it"
        )
      )
    }

    test(
      "a short name one primary source and one secondary source define resolves to the primary one, and one note names the other"
    ) {
      val secondary = Config.empty.copy(secondary = Vector("dev/outline/fixture/src/store"))
      val r = resolve("Tx", secondary)
      assert(r.defns.map(_.fullName).distinct == Vector("grit.outline.fixture.Tx"))
      assert(
        r.notes == Vector(
          "-- Tx taken as grit.outline.fixture.Tx, the only one outside test and secondary sources; also grit.outline.fixture.store.Tx (secondary: dev/outline/fixture/src/store): name it fully to read it"
        )
      )
    }

    test("two secondary candidates and no primary one stay ambiguous, each line tagged secondary") {
      val secondary = Config.empty.copy(secondary = Vector("dev/outline/fixture/src"))
      val r = resolve("Tx", secondary)
      assert(r.defns.map(_.fullName).distinct.size == 2)
      assert(
        r.notes.drop(1) == Vector(
          "--   grit.outline.fixture.Tx  dev/outline/fixture/src/Sample.scala:3-4  trait Tx extends caps.SharedCapability  [0.2 KB]  (secondary: dev/outline/fixture/src)",
          "--   grit.outline.fixture.store.Tx  dev/outline/fixture/src/store/Store.scala:53-54  trait Tx  [0.2 KB]  (secondary: dev/outline/fixture/src)"
        )
      )
    }

    test(
      "a qualifier naming no candidate's owner resolves to none, and the note lists the candidate"
    ) {
      val r = resolve("Wrong.Mix")
      assert(r.defns.isEmpty)
      assert(
        r.notes == Vector(
          "-- no Wrong.Mix as written; by last segment, name one fully: grit.outline.fixture.Colour.Mix"
        )
      )
    }

    test("a wrong qualifier naming one candidate's owner resolves to that candidate alone") {
      val r = resolve("Wrong.store.Quiver")
      assert(r.defns.map(_.fullName) == Vector("grit.outline.fixture.store.Quiver"))
      assert(r.notes.exists(_.startsWith("-- no Wrong.store.Quiver as written; by last segment:")))
    }

    test(
      "a qualifier matching no candidate's owner, with several candidates, resolves to none and lists them"
    ) {
      val r = resolve("Wrong.Quiver")
      assert(r.defns.isEmpty)
      assert(
        r.notes == Vector(
          "-- no Wrong.Quiver as written; by last segment, name one fully: grit.outline.fixture.Quiver, grit.outline.fixture.store.Quiver"
        )
      )
    }

    test("a hidden source is found by its full name only, never by a short name") {
      val hidden = Config(Vector("dev/outline/fixture"), Vector.empty)
      assert(resolve("A.keep", hidden).defns.forall(d => !d.file.startsWith("dev/outline/fixture")))
      assert(
        resolve("grit.outline.fixture.A.keep", hidden).defns.map(_.fullName) == Vector(
          "grit.outline.fixture.A.keep"
        )
      )
    }

    test(
      "a .outline.conf's hide lines are read, comments skipped, and a bad line is named by number"
    ) {
      val bad = os.temp.dir()
      os.write(bad / ".outline.conf", "hide a/b\n# a comment\nhide\n")
      assert(Config.read(Root(bad)).left.toOption.exists(_.contains("line 3")))
      val good = os.temp.dir()
      os.write(good / ".outline.conf", "hide a/b\n# a comment\n")
      assert(Config.read(Root(good)) == Right(Config(Vector("a/b"), Vector.empty)))
    }

    test("a name nothing defines resolves to nothing, and the note says there is no match") {
      val r = resolve("grit.outline.fixture.Nonexistent")
      assert(r.defns.isEmpty)
      assert(
        r.notes == Vector(
          "-- no match for grit.outline.fixture.Nonexistent; try find *Nonexistent*"
        )
      )
    }

    test("preload reads each name's files once, and a resolve after it reads none") {
      val syms = Vector("grit.outline.fixture.A.keep", "grit.outline.fixture.store.Store")
      val tasty = syms.flatMap(sym => Locate.forTopLevel(root, FixtureLayout, sym)).distinct
      val expected = Loaded.defns(root, FixtureLayout, tasty, Loaded.empty) match {
        case Right((_, _, read)) => read
        case Left(message) => throw new Exception(message)
      }
      val preloaded = Resolve.preload(root, FixtureLayout, Scope.Main, syms, Loaded.empty) match {
        case Right(in) => in
        case Left(message) => throw new Exception(message)
      }
      assert(expected > 0)
      assert(preloaded.byTasty.size == expected)
      assert(Loaded.defns(root, FixtureLayout, tasty, preloaded).map(_._3) == Right(0))
      syms.foreach { sym =>
        Resolve.resolve(root, FixtureLayout, Config.empty, Scope.Main, sym, preloaded) match {
          case Right(r) => assert(r.loaded.byTasty.keySet == preloaded.byTasty.keySet)
          case Left(message) => throw new Exception(message)
        }
      }
    }
  }
}
