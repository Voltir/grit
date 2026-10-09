package grit.outline.query

import grit.outline.locate.{MillLayout, Root}

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
      scope: Scope = Scope.Main
  ): Resolved =
    Resolve.resolve(root, MillLayout, config, scope, sym, Loaded.empty) match {
      case Right(r) => r
      case Left(message) => throw new Exception(message)
    }

  def tests = Tests {
    test("a fully named member resolves to exactly that one definition, with no note") {
      val r = resolve("grit.outline.fixture.A.keep")
      assert(r.defns.map(_.fullName) == Vector("grit.outline.fixture.A.keep"))
      assert(r.notes.isEmpty)
    }

    test("a short name shared by two packages resolves to both, and the note names both") {
      val r = resolve("Tx")
      assert(
        Set("grit.outline.fixture.Tx", "grit.outline.fixture.store.Tx").subsetOf(
          r.defns.map(_.fullName).toSet
        )
      )
      assert(r.notes.exists(_.startsWith("-- ambiguous Tx:")))
      assert(
        r.notes.exists(n =>
          n.contains("grit.outline.fixture.Tx") && n.contains("grit.outline.fixture.store.Tx")
        )
      )
    }

    test("a wrong qualifier falls back to the last segment, and the note says so") {
      val r = resolve("Wrong.Mix")
      assert(r.defns.map(_.fullName) == Vector("grit.outline.fixture.Colour.Mix"))
      assert(r.notes.exists(_.startsWith("-- no Wrong.Mix as written; by last segment:")))
    }

    test("a hidden source is found by its full name only, never by a short name") {
      val hidden = Config(Vector("dev/outline/fixture"))
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
      os.write(bad / ".outline.conf", "hide a/b\n# a comment\nbogus\n")
      assert(Config.read(Root(bad)).left.toOption.exists(_.contains("line 3")))
      val good = os.temp.dir()
      os.write(good / ".outline.conf", "hide a/b\n# a comment\n")
      assert(Config.read(Root(good)) == Right(Config(Vector("a/b"))))
    }
  }
}
