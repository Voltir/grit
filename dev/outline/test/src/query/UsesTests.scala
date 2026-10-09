package grit.outline.query

import grit.outline.locate.{Locate, MillLayout, Root}
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
    Query.uses(root, MillLayout, Roots.empty(6000), Vector(sym), in, outside, 80000)

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
      "the uses of Limits.of include the 2-argument overload's call on line 28, which targets the 3-argument of on line 31"
    ) {
      val (answer, _) = uses("grit.outline.fixture.Limits.of")
      assert(answer.text.linesIterator.exists(_.contains("28  in Limits.of")))
      assert(answer.text.contains("[→ of :31]"))
    }

    test("the uses of Store.get outside the fixture are no match") {
      val (answer, _) = uses(s"$storePackage.Store.get", outside = Some("dev/outline/fixture"))
      assert(answer.status == Status.NoMatch)
    }
  }
}
