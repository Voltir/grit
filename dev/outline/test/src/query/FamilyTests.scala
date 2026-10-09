package grit.outline.query

import grit.outline.locate.{Locate, MillLayout, Root}
import grit.outline.read.Read

import utest.*

object FamilyTests extends TestSuite {

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

  private def family(name: String, member: Option[String], withBody: Boolean): (Answer, Roots) =
    Query.family(root, MillLayout, Roots.empty(6000), name, member, withBody, 80000)

  def tests = Tests {
    test(
      "the implementations of Store are MemStore and NoStore, and its contract StoreContract is run by MemStoreContract"
    ) {
      val defns = Read.defns(
        root,
        MillLayout,
        Locate
          .inPackageOf(root, MillLayout, os.RelPath("dev/outline/fixture/src/store/Store.scala"))
      ) match {
        case Right(ds) => ds
        case Left(message) => throw new Exception(message)
      }
      val result = Query.familyOf(defns, "Store", None, false)
      val impls = result.toOption.map(_.impls.map(_.fullName).toSet).getOrElse(Set.empty[String])
      assert(impls == Set(s"$storePackage.MemStore", s"$storePackage.NoStore"))
      val contracts =
        result.toOption.map(_.contracts.map(_._1.fullName).toSet).getOrElse(Set.empty[String])
      assert(contracts == Set(s"$storePackage.StoreContract"))
      val suites = result.toOption
        .map(_.contracts.flatMap(_._2.map(_.fullName)).toSet)
        .getOrElse(Set.empty[String])
      assert(suites == Set(s"$storePackage.MemStoreContract"))
    }

    test(
      "with member put, the trait shows put and not get, and each implementation ends a line with put"
    ) {
      val (answer, _) = family("Store", Some("put"), false)
      assert(answer.text.contains("  def put(key: String, value: String): Store"))
      assert(!answer.text.contains("def get(key: String): Option[String]"))
      assert(answer.text.linesIterator.count(_.endsWith(" put")) == 2)
    }

    test("with member put and body, MemStore's put shows its body") {
      val (answer, _) = family("Store", Some("put"), true)
      assert(answer.text.contains("MemStore(values + (key -> value))"))
    }

    test("a name that is no trait or abstract class is NoMatch") {
      val (answer, _) = family("Nope", None, false)
      assert(answer.status == Status.NoMatch)
    }

    test("an object, Unrelated, is NoMatch: it is not a trait") {
      val (answer, _) = family("Unrelated", None, false)
      assert(answer.status == Status.NoMatch)
    }
  }
}
