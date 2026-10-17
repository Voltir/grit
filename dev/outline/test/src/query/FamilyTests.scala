package grit.outline.query

import grit.outline.locate.{Locate, Root}
import grit.outline.read.Read
import grit.outline.testing.FixtureLayout

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

  private def family(
      name: String,
      member: Option[String],
      withBody: Boolean,
      config: Config = Config.empty
  ): (Answer, Roots) =
    Query.family(
      root,
      FixtureLayout,
      config,
      Roots.empty(6000),
      name,
      member,
      withBody,
      80000
    )

  /** The `-- not compiled, not searched:` note's opening: the files it names are this checkout's sources that mention a
    * fixture's name and that no .tasty of the fixture's layout records, so they change with the repository; this suite's
    * own file is always one.
    */
  private val unsearched = "-- not compiled, not searched: …"

  /** `answer`'s lines after its root line, a not-searched note cut to `unsearched`. */
  private def afterRoot(answer: Answer): Vector[String] =
    answer.text.linesIterator
      .drop(1)
      .map(l => if (l.startsWith(unsearched.dropRight(1))) unsearched else l)
      .toVector

  def tests = Tests {
    test(
      "the implementations of Store are MemStore and NoStore, and its contract StoreContract is run by MemStoreContract"
    ) {
      val defns = Read.defns(
        root,
        FixtureLayout,
        Locate
          .inPackageIn(
            root,
            FixtureLayout.classesDirs(root),
            os.RelPath("dev/outline/fixture/src/store/Store.scala")
          )
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
      "with member put, the trait shows put and not get, and each implementation ends a line with put and its size"
    ) {
      val (answer, _) = family("Store", Some("put"), false)
      assert(answer.text.contains("  def put(key: String, value: String): Store"))
      assert(!answer.text.contains("def get(key: String): Option[String]"))
      assert(answer.text.linesIterator.count(_.endsWith(" put  [0.2 KB]")) == 2)
      assert(answer.text.linesIterator.contains("9-10 def Store.put  [0.2 KB]"))
    }

    test(
      "the trait's header, each implementation's line and each contract's line end with its size"
    ) {
      val lines = family("Store", None, false)._1.text.linesIterator.toVector
      assert(lines.contains("3-11  [0.2 KB]"))
      assert(lines.containsSlice(Vector("6-7  [0.2 KB]", "  /** The value at `key`, if any. */")))
      assert(
        lines.contains(
          "13-17 case class MemStore  final case class MemStore(values: Map[String, String]) extends Store  [0.2 KB]"
        )
      )
      assert(
        lines.contains(
          "25-33 contract StoreContract  abstract class StoreContract  [0.2 KB]"
        )
      )
    }

    test("the trait's closing lines end its entry, after its members: Store's 10-11") {
      val lines = family("Store", None, false)._1.text.linesIterator.toVector
      val at = lines.indexOf("10-11 end of Store")
      assert(at > lines.indexWhere(_.startsWith("9-10 def Store.put")))
      assert(
        lines.slice(at + 1, at + 3) == Vector("  def put(key: String, value: String): Store", "}")
      )
    }

    test("with member put, the trait's put prints its doc and signature as the file's own lines") {
      val (answer, _) = family("Store", Some("put"), false)
      val lines = answer.text.linesIterator.toVector
      val at = lines.indexWhere(_.startsWith("9-10 def Store.put"))
      val file = os.read
        .lines(root.dir / "dev" / "outline" / "fixture" / "src" / "store" / "Store.scala")
        .toVector
      assert(at >= 0)
      assert(lines.slice(at + 1, at + 3) == file.slice(8, 10))
    }

    test("with member put and body, MemStore's put shows its body") {
      val (answer, _) = family("Store", Some("put"), true)
      assert(answer.text.contains("MemStore(values + (key -> value))"))
    }

    test(
      "a name nothing defines is NoMatch, under resolve's note naming the find to try"
    ) {
      val (answer, _) = family("Nope", None, false)
      assert(answer.status == Status.NoMatch)
      assert(
        afterRoot(answer) == Vector(
          "-- no match for Nope; try find *Nope*",
          unsearched,
          "no trait or abstract class named Nope"
        )
      )
    }

    test(
      "a short name two traits share is NoMatch listing both, and no family is picked from them"
    ) {
      val (answer, _) = family("Tx", None, false)
      assert(answer.status == Status.NoMatch)
      assert(
        answer.text.linesIterator.drop(1).toVector == Vector(
          "-- ambiguous Tx, name one fully:",
          "--   grit.outline.fixture.Tx  dev/outline/fixture/src/Sample.scala:3-4  trait Tx extends caps.SharedCapability  [0.2 KB]",
          "--   grit.outline.fixture.store.Tx  dev/outline/fixture/src/store/Store.scala:53-54  trait Tx  [0.2 KB]"
        )
      )
    }

    test(
      "a short name with one primary trait among its candidates is that trait's family, under the note naming the other"
    ) {
      val secondary = Config.empty.copy(secondary = Vector("dev/outline/fixture/src/store"))
      val (answer, _) = family("Tx", None, false, secondary)
      val lines = answer.text.linesIterator.toVector
      assert(answer.status == Status.Found)
      assert(
        lines.lift(1) == Some(
          "-- Tx taken as grit.outline.fixture.Tx, the only one outside test and secondary sources; also grit.outline.fixture.store.Tx (secondary: dev/outline/fixture/src/store): name it fully to read it"
        )
      )
      assert(lines.contains("== dev/outline/fixture/src/Sample.scala  grit.outline.fixture"))
      assert(!lines.exists(_.startsWith("== dev/outline/fixture/src/store/Store.scala")))
    }

    test(
      "an object, a case class or a def is NoMatch saying which it is, with the show and the uses that answer for it"
    ) {
      def noFamily(name: String, kind: String): Vector[String] = Vector(
        unsearched,
        s"$name is $kind, not a trait or abstract class, so it has no family: show $name reads it, uses $name lists what names it"
      )
      val answers = Vector("Unrelated", "Spot", "noteOf").map(n => family(n, None, false)._1)
      assert(answers.map(_.status) == Vector.fill(3)(Status.NoMatch))
      assert(
        answers.map(afterRoot) == Vector(
          noFamily("grit.outline.fixture.store.Unrelated", "an object"),
          noFamily("grit.outline.fixture.Spot", "a case class"),
          "-- no noteOf as written; by last segment: grit.outline.fixture.Fields.noteOf" +:
            noFamily("grit.outline.fixture.Fields.noteOf", "a def")
        )
      )
    }

    test("the .outline.conf lines this outline ignored are named after an answer's root line") {
      val config = Config.empty.copy(ignored = Vector("line 2: frobnicate x", "line 5: zap"))
      val (answer, _) = family("Store", None, false, config)
      assert(
        answer.text.linesIterator.drop(1).nextOption() == Some(
          "-- .outline.conf lines this outline does not know, ignored (a newer outline may read them): line 2: frobnicate x, line 5: zap"
        )
      )
    }

    test(
      "an opaque type is NoMatch saying it is one, with the show and the uses that answer for it"
    ) {
      val (answer, _) = family("Ids.Id", None, false)
      assert(answer.status == Status.NoMatch)
      assert(
        afterRoot(answer) ==
          Vector(
            unsearched,
            "grit.outline.fixture.Ids.Id is an opaque type, not a trait or abstract class, so it has no family: show grit.outline.fixture.Ids.Id reads it, uses grit.outline.fixture.Ids.Id lists what names it"
          )
      )
    }
  }
}
