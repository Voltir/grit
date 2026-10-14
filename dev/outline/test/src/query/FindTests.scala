package grit.outline.query

import grit.outline.locate.Root
import grit.outline.testing.{FixtureLayout, FixtureTestLayout}

import utest.*

object FindTests extends TestSuite {

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

  private def find(pattern: String): Answer =
    Query.find(root, FixtureLayout, Config.empty, Roots.empty(6000), pattern, 80000)._1

  /** The full names an answer lists, one per match line. */
  private def names(answer: Answer): Vector[String] =
    answer.text.linesIterator
      .drop(1)
      .filterNot(_.startsWith("["))
      .map(_.takeWhile(_ != ' '))
      .toVector

  def tests = Tests {
    test(
      "a one-segment pattern matches simple names, one line each with the file, lines and collapsed signature"
    ) {
      val answer = find("*Store*")
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains(
          "grit.outline.fixture.store.MemStore  dev/outline/fixture/src/store/Store.scala:13-17  final case class MemStore(values: Map[String, String]) extends Store"
        )
      )
      assert(
        names(answer) == Vector(
          "grit.outline.fixture.store.MemStore",
          "grit.outline.fixture.store.MemStoreContract",
          "grit.outline.fixture.store.NoStore",
          "grit.outline.fixture.store.Store",
          "grit.outline.fixture.store.StoreContract",
          "grit.outline.fixture.store.StoreSuite"
        )
      )
    }

    test("*.get lists the member get of every owner, and no other member") {
      assert(
        names(find("*.get")) == Vector(
          "grit.outline.fixture.store.MemStore.get",
          "grit.outline.fixture.store.NoStore.get",
          "grit.outline.fixture.store.Store.get",
          "grit.outline.fixture.store.Unrelated.get"
        )
      )
    }

    test(
      "each segment of a dotted pattern matches one whole segment: Lim*.of finds Limits.of's overloads, not Nested.of"
    ) {
      assert(
        names(find("Lim*.of")) == Vector(
          "grit.outline.fixture.Limits.of",
          "grit.outline.fixture.Limits.of"
        )
      )
    }

    test(
      "a segment matches a whole name segment, never part of one: imits.of and Lim*.o are NoMatch"
    ) {
      assert(find("imits.of").status == Status.NoMatch)
      assert(find("Lim*.o").status == Status.NoMatch)
    }

    test("a case class's constructor val is listed as its member: note finds B.note on B's line") {
      val answer = find("note")
      assert(names(answer) == Vector("grit.outline.fixture.B.note"))
      assert(
        answer.text.contains(
          "grit.outline.fixture.B.note  dev/outline/fixture/src/Sample.scala:17-17  note: String"
        )
      )
    }

    test(
      "a name only test sources define is listed after a note saying none in main sources matches"
    ) {
      val (answer, _) =
        Query.find(root, FixtureTestLayout, Config.empty, Roots.empty(6000), "Nested", 80000)
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains(
          "-- no definition in main sources matches Nested; in test sources:"
        )
      )
      assert(names(answer).contains("grit.outline.fixture.Holder.Nested"))
    }

    test("a name only a renaming import gives is named as one, with its target's definition") {
      val answer = find("Counted")
      assert(answer.status == Status.Found)
      assert(
        answer.text.linesIterator.contains(
          "-- Counted is a renaming import of grit.outline.fixture.Holder.Nested at dev/outline/fixture/src/Sample.scala:105"
        )
      )
      assert(names(answer).contains("grit.outline.fixture.Holder.Nested"))
    }

    test("a private member is not listed, and a pattern nothing matches is NoMatch saying so") {
      val answer = find("hidd*")
      assert(answer.status == Status.NoMatch)
      assert(answer.text.endsWith("\nno definition matches hidd*"))
    }

    test("a pattern with an empty segment is Failed, naming the pattern") {
      val answer = find("Limits..of")
      assert(answer.status == Status.Failed)
      assert(answer.text.contains("Limits..of has an empty segment"))
    }
  }
}
