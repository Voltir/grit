package grit.projects.plugin

import grit.core.id.DocKey
import grit.core.plugin.CommandOutcome
import grit.core.visibility.{Clearance, Label, TestLabels}

import utest.*

object ReorderTests extends TestSuite {
  import CommandFixtures.*

  /** The rank `project_show` gives `key`. */
  private def rankOf(projects: Projects, store: Store, key: String): String =
    shown(projects, store, key).linesIterator.find(_.startsWith("rank: ")).fold("")(_.drop(6))

  /** A project `p` holding `a`, `b` and `c` (`p/1` to `p/3`), under no parent, in that order. */
  private def three(): (Projects, Store) = {
    val projects = plugin()
    val store = Store()
    started(projects, store)
    Vector("a", "b", "c").foreach(l => kept(projects, store, l, "note", s"Text of $l."))
    (projects, store)
  }

  private def reorder(projects: Projects, store: Store, args: (String, ujson.Value)*) =
    ask(projects, store, "project_reorder", args*)

  val tests = Tests {
    test(
      "a reorder with no room left re-spreads every sibling evenly, a version for each it re-ranks"
    ) {
      val (projects, store) = three()
      rankedAt(store, "p/2", "n")
      def versions(key: String): Int =
        done(ask(projects, store, "project_history", "key" -> key)).linesIterator.size
      val siblings = Vector("p/1", "p/2", "p/3")
      val before = siblings.map(versions)
      val answer = done(reorder(projects, store, "artifact" -> "p/c", "after" -> "p/a"))
      (
        answer,
        Vector("p/1", "p/3", "p/2").map(rankOf(projects, store, _)),
        siblings.map(versions)
      ) ==> (
        "Placed p/3 c (note, open) after p/1 a (note, open). Re-ranked 2 siblings to make room.",
        Vector("h", "n", "u"),
        before.map(_ + 1)
      )
    }
    test("a keep that re-spreads its siblings answers the version of the artifact it kept") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      Vector("a", "b", "c", "d", "e").foreach(kept(projects, store, _, "note", "Words."))
      rankedAt(store, "p/5", "z" * 32)
      val answer = done(
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "f",
          "kind" -> "note",
          "title" -> "F",
          "body" -> "More words."
        )
      )
      answer ==> ("Kept p/6 f (note, open). Re-ranked 5 siblings to make room. " +
        s"Now at version ${versionOf(projects, store, "p/6")}.")
    }
    test("a reorder with room gives the moved artifact alone a rank between its new neighbours") {
      val (projects, store) = three()
      val versions = Vector("p/1", "p/2").map(versionOf(projects, store, _))
      val answer = done(reorder(projects, store, "artifact" -> "p/c", "before" -> "p/a"))
      (
        answer,
        Vector("p/3", "p/1", "p/2").map(rankOf(projects, store, _)),
        Vector("p/1", "p/2").map(versionOf(projects, store, _))
      ) ==> (
        "Placed p/3 c (note, open) before p/1 a (note, open).",
        Vector("g", "n", "naab"),
        versions
      )
    }
    test("a reorder to where an artifact already sits keeps nothing, and says so") {
      val (projects, store) = three()
      val version = versionOf(projects, store, "p/2")
      val answer = done(reorder(projects, store, "artifact" -> "p/b", "after" -> "p/a"))
      (answer, versionOf(projects, store, "p/2")) ==> (
        "p/2 b (note, open) already sits after p/1 a (note, open).",
        version
      )
    }
    test("a reorder beside the artifact itself, or beside one under another parent, is refused") {
      val (projects, store) = three()
      done(
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "d",
          "kind" -> "note",
          "title" -> "D",
          "body" -> "Under a.",
          "parent" -> "p/a"
        )
      )
      (
        reorder(projects, store, "artifact" -> "p/b", "after" -> "p/b"),
        reorder(projects, store, "artifact" -> "p/d", "before" -> "p/b")
      ) ==> (
        CommandOutcome.Refused("p/2 b cannot sit beside itself"),
        CommandOutcome.Refused(
          "p/2 b is no sibling of p/4 d: it sits under no parent, and p/4 d under p/1"
        )
      )
    }
    test(
      "a re-spread from a room above its siblings' label is refused"
    ) {
      val (projects, store) = three()
      keptAbove(store, "p/3", TestLabels.Trial)
      def versions(key: String, label: Label): Int = {
        given grit.core.store.Tx = grit.dbos.sql.TestTx.fake(Clearance.of(TestLabels.Trial))
        val docKey = DocKey.of(key).fold(sys.error, identity)
        store.keeper.versions(docKey, label, 100).fold(e => sys.error(e.toString), _.size)
      }
      val above =
        askBy(direct(), at, Clearance.of(TestLabels.Trial))(projects, store, "project_reorder", _*)
      // With room, only c, kept at the room's label, changes.
      val roomy = above(Vector("artifact" -> "p/c", "before" -> "p/a"))
      rankedAt(store, "p/2", "n")
      val before = Vector(
        versions("p/1", Label.Public),
        versions("p/2", Label.Public),
        versions("p/3", TestLabels.Trial)
      )
      val answer = above(Vector("artifact" -> "p/c", "after" -> "p/a"))
      (
        roomy,
        answer,
        Vector(
          versions("p/1", Label.Public),
          versions("p/2", Label.Public),
          versions("p/3", TestLabels.Trial)
        )
      ) ==> (
        CommandOutcome.Done("Placed p/3 c (note, open) before p/1 a (note, open)."),
        CommandOutcome.Refused(
          Vector("p/2", "p/1")
            .map(subject =>
              s"$subject is kept labelled [level: public], below what this room writes " +
                "([level: public, in: {trial}]): reorder from a room at its label"
            )
            .mkString("; ")
        ),
        before
      )
    }
    test("a re-spread re-ranks a sibling another session holds, its brief as it was") {
      val (projects, store) = three()
      rankedAt(store, "p/2", "n")
      done(
        ask(
          projects,
          store,
          "project_claim",
          "key" -> "p/b",
          "role" -> "builder",
          "holder" -> "coder-opus/abc"
        )
      )
      val brief = briefOf(store, "coder-opus/abc")
      val answer = done(reorder(projects, store, "artifact" -> "p/c", "after" -> "p/a"))
      (
        answer,
        rankOf(projects, store, "p/2"),
        briefOf(store, "coder-opus/abc").map(_.version)
      ) ==> (
        "Placed p/3 c (note, open) after p/1 a (note, open). Re-ranked 2 siblings to make room.",
        "u",
        brief.map(_.version)
      )
    }
    test("a reorder naming both after and before, or neither, is refused") {
      val projects = plugin()
      (
        refusedArgs(
          projects,
          "project_reorder",
          "artifact" -> "p/a",
          "after" -> "p/b",
          "before" -> "p/c"
        ),
        refusedArgs(projects, "project_reorder", "artifact" -> "p/a")
      ) ==> (
        Some("Give after or before, not both."),
        Some("Give after or before: the sibling it is to sit beside.")
      )
    }
  }
}
