package grit.projects.plugin

import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.plugin.CommandOutcome
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.dbos.sql.TestTx
import grit.projects.method.{ArtifactKind, MethodFixtures, Transition, TransitionAction}

import utest.*

/** A large artifact made a tree by `project_split`: each `##` section a child of kind `section`,
  * the artifact left its lead and a list of links to them.
  */
object SplitTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{change, kind, note, state, transitions}

  private val section: ArtifactKind =
    ArtifactKind(kind("section"), state("current"), Vector.empty, Set.empty, Vector.empty)

  private val plan = "Lead.\n\n## Goal\n\nOne.\n\n## Reads\n\nTwo.\n\n## Commits\n\nThree."

  /** Project `p` under a method with sections, holding `p/1` (handle `a`), a note of `body`. */
  private def holding(body: String = plan): (Projects, Store) = {
    val projects = plugin()
    val store = Store(declared = Vector(method(kinds = Vector(change, note, section))))
    started(projects, store)
    kept(projects, store, "a", "note", body)
    (projects, store)
  }

  private def split(projects: Projects, store: Store, version: Long): CommandOutcome =
    ask(projects, store, "project_split", "artifact" -> "p/1", "version" -> version)

  /** `key`'s text as `project_show` gives it, after its front matter. */
  private def body(projects: Projects, store: Store, key: String): String =
    shown(projects, store, key).split("\n---\n", 2).lift(1).getOrElse("")

  val tests: Tests = Tests {
    test("a split of a plan with three sections gives three children and a contents list") {
      val (projects, store) = holding()
      val answer = done(split(projects, store, versionOf(projects, store, "p/1")))
      val children = done(
        ask(projects, store, "project_list", "project" -> "p", "parent" -> "p/1")
      )
      (
        answer,
        body(projects, store, "p/1"),
        children,
        Vector("p/2", "p/3", "p/4").map(body(projects, store, _))
      ) ==> (
        "Split p/1 a (note, open), each section moved to an artifact of its own under it: " +
          "p/2 Goal; p/3 Reads; p/4 Commits. Now at version 8.",
        "\nLead.\n\n- [Goal](artifact:p/2)\n- [Reads](artifact:p/3)\n- [Commits](artifact:p/4)\n",
        "3 match: section 3 (current 3).\n" +
          "p/2 (section, current): Goal · 0.1 KB · changed 2026-10-27\n" +
          "p/3 (section, current): Reads · 0.1 KB · changed 2026-10-27\n" +
          "p/4 (section, current): Commits · 0.1 KB · changed 2026-10-27",
        Vector("\nOne.\n", "\nTwo.\n", "\nThree.\n")
      )
    }
    test("the sections a kind must hold and each delta stay, below the list, and it moves on") {
      val projects = plugin()
      val store = Store(declared = Vector(method(kinds = Vector(change, note, section))))
      started(projects, store)
      kept(
        projects,
        store,
        "a",
        "change",
        "## Why\n\nBecause.\n\n## How\n\nSo.\n\n## Delta: Spec\n\nMore."
      )
      val answer = done(split(projects, store, versionOf(projects, store, "p/1")))
      val moved = Vector("building", "done").map(to =>
        ask(projects, store, "project_move", "key" -> "p/1", "state" -> to)
      )
      (answer, body(projects, store, "p/1"), body(projects, store, "p/2"), moved.last) ==> (
        "Split p/1 a (change, proposed), each section moved to an artifact of its own under " +
          "it: p/2 How. Now at version 6.",
        "\n- [How](artifact:p/2)\n\n## Why\n\nBecause.\n\n## Delta: Spec\n\nMore.\n",
        "\nSo.\n",
        CommandOutcome.Done("Moved p/1 a (change, done).")
      )
    }
    test("a body whose every section stays, or that has none, is refused, keeping nothing") {
      val (projects, plain) = holding("Just a lead.")
      // A change must hold `## Why`.
      val projects2 = plugin()
      val store2 = Store(declared = Vector(method(kinds = Vector(change, note, section))))
      started(projects2, store2)
      kept(projects2, store2, "a", "change", "## Why\n\nBecause.")
      (
        split(projects2, store2, versionOf(projects2, store2, "p/1")),
        split(projects, plain, versionOf(projects, plain, "p/1")),
        body(projects2, store2, "p/1")
      ) ==> (
        CommandOutcome.Refused("p/1 a cannot be split: each of its sections stays in it: Why"),
        CommandOutcome.Refused("p/1 a cannot be split: it has no `##` section"),
        "\n## Why\n\nBecause.\n"
      )
    }
    test("a split at a version kept over since is refused naming the current one") {
      val (projects, store) = holding()
      val read = versionOf(projects, store, "p/1")
      kept(projects, store, "a", "note", plan + "\n\nMore.")
      split(projects, store, read) ==> CommandOutcome.Refused(
        s"p/1 a is at version ${versionOf(projects, store, "p/1")}, not $read as given: kept " +
          s"since by $by at $at. Show it again and keep from what it holds now."
      )
    }
    test("sections sharing a heading, a blank heading, or one holding nothing are refused") {
      def refused(body: String): CommandOutcome = {
        val (projects, store) = holding(body)
        split(projects, store, versionOf(projects, store, "p/1"))
      }
      (
        refused("## Goal\n\nOne.\n\n## Goal\n\nTwo."),
        refused("## Goal\n\n## Reads\n\nTwo."),
        refused("## Goal\n\nOne.\n\n# Top\n\nTwo."),
        refused("## Goal\n\nOne.\n\n##\n\nTwo.")
      ) ==> (
        CommandOutcome.Refused("p/1 a cannot be split: it has several sections titled \"Goal\""),
        CommandOutcome
          .Refused("p/1 a cannot be split: its section \"Goal\" holds nothing under its heading"),
        CommandOutcome
          .Refused("p/1 a cannot be split: its heading \"Top\" sits below its first `##` heading"),
        CommandOutcome.Refused("p/1 a cannot be split: a section's heading is blank")
      )
    }
    test("a kind whose sections another kind's deltas merge into is refused") {
      val merging = Transition(
        kind("change"),
        state("building"),
        state("done"),
        Vector.empty,
        TransitionAction.MergeDeltas(kind("note"))
      )
      val projects = plugin()
      val store = Store(declared =
        Vector(method(moves = transitions :+ merging, kinds = Vector(change, note, section)))
      )
      started(projects, store)
      kept(projects, store, "a", "note", plan)
      split(projects, store, versionOf(projects, store, "p/1")) ==> CommandOutcome.Refused(
        "p/1 a cannot be split: a move of a change merges its deltas into the sections of a note"
      )
    }
    test("a new artifact over the stored limit is refused naming no split, having none to split") {
      val (projects, store) = holding()
      val long = Vector.fill(3000)("ten chars.").mkString(" ")
      ask(
        projects,
        store,
        "project_keep",
        "project" -> "p",
        "kind" -> "note",
        "title" -> "Long",
        "body" -> s"## Goal\n\n$long"
      ) ==> CommandOutcome.Refused("a body's stored form is 33113 bytes, over 28672")
    }
    test("a method with no kind section refuses the split, keeping nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", plan)
      (split(projects, store, versionOf(projects, store, "p/1")), body(projects, store, "p/1")) ==>
        (
          CommandOutcome.Refused(
            "p/1 a cannot be split: section is no kind of method grit; its kinds: change, note"
          ),
          "\n" + plan + "\n"
        )
    }
    test(
      "each part is believed at no more than the artifact it was cut from, kept below its caller"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(method(kinds = Vector(change, note, section))))
      started(projects, store)
      done(
        askBelieved(Integrity.Member)(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "a",
          "kind" -> "note",
          "title" -> "A",
          "body" -> plan
        )
      )
      done(
        askBelieved(Integrity.Authority)(
          projects,
          store,
          "project_split",
          "artifact" -> "p/1",
          "version" -> versionOf(projects, store, "p/1")
        )
      )
      (believedOf(store, "p/1"), believedOf(store, "p/2")) ==>
        (Some(Integrity.written(Integrity.Member)), Some(Integrity.written(Integrity.Member)))
    }
    test("each part is believed at no more than its caller, below the artifact it was cut from") {
      val projects = plugin()
      val store = Store(declared = Vector(method(kinds = Vector(change, note, section))))
      started(projects, store)
      done(
        askBelieved(Integrity.Authority)(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "a",
          "kind" -> "note",
          "title" -> "A",
          "body" -> plan
        )
      )
      done(
        askBelieved(Integrity.Member)(
          projects,
          store,
          "project_split",
          "artifact" -> "p/1",
          "version" -> versionOf(projects, store, "p/1")
        )
      )
      (believedOf(store, "p/1"), believedOf(store, "p/2")) ==>
        (Some(Integrity.written(Integrity.Member)), Some(Integrity.written(Integrity.Member)))
    }
    test("each part is kept at the label of the artifact it was cut from, never below") {
      val projects = plugin()
      val store = Store(declared = Vector(method(kinds = Vector(change, note, section))))
      val above = askBy(direct(), at, Clearance.of(TestLabels.Trial))(projects, store, _, _*)
      done(above("project_new", Vector("project" -> "p")))
      done(
        above(
          "project_keep",
          Vector(
            "project" -> "p",
            "handle" -> "a",
            "kind" -> "note",
            "title" -> "A",
            "body" -> plan
          )
        )
      )
      val version = {
        given Tx = TestTx.fake(Clearance.of(TestLabels.Trial))
        store.keeper
          .current(DocKey.of("p/1").fold(sys.error, identity), TestLabels.Trial)
          .fold(e => sys.error(e.toString), _.fold(0L)(d => DocumentVersionId.value(d.version)))
      }
      def labelled(key: String, label: Label): Boolean = {
        given Tx = TestTx.fake(Clearance.of(TestLabels.Trial))
        store.keeper
          .current(DocKey.of(key).fold(sys.error, identity), label)
          .fold(e => sys.error(e.toString), _.isDefined)
      }
      done(above("project_split", Vector("artifact" -> "p/1", "version" -> version)))
      Vector("p/2", "p/3", "p/4").map(k =>
        (labelled(k, TestLabels.Trial), labelled(k, Label.Public))
      ) ==>
        Vector.fill(3)((true, false))
    }
  }
}
