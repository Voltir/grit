package grit.projects.method

import grit.projects.artifact.Artifact
import grit.projects.method.MethodFixtures.*
import grit.projects.name.StateName

import utest.*

object MigrationTests extends TestSuite {

  /** A task: drafted, then building, then done. */
  private val task = ArtifactKind(
    kind("task"),
    state("drafted"),
    Vector(state("building"), state("done")),
    Set(state("done")),
    Vector.empty
  )

  /** A backlog item: open, then taken. */
  private val backlog =
    ArtifactKind(
      kind("backlog"),
      state("open"),
      Vector(state("taken")),
      Set(state("taken")),
      Vector.empty
    )

  /** An issue: open, like a note, then closed. */
  private val issue =
    ArtifactKind(
      kind("issue"),
      state("open"),
      Vector(state("closed")),
      Set(state("closed")),
      Vector.empty
    )

  /** Version 2 of these kinds, each state reached from the kind's first. */
  private def version2(kinds: ArtifactKind*): Method =
    Method
      .of(
        method,
        2,
        kinds.toVector,
        kinds.toVector
          .flatMap(k => k.states.map(s => move(k, StateName.value(k.first), StateName.value(s)))),
        Vector.empty,
        TransitionChecks.all
      )
      .fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private val a = artifact(head("p/a", change, "proposed"), "## Why\n\nA.")
  private val b = artifact(head("p/b", change, "building"), "## Why\n\nB.")
  private val n = artifact(head("p/n", note, "open"), "N.")
  private val m = artifact(head("p/m", note, "open"), "M.")

  val tests = Tests {
    test(
      "a kind and a state mapping rename each artifact they name, and only those, in key order"
    ) {
      val to = version2(task, note)
      to.migrated(
        project(b, a, n),
        Remap(
          kinds = Map(change.name -> task.name),
          states = Map((change.name, state("proposed")) -> state("drafted"))
        )
      ) ==> Right(
        Migrated(
          pinnedTo(
            to,
            Artifact(b.head.copy(kind = task.name), b.body),
            Artifact(a.head.copy(kind = task.name, state = state("drafted")), a.body),
            n
          ),
          Vector(key("p/a"), key("p/b"))
        )
      )
    }

    test("an artifact's override gives it alone its kind, over the kind mapping") {
      val to = version2(issue, backlog)
      to.migrated(
        project(n, m),
        Remap(kinds = Map(note.name -> issue.name), artifacts = Map(key("p/n") -> backlog.name))
      ) ==> Right(
        Migrated(
          pinnedTo(
            to,
            Artifact(n.head.copy(kind = backlog.name), n.body),
            Artifact(m.head.copy(kind = issue.name), m.body)
          ),
          Vector(key("p/m"), key("p/n"))
        )
      )
    }

    test("kinds and states the target lacks, and overrides of no artifact, are refused at once") {
      val closed = artifact(head("p/c", note, "closed"), "C.")
      version2(task, note.copy(states = Vector.empty, ends = Set.empty)).migrated(
        project(a, b, closed),
        Remap(artifacts = Map(key("p/x") -> task.name))
      ) ==> Left(
        "project p has no artifact p/14; " +
          "p/1 a, p/2 b would be of kind change, which grit version 2 does not declare; " +
          "p/3 c would be in state closed, which kind note of grit version 2 does not declare"
      )
    }

    test(
      "a migration to the project's own version is refused once the method's digest has changed"
    ) {
      val was = version2(task, note)
      val now = version2(task, note, backlog)
      now.migrated(pinnedTo(was, n), Remap()) ==> Left(
        "project p holds to method grit version 2, whose rules have changed since the project " +
          "started; declare a new version of the method rather than changing this one"
      )
      was.migrated(pinnedTo(was, n), Remap()) ==> Right(Migrated(pinnedTo(was, n), Vector()))
    }

    test(
      "a migration to kinds whose nesting refuses a parent is refused, naming each out of place"
    ) {
      val rooted = change.copy(nesting = Nesting.Root)
      val inChange = note.copy(nesting = right(Nesting.under(Set(change.name))))
      val inside = artifact(head("p/n", note, "open", parent = Some("p/a")), "N.")
      val loose = artifact(head("p/m", note, "open"), "M.")
      val sunk = artifact(head("p/b", change, "building", parent = Some("p/n")), "## Why\n\nB.")
      version2(rooted, inChange).migrated(project(a, inside, loose, sunk), Remap()).map(_ => ()) ==>
        Left(
          "p/2 b, of kind change, cannot sit under p/12 n, of kind note: " +
            "it may sit only at the project's root; " +
            "p/11 m, of kind note, cannot sit at the project's root: it may sit under change"
        )
      version2(rooted, inChange)
        .migrated(project(a, inside), Remap())
        .map(_.project.artifacts.map(x => (x.head.called, x.head.parent.map(_.value)))) ==>
        Right(Vector(("p/1 a", None), ("p/12 n", Some("p/1"))))
    }
  }
}
