package grit.projects.plugin

import grit.core.plugin.CommandOutcome
import grit.projects.method.{MethodFixtures, Nesting}

import utest.*

object NestingTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{change, note}

  /** A store whose method's changes sit only at the root, and its notes anywhere. */
  private def rooted(): Store =
    Store(declared = Vector(method(kinds = Vector(change.copy(nesting = Nesting.Root), note))))

  private def keep(projects: Projects, store: Store, args: (String, ujson.Value)*): CommandOutcome =
    ask(projects, store, "project_keep", args*)

  val tests = Tests {
    test(
      "a keep placing an artifact where its kind's nesting refuses is refused, and one it allows is kept"
    ) {
      val projects = plugin()
      val store = rooted()
      started(projects, store)
      kept(projects, store, "n", "note", "A note.")
      kept(projects, store, "c", "change", "## Why\n\nC.")
      (
        keep(
          projects,
          store,
          "project" -> "p",
          "handle" -> "d",
          "kind" -> "change",
          "title" -> "D",
          "body" -> "## Why\n\nD.",
          "parent" -> "p/n"
        ),
        keep(
          projects,
          store,
          "key" -> "p/c",
          "title" -> "C",
          "body" -> "## Why\n\nC.",
          "parent" -> "p/n"
        ),
        keep(
          projects,
          store,
          "project" -> "p",
          "handle" -> "m",
          "kind" -> "note",
          "title" -> "M",
          "body" -> "Under c.",
          "parent" -> "p/c"
        )
      ) ==> (
        CommandOutcome.Refused(
          "p/3 d, of kind change, cannot sit under p/1 n, of kind note: " +
            "it may sit only at the project's root"
        ),
        CommandOutcome.Refused(
          "p/2 c, of kind change, cannot sit under p/1 n, of kind note: " +
            "it may sit only at the project's root"
        ),
        CommandOutcome.Done("Kept p/3 m (note, open). Now at version 7.")
      )
    }
    test("a keep leaving in place an artifact already out of place is kept") {
      val projects = plugin()
      val store = rooted()
      started(projects, store)
      kept(projects, store, "n", "note", "A note.")
      kept(projects, store, "c", "change", "## Why\n\nC.")
      rewritten(store, "p/2", _.copy(parent = Some(MethodFixtures.key("p/1"))))
      keep(projects, store, "key" -> "p/c", "title" -> "C", "body" -> "## Why\n\nC again.") ==>
        CommandOutcome.Done("Kept p/2 c (change, proposed). Now at version 8.")
    }
  }
}
