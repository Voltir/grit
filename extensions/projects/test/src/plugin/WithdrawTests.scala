package grit.projects.plugin

import grit.core.plugin.CommandOutcome

import utest.*

object WithdrawTests extends TestSuite {

  import CommandFixtures.*

  /** Keeps `p/<local>`, a note holding `body`, `m` minutes after [[at]], with `more` arguments. */
  private def keptAt(
      projects: Projects,
      store: Store,
      m: Int,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ): Unit =
    done(
      askAt(at.plusSeconds(60L * m))(
        projects,
        store,
        "project_keep",
        (Vector[(String, ujson.Value)](
          "key" -> s"p/$local",
          "kind" -> "note",
          "title" -> s"Title $local",
          "body" -> body
        ) ++ more)*
      )
    )

  private def withdrawnAt(m: Int)(projects: Projects, store: Store, args: (String, ujson.Value)*) =
    askAt(at.plusSeconds(60L * m))(projects, store, "project_withdraw", args*)

  /** The last line of what `project_show` answers for `key`, at `m` minutes when given. */
  private def lastShown(projects: Projects, store: Store, key: String, m: Option[Int]): String =
    ask(
      projects,
      store,
      "project_show",
      (Vector[(String, ujson.Value)]("key" -> key) ++
        m.map(n => "at" -> ujson.Str(at.plusSeconds(60L * n).toString)))*
    ) match {
      case CommandOutcome.Done(text) => text.linesIterator.toVector.lastOption.getOrElse("")
      case CommandOutcome.Refused(why) => s"refused: $why"
    }

  val tests: Tests = Tests {

    test(
      "a withdrawn artifact leaves the project; project_show at an instant before still shows it, and project_history lists its withdrawal"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "a", "One.")
      val answer = done(withdrawnAt(2)(projects, store, "key" -> "p/a"))
      val history = ask(projects, store, "project_history", "key" -> "p/a") match {
        case CommandOutcome.Done(text) =>
          text.linesIterator.map(_.split(" ", 2).toVector.drop(1).mkString).toVector
        case CommandOutcome.Refused(why) => Vector(s"refused: $why")
      }
      (
        answer,
        lastShown(projects, store, "p/a", None),
        lastShown(projects, store, "p/a", Some(1)),
        history,
        done(ask(projects, store, "project_list", "project" -> "p"))
      ) ==> (
        (
          "Withdrawn: p/a\nTheir history stays readable for 180 days: project_show with an `at` " +
            "before now, and project_history.",
          "refused: project p has no artifact p/a",
          "One.",
          Vector(
            "2026-10-27T12:02:00Z withdrawn",
            s"2026-10-27T12:01:00Z by $by (open): Title a"
          ),
          "No artifact matches."
        )
      )
    }

    test(
      "a withdrawal is refused, withdrawing nothing, while another artifact has it as parent or cites it, naming each"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "a", "One.")
      keptAt(projects, store, 1, "b", "Under a.", "parent" -> "p/a")
      keptAt(projects, store, 1, "c", "See [a](artifact:p/a).")
      (
        withdrawnAt(2)(projects, store, "key" -> "p/a"),
        lastShown(projects, store, "p/a", None)
      ) ==> (
        (
          CommandOutcome.Refused("p/a is named by p/b, p/c: withdraw them first"),
          "One."
        )
      )
    }

    test("a claimed artifact's withdrawal is refused, withdrawing nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nOne.")
      done(
        ask(projects, store, "project_claim", "key" -> "p/a", "role" -> "builder", "for" -> "slot")
      )
      (
        withdrawnAt(2)(projects, store, "key" -> "p/a"),
        lastShown(projects, store, "p/a", None)
      ) ==> ((CommandOutcome.Refused("p/a is claimed for slot: release it first"), "One."))
    }
  }
}
