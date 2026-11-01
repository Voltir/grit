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

  /** The store's count of artifacts, and the last lines `project_show` answers for `p/a` and
    * for its citers `p/b` and `p/d`.
    */
  private final case class Seen(count: Int, shown: Vector[String])

  private def seen(projects: Projects, store: Store): Seen =
    Seen(store.count, Vector("p/a", "p/b", "p/d").map(lastShown(projects, store, _, None)))

  /** Project `p`: `p/a` open, `p/b` under it and `p/c` under `p/b`, both closed; `p/d` closed,
    * citing `p/b` in its head and `p/c` and `p/a` in its body; and `p/e` open, citing none.
    */
  private def cascading(projects: Projects, store: Store): Unit = {
    started(projects, store)
    keptAt(projects, store, 1, "a", "One.")
    keptAt(projects, store, 1, "b", "Under a.", "parent" -> "p/a")
    keptAt(projects, store, 1, "c", "Under b.", "parent" -> "p/b")
    keptAt(
      projects,
      store,
      1,
      "d",
      "See [c](artifact:p/c) and [a](artifact:p/a).",
      "cites" -> ujson.Arr("p/b")
    )
    keptAt(projects, store, 1, "e", "Alone.")
    Vector("b", "c", "d").foreach(k =>
      done(ask(projects, store, "project_move", "key" -> s"p/$k", "state" -> "closed"))
    )
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
          CommandOutcome.Refused(
            "p/a is named by p/b, p/c: withdraw them first, or withdraw it with cascade"
          ),
          "One."
        )
      )
    }

    test(
      "with cascade, it and everything under it are withdrawn, and an ended artifact citing one is rewritten not to, by the withdrawer"
    ) {
      val projects = plugin()
      val store = Store()
      cascading(projects, store)
      val answer = askAs(other, at.plusSeconds(120))(
        projects,
        store,
        "project_withdraw",
        "key" -> "p/a",
        "cascade" -> true
      )
      val d = shown(projects, store, "p/d")
      (
        answer,
        d.linesIterator.filter(l => l.startsWith("cites:") || l.startsWith("by:")).toVector,
        d.linesIterator.toVector.lastOption,
        done(ask(projects, store, "project_list", "project" -> "p")).linesIterator
          .map(_.takeWhile(_ != ' '))
          .toVector,
        ask(projects, store, "project_export", "project" -> "p").getClass
      ) ==> (
        (
          CommandOutcome.Done(
            "Withdrawn: p/a, p/b, p/c\nCitations of them dropped from: p/d\nTheir history " +
              "stays readable for 180 days: project_show with an `at` before now, and " +
              "project_history."
          ),
          Vector("cites: []", s"by: \"$otherBy\""),
          Some("See c and a."),
          Vector("p/d", "p/e"),
          classOf[CommandOutcome.Done]
        )
      )
    }

    test(
      "a cascade is refused, withdrawing nothing, while an artifact under it has not ended, or one not ended cites one of them"
    ) {
      val projects = plugin()
      def cascade(store: Store) = withdrawnAt(3)(projects, store, "key" -> "p/a", "cascade" -> true)
      val open = Store()
      cascading(projects, open)
      keptAt(projects, open, 2, "f", "Under c.", "parent" -> "p/c")
      val citing = Store()
      cascading(projects, citing)
      keptAt(projects, citing, 2, "f", "Cites [b](artifact:p/b).")
      val before = (seen(projects, open), seen(projects, citing))
      (cascade(open), cascade(citing), (seen(projects, open), seen(projects, citing))) ==> (
        (
          CommandOutcome.Refused(
            "p/a has artifacts under it not ended: p/f: end or withdraw them first"
          ),
          CommandOutcome.Refused(
            "artifacts not ended cite what would be withdrawn: p/f: end them, or drop their " +
              "citations, first"
          ),
          before
        )
      )
      before._2 ==> Seen(
        7,
        Vector("One.", "Under a.", "See [c](artifact:p/c) and [a](artifact:p/a).")
      )
    }

    test(
      "a cascade is refused, withdrawing nothing, while an ended artifact it would rewrite is claimed"
    ) {
      val projects = plugin()
      val store = Store()
      cascading(projects, store)
      done(
        ask(
          projects,
          store,
          "project_keep",
          "key" -> "p/h",
          "kind" -> "change",
          "title" -> "Title h",
          "body" -> "## Why\n\nSee [b](artifact:p/b)."
        )
      )
      done(ask(projects, store, "project_move", "key" -> "p/h", "state" -> "building"))
      done(ask(projects, store, "project_move", "key" -> "p/h", "state" -> "done"))
      done(
        ask(projects, store, "project_claim", "key" -> "p/h", "role" -> "builder", "for" -> "slot")
      )
      val before = seen(projects, store)
      (
        withdrawnAt(3)(projects, store, "key" -> "p/a", "cascade" -> true),
        seen(projects, store),
        lastShown(projects, store, "p/h", None)
      ) ==> (
        (
          CommandOutcome.Refused("p/h is claimed for slot: release it first"),
          before,
          "See [b](artifact:p/b)."
        )
      )
      // Seven artifacts' documents, and p/h's brief, kept at its claim.
      before ==> Seen(8, Vector("One.", "Under a.", "See [c](artifact:p/c) and [a](artifact:p/a)."))
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
