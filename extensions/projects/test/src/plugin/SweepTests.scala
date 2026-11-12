package grit.projects.plugin

import java.time.{Instant, LocalDate}

import grit.core.plugin.CommandOutcome
import grit.projects.method.MethodFixtures.{kind, right}
import grit.projects.name.ProjectKey

import utest.*

object SweepTests extends TestSuite {
  import CommandFixtures.*

  /** Before the day the sweeps below are asked to sweep before, 2026-10-28. */
  private val old: Instant = at
  private val recent: Instant = Instant.parse("2026-10-29T12:00:00Z")
  private val sweptAt: Instant = Instant.parse("2026-11-01T12:00:00Z")

  /** Keeps `p/<local>`, a note holding `body`, at `when`, with `more` arguments. */
  private def noted(
      projects: Projects,
      store: Store,
      when: Instant,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ): Unit =
    done(
      askAt(when)(
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

  /** Keeps `p/<local>` as [[noted]] does and closes it, both at `when`. */
  private def closed(
      projects: Projects,
      store: Store,
      when: Instant,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ): Unit = {
    noted(projects, store, when, local, body, more*)
    done(askAt(when)(projects, store, "project_move", "key" -> s"p/$local", "state" -> "closed"))
  }

  /** Claims `p/<local>` for `slot`, at [[sweptAt]]. */
  private def claimed(projects: Projects, store: Store, local: String): Unit =
    done(
      askAt(sweptAt)(
        projects,
        store,
        "project_claim",
        "key" -> s"p/$local",
        "role" -> "builder",
        "holder" -> "slot"
      )
    )

  /** `project_sweep` of notes before 2026-10-28, by [[other]] at [[sweptAt]]. */
  private def swept(projects: Projects, store: Store, more: (String, ujson.Value)*) =
    askAs(other, sweptAt)(
      projects,
      store,
      "project_sweep",
      (Vector[(String, ujson.Value)](
        "project" -> "p",
        "kinds" -> ujson.Arr("note"),
        "before" -> "2026-10-28"
      ) ++ more)*
    )

  /** Project `p`, of notes all closed but `p/live`, and all last changed before 2026-10-28 but
    * `p/c` and `p/g`: `p/a`, which `p/live` cites; `p/b`, with `p/c` under it; `p/d`, claimed;
    * `p/e`, citing `p/f`; `p/g`, citing `p/f`; and `p/h`, which nothing names.
    */
  private def sweeping(projects: Projects, store: Store): Unit = {
    started(projects, store)
    closed(projects, store, old, "a", "A.")
    noted(projects, store, old, "live", "See [a](artifact:p/a).")
    closed(projects, store, old, "b", "B.")
    closed(projects, store, recent, "c", "Under b.", "parent" -> "p/b")
    closed(projects, store, old, "d", "D.")
    closed(projects, store, old, "f", "F.")
    closed(projects, store, old, "e", "See [f](artifact:p/f).")
    closed(projects, store, recent, "g", "After [f](artifact:p/f).")
    closed(projects, store, old, "h", "H.")
    claimed(projects, store, "d")
  }

  private val lines: String =
    "p/a (note, closed): Title a · stays: cited by p/live (note, open)\n" +
      "p/b (note, closed): Title b · stays: p/c (note, closed) under it stays\n" +
      "p/d (note, closed): Title d · stays: claimed for slot\n"

  val tests: Tests = Tests {

    test(
      "a sweep withdraws the old ended artifacts nothing live holds, answering what went, what lost citations and what stayed, and leaves a project that exports"
    ) {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      (
        swept(projects, store),
        done(ask(projects, store, "project_list", "project" -> "p", "ended" -> true)).linesIterator
          .drop(1)
          .map(_.takeWhile(_ != ' '))
          .toVector
          .sorted,
        ask(projects, store, "project_export", "project" -> "p").getClass
      ) ==> (
        (
          CommandOutcome.Done(
            "Withdrew 3: note 3 (closed 3).\n" +
              "Citations of them dropped from 1.\n" +
              "3 old enough stayed: claimed 1; has one under it that stays 1; still cited 1.\n" +
              lines +
              "p/g (note, closed): Title g · rewritten without its citations of what went\n" +
              "p/e (note, closed): Title e · withdrawn\n" +
              "p/f (note, closed): Title f · withdrawn\n" +
              "p/h (note, closed): Title h · withdrawn\n" +
              "Their history stays readable for 180 days: project_show with an `at` before now, " +
              "and project_history."
          ),
          Vector("p/a", "p/b", "p/c", "p/d", "p/g", "p/live"),
          classOf[CommandOutcome.Done]
        )
      )
    }

    test("with dry, nothing is written, and it answers what would go and what would stay") {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      val before = store.count
      (swept(projects, store, "dry" -> true), store.count) ==> (
        (
          CommandOutcome.Done(
            "Dry run: nothing withdrawn or changed.\n" +
              "Would withdraw 3: note 3 (closed 3).\n" +
              "Would drop citations of them from 1.\n" +
              "3 old enough stayed: claimed 1; has one under it that stays 1; still cited 1.\n" +
              lines +
              "p/g (note, closed): Title g · would be rewritten without its citations of what goes\n" +
              "p/e (note, closed): Title e · would be withdrawn\n" +
              "p/f (note, closed): Title f · would be withdrawn\n" +
              "p/h (note, closed): Title h · would be withdrawn"
          ),
          before
        )
      )
    }

    test("an artifact rewritten without its citations is written by the sweeper, changed today") {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      done(swept(projects, store))
      val g = shown(projects, store, "p/g")
      (
        g.linesIterator.filter(_.startsWith("by:")).toVector,
        g.linesIterator.toVector.lastOption,
        done(ask(projects, store, "project_list", "project" -> "p", "ended" -> true)).linesIterator
          .filter(_.startsWith("p/g "))
          .map(_.split(" · ").toVector.filter(_.startsWith("changed")))
          .toVector
      ) ==> (
        (
          Vector(s"by: \"$otherBy\""),
          Some("After f."),
          Vector(Vector("changed 2026-11-01"))
        )
      )
    }

    test("one last changed before the day began goes; one changed as it began is not asked for") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      closed(projects, store, Instant.parse("2026-10-27T23:59:59Z"), "x", "X.")
      closed(projects, store, Instant.parse("2026-10-28T00:00:00Z"), "y", "Y.")
      done(swept(projects, store)) ==>
        "Withdrew 1: note 1 (closed 1).\n" +
        "p/x (note, closed): Title x · withdrawn\n" +
        "Their history stays readable for 180 days: project_show with an `at` before now, and " +
        "project_history."
    }

    test("a claimed ended artifact citing an old one is not rewritten, and what it cites stays") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      closed(projects, store, old, "r", "R.")
      closed(projects, store, recent, "q", "See [r](artifact:p/r).")
      claimed(projects, store, "q")
      (
        done(swept(projects, store)),
        shown(projects, store, "p/q").linesIterator.toVector.lastOption
      ) ==> (
        (
          "Withdrew none.\n" +
            "1 old enough stayed: still cited 1.\n" +
            "p/r (note, closed): Title r · stays: cited by p/q (note, closed)",
          Some("See [r](artifact:p/r).")
        )
      )
    }

    test("an old ended artifact stays above one of a kind not swept, naming its kind") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      closed(projects, store, old, "n", "N.")
      done(
        askAt(old)(
          projects,
          store,
          "project_keep",
          "key" -> "p/k",
          "kind" -> "change",
          "title" -> "Title k",
          "body" -> "## Why\n\nBecause.",
          "parent" -> "p/n"
        )
      )
      done(askAt(old)(projects, store, "project_move", "key" -> "p/k", "state" -> "building"))
      done(askAt(old)(projects, store, "project_move", "key" -> "p/k", "state" -> "done"))
      done(swept(projects, store)) ==>
        "Withdrew none.\n" +
        "1 old enough stayed: has one under it that stays 1.\n" +
        "p/n (note, closed): Title n · stays: p/k (change, done) under it stays"
    }

    test(
      "past 40 lines the answer is cut, saying how many more, and when dry the skip that shows the next"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      (1 to 42).foreach(i => closed(projects, store, old, f"n$i%02d", "N."))
      val dry = done(swept(projects, store, "dry" -> true)).linesIterator.toVector
      val one = done(swept(projects, store, "dry" -> true, "show" -> "withdrawn")).linesIterator
      val real = done(swept(projects, store)).linesIterator.toVector
      (dry.size, dry.lastOption, one.toVector.lastOption, real.size, real.takeRight(2)) ==> (
        (
          43,
          Some(
            "2 more not shown; skip: 40 shows the next, or show one of stayed, rewritten, " +
              "withdrawn."
          ),
          Some("2 more not shown; skip: 40 shows the next."),
          43,
          Vector(
            "2 more not shown.",
            "Their history stays readable for 180 days: project_show with an `at` before now, " +
              "and project_history."
          )
        )
      )
    }

    test("with show, only that group's lines print, under counts of everything") {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      done(swept(projects, store, "dry" -> true, "show" -> "rewritten")) ==>
        "Dry run: nothing withdrawn or changed.\n" +
        "Would withdraw 3: note 3 (closed 3).\n" +
        "Would drop citations of them from 1.\n" +
        "3 old enough stayed: claimed 1; has one under it that stays 1; still cited 1.\n" +
        "p/g (note, closed): Title g · would be rewritten without its citations of what goes"
    }

    test(
      "skip passes over that many lines of the group shown, and at or past its end says no line is past it"
    ) {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      def counts(lines: String*): String =
        (Vector(
          "Dry run: nothing withdrawn or changed.",
          "Would withdraw 3: note 3 (closed 3).",
          "Would drop citations of them from 1.",
          "3 old enough stayed: claimed 1; has one under it that stays 1; still cited 1."
        ) ++ lines).mkString("\n")
      (
        done(swept(projects, store, "dry" -> true, "show" -> "withdrawn", "skip" -> 1)),
        done(swept(projects, store, "dry" -> true, "show" -> "stayed", "skip" -> 3))
      ) ==> (
        (
          counts(
            "p/f (note, closed): Title f · would be withdrawn",
            "p/h (note, closed): Title h · would be withdrawn"
          ),
          counts("No line past 3.")
        )
      )
    }

    test("a show none of the three, or a skip under 0, is refused") {
      val projects = plugin()
      val args = Vector[(String, ujson.Value)](
        "project" -> "p",
        "kinds" -> ujson.Arr("note"),
        "before" -> "2026-10-28"
      )
      (
        refusedArgs(projects, "project_sweep", (args :+ ("show" -> ujson.Str("kept")))*),
        refusedArgs(projects, "project_sweep", (args :+ ("skip" -> ujson.Num(-1)))*),
        ProjectCommands.Sweeping
          .of(
            right(ProjectKey.of("p")),
            Set(kind("note")),
            LocalDate.parse("2026-10-28"),
            true,
            None,
            -1
          )
      ) ==> (
        (
          Some("`show` takes one of `stayed`, `rewritten`, `withdrawn`, not \"kept\"."),
          Some("`skip` takes a whole number from 0 to 20000, not -1."),
          Left("skip is -1, under 0")
        )
      )
    }

    test("a day later than today, or a kind the method lacks, is refused, changing nothing") {
      val projects = plugin()
      val store = Store()
      sweeping(projects, store)
      val before = store.count
      (
        swept(projects, store, "before" -> "2026-11-02"),
        swept(projects, store, "kinds" -> ujson.Arr("note", "notes")),
        store.count
      ) ==> (
        (
          CommandOutcome.Refused("before is 2026-11-02, later than today, 2026-11-01 (UTC)"),
          CommandOutcome.Refused(
            "notes: not among the kinds of grit version 1, which are change, note"
          ),
          before
        )
      )
    }
  }
}
