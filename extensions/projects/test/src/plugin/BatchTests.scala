package grit.projects.plugin

import grit.core.plugin.CommandOutcome
import grit.core.tool.{ArgsError, ToolName}

import utest.*

/** `project_batch`: keeps, edits and moves in order, all or none, and a dry run of them. */
object BatchTests extends TestSuite {
  import CommandFixtures.*

  private def keep(key: String, body: String, more: (String, ujson.Value)*): ujson.Obj =
    ujson.Obj.from(
      Vector(
        "command" -> ujson.Str("keep"),
        "key" -> ujson.Str(key),
        "title" -> ujson.Str("T"),
        "body" -> ujson.Str(body)
      ) ++ more
    )

  private def move(key: String, state: String): ujson.Obj =
    ujson.Obj("command" -> "move", "key" -> key, "state" -> state)

  /** An edit step of `key`, read at `version`, replacing `old` with `replaced`. */
  private def edit(key: String, version: Long, old: String, replaced: String): ujson.Obj =
    ujson.Obj(
      "command" -> "edit",
      "key" -> key,
      "version" -> version,
      "old" -> old,
      "new" -> replaced
    )

  /** Why `answer` refused, or what it was when it did not. */
  private def why(answer: CommandOutcome): String = answer match {
    case CommandOutcome.Refused(why) => why
    case other => other.toString
  }

  /** The version `project_history` lists first for `key`: its current one. */
  private def current(projects: Projects, store: Store, key: String): Long =
    done(ask(projects, store, "project_history", "key" -> key)).takeWhile(_ != ' ').toLong

  private def batch(projects: Projects, store: Store, steps: ujson.Obj*): CommandOutcome =
    ask(projects, store, "project_batch", "project" -> "p", "steps" -> ujson.Arr(steps*))

  private def dry(projects: Projects, store: Store, steps: ujson.Obj*): CommandOutcome =
    ask(
      projects,
      store,
      "project_batch",
      "project" -> "p",
      "steps" -> ujson.Arr(steps*),
      "dry" -> true
    )

  /** The versions `project_history` lists for `key`. */
  private def versions(projects: Projects, store: Store, key: String): Int =
    done(ask(projects, store, "project_history", "key" -> key)).linesIterator.size

  /** Why `args` do not read as `project_batch`'s arguments, if they do not. */
  private def unread(projects: Projects, args: ujson.Obj): Option[String] =
    projects.commands
      .find(c => ToolName.value(c.spec.name) == "project_batch")
      .flatMap(_.spec.args.read(args).left.toOption.map(_.message))

  val tests = Tests {
    test(
      "a batch keeps and moves in order, each step seeing the ones before, one version per artifact"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val answer = batch(
        projects,
        store,
        keep("p/a", "First.", "kind" -> "note"),
        move("p/a", "closed"),
        keep("p/b", "Under it.", "kind" -> "note", "parent" -> "p/a")
      )
      (
        answer,
        versions(projects, store, "p/a"),
        shown(projects, store, "p/a").contains("\nstate: closed\n"),
        shown(projects, store, "p/b").contains("\nparent: p/a\n")
      ) ==> (
        CommandOutcome.Done(
          "Every step is done:\n1. Kept p/a (note, open).\n2. Moved p/a (note, closed).\n" +
            "3. Kept p/b (note, open)."
        ),
        1,
        true,
        true
      )
    }

    test("a batch with a refused step keeps none of its steps, naming the step") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val before = store.count
      val answer = batch(
        projects,
        store,
        keep("p/a", "First.", "kind" -> "note"),
        move("p/a", "nowhere")
      )
      (answer, store.count) ==> (
        CommandOutcome.Refused(
          "Nothing was kept: step 2 of 2 (move p/a to nowhere) was refused: p/a (note, open) " +
            "cannot move to nowhere; it may move to closed"
        ),
        before
      )
    }

    test("a dry batch keeps nothing and answers what each step would, refused steps among them") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "First.")
      val refused =
        dry(projects, store, move("p/a", "closed"), keep("p/zz", "New."), keep("p/a", "Second."))
      val fine = dry(projects, store, move("p/a", "closed"))
      (
        refused,
        fine,
        versions(projects, store, "p/a"),
        shown(projects, store, "p/a").endsWith("\nFirst.\n")
      ) ==> (
        CommandOutcome.Refused(
          "Dry run, nothing kept; the batch would be refused. Each step would answer:\n" +
            "1. Moved p/a (note, closed).\n2. Refused: p/zz is new, so give its kind\n" +
            "3. Kept p/a (note, closed)."
        ),
        CommandOutcome.Done(
          "Dry run, nothing kept. Each step would answer:\n1. Moved p/a (note, closed)."
        ),
        1,
        true
      )
    }

    test("a batch checks a keep's version against the artifact before the batch") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "First.")
      val read = done(ask(projects, store, "project_history", "key" -> "p/a")).takeWhile(_ != ' ')
      batch(
        projects,
        store,
        move("p/a", "closed"),
        keep("p/a", "Second.", "version" -> read.toLong)
      ) ==>
        CommandOutcome.Done(
          "Every step is done:\n1. Moved p/a (note, closed).\n2. Kept p/a (note, closed)."
        )
    }

    test(
      "a batch refuses over 50 steps, a step outside its project, and a move given a keep's fields"
    ) {
      val projects = plugin()
      val many =
        ujson.Obj("project" -> "p", "steps" -> ujson.Arr(Vector.fill(51)(move("p/a", "closed"))*))
      val outside = ujson.Obj(
        "project" -> "p",
        "steps" -> ujson.Arr(move("p/a", "closed"), move("q/b", "closed"))
      )
      val mixed = ujson.Obj(
        "project" -> "p",
        "steps" -> ujson.Arr(
          ujson.Obj("command" -> "move", "key" -> "p/a", "state" -> "closed", "title" -> "T")
        )
      )
      (unread(projects, many), unread(projects, outside), unread(projects, mixed)) ==> (
        Some(ArgsError.Refused("steps", "At most 50 steps; 51 were given.").message),
        Some(ArgsError.Refused("steps", "Step 2's key q/b is in project q, not p.").message),
        Some(
          ArgsError
            .Refused("steps", "A move takes key and state alone; step 1 gives title.")
            .message
        )
      )
    }

    test(
      "a batch's edit changes the body the steps before it left, its version that of the artifact before the batch"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "First.")
      val read = current(projects, store, "p/a")
      val answer = batch(
        projects,
        store,
        keep("p/a", "First. Second.", "version" -> read),
        edit("p/a", read, "Second.", "Third.")
      )
      (
        answer,
        shown(projects, store, "p/a").contains("First. Third."),
        versions(projects, store, "p/a")
      ) ==>
        (
          CommandOutcome.Done(
            "Every step is done:\n1. Kept p/a (note, open).\n2. Kept p/a (note, open)."
          ),
          true,
          2
        )
    }

    test("a batch's edit refused as project_edit refuses it keeps nothing, naming the step") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "First.")
      val read = current(projects, store, "p/a")
      val alone = ask(
        projects,
        store,
        "project_edit",
        "key" -> "p/a",
        "version" -> read,
        "old" -> "Absent.",
        "new" -> "Here."
      )
      val inBatch =
        batch(projects, store, move("p/a", "closed"), edit("p/a", read, "Absent.", "Here."))
      (alone, inBatch, versions(projects, store, "p/a")) ==> (
        CommandOutcome.Refused(why(alone)),
        CommandOutcome.Refused(
          s"Nothing was kept: step 2 of 2 (edit p/a) was refused: ${why(alone)}"
        ),
        1
      )
    }

    test("a batch refuses an edit given a keep's or a move's fields, no version, or no change") {
      val projects = plugin()
      def steps(step: ujson.Obj) = ujson.Obj("project" -> "p", "steps" -> ujson.Arr(step))
      val titled = edit("p/a", 1, "x", "y")
      titled("title") = "T"
      titled("state") = "closed"
      val unversioned = edit("p/a", 1, "x", "y")
      unversioned.value.remove("version")
      val unchanged = ujson.Obj("command" -> "edit", "key" -> "p/a", "version" -> 1)
      (
        unread(projects, steps(titled)),
        unread(projects, steps(unversioned)),
        unread(
          projects,
          steps(unchanged)
        )
      ) ==> (
        Some(
          ArgsError
            .Refused(
              "steps",
              "An edit takes key, version, and old and new or section and body; step 1 gives " +
                "state, title."
            )
            .message
        ),
        Some(ArgsError.Refused("steps", "An edit needs its version; step 1 gives none.").message),
        Some(
          ArgsError
            .Refused(
              "steps",
              "Step 1: " + ArgsError.Missing("old", "old and new, or section and body").message
            )
            .message
        )
      )
    }
  }
}
