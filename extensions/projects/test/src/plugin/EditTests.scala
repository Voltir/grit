package grit.projects.plugin

import grit.core.plugin.CommandOutcome

import utest.*

/** A section of an artifact read by `project_show`, and a part of its body changed by
  * `project_edit`, kept as `project_keep` keeps it.
  */
object EditTests extends TestSuite {

  import CommandFixtures.*

  private val sectioned = "Lead.\n\n## Goal\n\nOne.\n\n## Reads\n\nTwo."

  /** Project `p` holding `p/a`, a note of `body`. */
  private def holding(body: String = sectioned): (Projects, Store) = {
    val projects = plugin()
    val store = Store()
    started(projects, store)
    kept(projects, store, "a", "note", body)
    (projects, store)
  }

  private def edit(projects: Projects, store: Store, args: (String, ujson.Value)*) =
    ask(projects, store, "project_edit", (("key" -> ujson.Str("p/a")) +: args)*)

  /** `p/a`'s text as `project_show` gives it, after its front matter. */
  private def body(projects: Projects, store: Store): String =
    shown(projects, store, "p/a").split("\n---\n", 2).lift(1).getOrElse("")

  val tests = Tests {
    test("project_show with a section gives the front matter and that section alone") {
      val (projects, store) = holding()
      val whole = shown(projects, store, "p/a")
      val front = whole.split("\n---\n", 2).headOption.getOrElse("") + "\n---\n"
      (
        ask(projects, store, "project_show", "key" -> "p/a", "section" -> "Reads"),
        ask(projects, store, "project_show", "key" -> "p/a", "section" -> "Why")
      ) ==> (
        CommandOutcome.Done(front + "\n## Reads\n\nTwo.\n"),
        CommandOutcome.Refused(
          "p/a: it has no section titled \"Why\"; its sections: Goal; Reads"
        )
      )
    }

    test("project_show takes a section or head, not both") {
      refusedArgs(plugin(), "project_show", "key" -> "p/a", "head" -> true, "section" -> "Goal")
        .exists(_.contains("Give head or section, not both")) ==> true
    }

    test("an edit replaces an exact text held once, answering the version it is kept at") {
      val (projects, store) = holding()
      val read = versionOf(projects, store, "p/a")
      val answer =
        edit(projects, store, "version" -> read, "old" -> "One.", "new" -> "Uno.")
      val now = versionOf(projects, store, "p/a")
      (answer, body(projects, store), now > read) ==> (
        CommandOutcome.Done(s"Kept p/a (note, open). Now at version $now."),
        "\nLead.\n\n## Goal\n\nUno.\n\n## Reads\n\nTwo.\n",
        true
      )
    }

    test("an edit whose old text is held twice or not at all is refused, keeping nothing") {
      val (projects, store) = holding("One. One.")
      val read = versionOf(projects, store, "p/a")
      (
        edit(projects, store, "version" -> read, "old" -> "One.", "new" -> "Two."),
        edit(projects, store, "version" -> read, "old" -> "Three.", "new" -> "Two."),
        versionOf(projects, store, "p/a")
      ) ==> (
        CommandOutcome.Refused(
          "p/a cannot be edited: it holds the old text 2 times; give more of the text around " +
            "it, so it is held once"
        ),
        CommandOutcome.Refused(
          "p/a cannot be edited: it does not hold the old text; show it again and copy the " +
            "text exactly"
        ),
        read
      )
    }

    test("an edit of a section replaces what follows its heading, the heading kept") {
      val (projects, store) = holding()
      val read = versionOf(projects, store, "p/a")
      done(edit(projects, store, "version" -> read, "section" -> "Goal", "body" -> "- A\n- B"))
      body(projects, store) ==> "\nLead.\n\n## Goal\n\n- A\n- B\n\n## Reads\n\nTwo.\n"
    }

    test(
      "an edit at a version kept over since is refused naming the current one, keeping nothing"
    ) {
      val (projects, store) = holding()
      val read = versionOf(projects, store, "p/a")
      done(edit(projects, store, "version" -> read, "old" -> "One.", "new" -> "Uno."))
      val current = versionOf(projects, store, "p/a")
      val refused =
        edit(projects, store, "version" -> read, "section" -> "Goal", "body" -> "Eins.")
      (
        refused match {
          case CommandOutcome.Refused(why) =>
            why.startsWith(s"p/a is at version $current, not $read as given: kept since")
          case _ => false
        },
        versionOf(projects, store, "p/a")
      ) ==> (true, current)
    }

    test("an edit of a heading several sections share is refused, naming which they are") {
      val (projects, store) = holding("## Reads\n\nA.\n\n## Goal\n\nB.\n\n## Reads\n\nC.")
      val read = versionOf(projects, store, "p/a")
      edit(projects, store, "version" -> read, "section" -> "Reads", "body" -> "D.") ==>
        CommandOutcome.Refused(
          "p/a cannot be edited: it has 2 sections titled \"Reads\": its sections 1 and 3 of 3"
        )
    }

    test("an edit that takes a body past its stored limit is refused as a keep of it is") {
      val (projects, store) = holding()
      val read = versionOf(projects, store, "p/a")
      val long = Vector.fill(3000)("ten chars.").mkString(" ")
      val asKept = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/a",
        "title" -> "Title a",
        "body" -> s"Lead.\n\n## Goal\n\n$long\n\n## Reads\n\nTwo."
      )
      val edited = edit(projects, store, "version" -> read, "section" -> "Goal", "body" -> long)
      (asKept.isInstanceOf[CommandOutcome.Refused], edited, versionOf(projects, store, "p/a")) ==>
        (true, asKept, read)
    }

    test("an edit takes old and new, or section and body, and not both") {
      val projects = plugin()
      val refused = (args: Seq[(String, ujson.Value)]) =>
        refusedArgs(
          projects,
          "project_edit",
          (Seq("key" -> ujson.Str("p/a"), "version" -> ujson.Num(1)) ++ args)*
        )
      (
        refused(Seq("old" -> "a", "new" -> "b")),
        refused(Seq("section" -> "Goal", "body" -> "b")),
        refused(Seq("old" -> "a", "new" -> "b", "section" -> "Goal", "body" -> "b")).isDefined,
        refused(Seq("old" -> "a")).isDefined,
        refused(Seq()).isDefined
      ) ==> (None, None, true, true, true)
    }
  }
}
