package grit.projects.plugin

import grit.core.plugin.CommandOutcome

import utest.*

/** A version as `project_show` gives it and `project_keep` takes it back. */
object KeepVersionTests extends TestSuite {
  import CommandFixtures.*

  /** The versions of `key`, newest first, as `project_history` numbers them. */
  private def versions(projects: Projects, store: Store, key: String): Vector[String] =
    done(ask(projects, store, "project_history", "key" -> key)).linesIterator.toVector
      .map(_.takeWhile(_ != ' '))

  /** `text`'s front matter line `version: <n>`, if it has one. */
  private def versionLine(text: String): Option[String] =
    text.linesIterator.find(_.startsWith("version: "))

  val tests = Tests {
    test(
      "project_show gives each artifact's version as project_history numbers it, at an instant that version's"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      done(
        askAt(at.plusSeconds(60))(
          projects,
          store,
          "project_keep",
          "key" -> "p/a",
          "kind" -> "note",
          "title" -> "A",
          "body" -> "One."
        )
      )
      done(
        askAt(at.plusSeconds(180))(
          projects,
          store,
          "project_keep",
          "key" -> "p/a",
          "title" -> "A",
          "body" -> "Two."
        )
      )
      val (newer, older) = versions(projects, store, "p/a") match {
        case Vector(n, o) => (n, o)
        case other => throw new java.lang.AssertionError(s"versions: $other")
      }
      val now = shown(projects, store, "p/a")
      val head = done(ask(projects, store, "project_show", "key" -> "p/a", "head" -> true))
      val earlier = done(
        ask(projects, store, "project_show", "key" -> "p/a", "at" -> at.plusSeconds(120).toString)
      )
      (
        versionLine(now),
        versionLine(head),
        versionLine(earlier),
        now.contains(s"version: $newer\n---\n")
      ) ==> (
        Some(s"version: $newer"),
        Some(s"version: $newer"),
        Some(s"version: $older"),
        true
      )
    }

    test(
      "a keep given the version it read is kept; given one since kept over, it is refused naming the version current, keeping nothing"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "One.")
      val read = versions(projects, store, "p/a").headOption.getOrElse("")
      val keep = (body: String) =>
        askAt(at.plusSeconds(60))(
          projects,
          store,
          "project_keep",
          "key" -> "p/a",
          "title" -> "A",
          "body" -> body,
          "version" -> read.toLong
        )
      val first = keep("Two.")
      val current = versions(projects, store, "p/a").headOption.getOrElse("")
      val by = shown(projects, store, "p/a").linesIterator
        .find(_.startsWith("by: \""))
        .fold("")(_.drop(5).dropRight(1))
      (first, keep("Three."), shown(projects, store, "p/a").endsWith("\nTwo.\n")) ==> (
        CommandOutcome.Done("Kept p/a (note, open)."),
        CommandOutcome.Refused(
          s"p/a is at version $current, not $read as given: kept since by $by at " +
            s"${at.plusSeconds(60)}. Show it again and keep from what it holds now."
        ),
        true
      )
    }

    test("a keep given a version for a key the project lacks is refused") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "plan", "note", "One.")
      ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/plans",
        "kind" -> "note",
        "title" -> "A",
        "body" -> "Two.",
        "version" -> 7
      ) ==> CommandOutcome.Refused(
        "p/plans is new, so it has no version to give; leave out version (nearest: p/plan)"
      )
    }

    test("a keep given a project not its key's is refused, its own accepted") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val keep = (project: String) =>
        projects.commands
          .find(c => grit.core.tool.ToolName.value(c.spec.name) == "project_keep")
          .map(
            _.spec.args
              .read(
                ujson.Obj(
                  "project" -> project,
                  "key" -> "p/a",
                  "kind" -> "note",
                  "title" -> "A",
                  "body" -> "One."
                )
              )
              .left
              .map(_.message)
              .map(_ => "read")
          )
      (keep("q"), keep("p")) ==> (
        Some(
          Left(
            grit.core.tool.ArgsError
              .Refused("project", "The key p/a is in project p, not q.")
              .message
          )
        ),
        Some(Right("read"))
      )
    }
  }
}
