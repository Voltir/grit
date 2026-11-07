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
  }
}
