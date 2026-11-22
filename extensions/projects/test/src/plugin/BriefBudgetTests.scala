package grit.projects.plugin

import grit.core.plugin.CommandOutcome
import grit.projects.method.{MethodFixtures, RoleView}

import utest.*

/** A brief's budget seen before work on an artifact is claimed: where a role view names the
  * states its work on a kind is claimed in, `project_show` and the answers of `project_keep` and
  * `project_edit` say when that role's brief of an artifact of the kind would pass 9,500
  * characters.
  */
object BriefBudgetTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.{change, kind, note, right, role, state}

  /** A builder claimed in changes that are building, and a reviewer of changes claimed in none. */
  private val claimedIn = method(roles =
    Vector(
      right(
        RoleView.of(
          role("builder"),
          Set(change.name, note.name),
          1,
          claimedIn = Map(change.name -> Set(state("building")))
        )
      ),
      right(RoleView.of(role("reviewer"), Set(change.name), 1))
    )
  )

  /** 66 characters of header come before a change's body while it is proposed, so `n` characters
    * of body give a brief of `n + 66`.
    */
  private def proposed(body: Int): (plugin: Projects, store: Store) = {
    val projects = plugin()
    val store = Store(declared = Vector(claimedIn))
    started(projects, store)
    kept(projects, store, "a", "change", "x" * (body - 1) + "y")
    (projects, store)
  }

  private def briefLines(text: String): Vector[String] =
    text.linesIterator.filter(_.startsWith("brief:")).toVector

  val tests = Tests {
    test(
      "project_show's head says the brief of a role claimed in its kind when it passes 9,500 characters"
    ) {
      val (projects, store) = proposed(9435)
      val (_, atLimit) = proposed(9434)
      (
        briefLines(shown(projects, store, "p/a")),
        briefLines(done(ask(projects, store, "project_show", "key" -> "p/a", "head" -> true))),
        briefLines(shown(projects, atLimit, "p/a"))
      ) ==> ((
        Vector("brief: builder 9,501 of 9,500"),
        Vector("brief: builder 9,501 of 9,500"),
        Vector()
      ))
    }
    test(
      "project_show says nothing of a brief over 9,500 characters of a kind no role is claimed in"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(claimedIn))
      started(projects, store)
      kept(projects, store, "a", "note", "x" * 12000)
      briefLines(shown(projects, store, "p/a")) ==> Vector()
    }
    test(
      "a keep that leaves a brief over 9,500 characters, of a kind a role is claimed in, says what to cut"
    ) {
      val (projects, store) = proposed(10)
      val answer = done(keeping(projects, store, "a", "change", "x" * 9436))
      answer ==> s"Kept p/1 a (change, proposed). Now at version ${versionOf(projects, store, "p/a")}. " +
        "Its brief as builder would be 9,502 characters, 2 over the 9,500 a brief holds: cut at " +
        "least 2 characters before it moves to building."
    }
    test(
      "an edit that leaves a brief over 9,500 characters says what to cut, and one within it says nothing"
    ) {
      val (projects, store) = proposed(9434)
      def edited(old: String, now: String) = {
        val version = versionOf(projects, store, "p/a")
        val answer = done(
          ask(
            projects,
            store,
            "project_edit",
            "key" -> "p/a",
            "version" -> version,
            "old" -> old,
            "new" -> now
          )
        )
        answer.stripPrefix(
          s"Kept p/1 a (change, proposed). Now at version ${versionOf(projects, store, "p/a")}."
        )
      }
      (edited("y", "yz"), edited("yz", "y")) ==> ((
        " Its brief as builder would be 9,501 characters, 1 over the 9,500 a brief holds: cut at " +
          "least 1 character before it moves to building.",
        ""
      ))
    }
  }
}
