package grit.projects.plugin

import grit.core.plugin.CommandOutcome
import grit.projects.method.{MethodFixtures, RoleView}
import grit.projects.name.Handle

import utest.*

/** A brief's budget seen before work on an artifact is claimed: where a role view names the
  * states its work on a kind is claimed in, `project_show` and the answers of `project_keep` and
  * `project_edit` say when that role's brief of an artifact of the kind would pass 9,500
  * characters.
  */
object BriefBudgetTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.{change, note, right, role, state}

  /** A builder, claimed in changes that are building when `claimed`, and a reviewer of changes
    * claimed in none.
    */
  private def roles(claimed: Boolean): Vector[RoleView] = Vector(
    right(
      RoleView.of(
        role("builder"),
        Set(change.name, note.name),
        1,
        claimedIn =
          if (claimed) Map(change.name -> Set(state("building"))) else Map.empty
      )
    ),
    right(RoleView.of(role("reviewer"), Set(change.name), 1))
  )

  private val claimedIn = method(roles = roles(claimed = true))

  /** `p/a`, a change, building at a brief of 9,501 characters under a version that claims no
    * role in it, then moved to version 2, where the builder is claimed in it.
    */
  private def buildingOver(): (plugin: Projects, store: Store) = {
    val projects = plugin()
    val store = Store(declared = Vector(method(roles = roles(claimed = false))))
    started(projects, store)
    kept(projects, store, "a", "change", "x" * 9435)
    done(moved(projects, store, "building"))
    done(
      ask(
        projects,
        store,
        "project_declare",
        declaring(method(version = 2, roles = roles(claimed = true)))
      )
    )
    done(ask(projects, store, "project_migrate", "project" -> "p", "version" -> 2))
    (projects, store)
  }

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
    test(
      "a move into a state a role is claimed in is refused when its brief would pass 9,500 characters, naming what to cut"
    ) {
      val (projects, store) = proposed(9435)
      val (within, atLimit) = proposed(9434)
      (
        moved(projects, store, "building"),
        moved(within, atLimit, "building").isInstanceOf[CommandOutcome.Done]
      ) ==> ((CommandOutcome.Refused(overAsBuilder(9501, "p/1")), true))
    }
    test(
      "a keep that grows a brief past 9,500 characters in a state its role is claimed in is refused, keeping nothing"
    ) {
      val (projects, store) = proposed(10)
      done(moved(projects, store, "building"))
      val version = versionOf(projects, store, "p/a")
      (
        keeping(projects, store, "a", "change", "x" * 9435),
        versionOf(projects, store, "p/a")
      ) ==> ((CommandOutcome.Refused(overAsBuilder(9501, "p/1")), version))
    }
    test(
      "a keep of what a claimed-in role carries is refused when it puts a brief in that state past 9,500 characters"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(carrying))
      started(projects, store)
      kept(projects, store, "a", "change", "x" * 9000)
      kept(projects, store, "rules", "note", "Rules.")
      done(moved(projects, store, "building"))
      keeping(projects, store, "rules", "note", "x" * 1000) ==>
        CommandOutcome.Refused(
          // 9,066 of the change while building, 14 of `## Carried`, 29 of the note's heading line
          // and 1,000 of its body.
          "the brief of p/1 as builder would be 10,109 characters, 609 over the 9,500 a brief " +
            "holds (the artifact with what its role always carries): cut at least 609 " +
            "characters from p/2"
        )
    }
    test("a keep a live claim's brief refuses is refused once, for the claim") {
      val (projects, store) = proposed(10)
      done(moved(projects, store, "building"))
      done(
        ask(
          projects,
          store,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> holder
        )
      )
      keeping(projects, store, "a", "change", "x" * 9435) ==> CommandOutcome.Refused(
        s"the brief of p/1, claimed as builder for $holder, would be 9,501 characters, 1 over " +
          "the 9,500 a brief holds (the artifact with what its role always carries): cut at " +
          "least 1 character from p/1"
      )
    }
    test(
      "a claim in a state its role is claimed in is refused when its brief would pass 9,500 characters, claiming nothing"
    ) {
      val (projects, store) = buildingOver()
      (
        claimed(projects, store, "builder"),
        done(ask(projects, store, "project_claims")).contains("p/1")
      ) ==> ((CommandOutcome.Refused(overAsBuilder(9501, "p/1") + ". Nothing was claimed."), false))
    }
    test(
      "a claim over its brief's budget in a role claimed in no such state stands, with no brief"
    ) {
      val (projects, store) = buildingOver()
      val answer = ujson.read(done(claimed(projects, store, "reviewer")))
      answer("brief").str ==> "none"
    }
    test(
      "a takeover into a role claimed in the artifact's state is refused when its brief would pass 9,500 characters, moving nothing"
    ) {
      val (projects, store) = buildingOver()
      done(claimed(projects, store, "reviewer"))
      (
        ask(
          projects,
          store,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> holder,
          "takeover" -> "claude-code:s-b"
        ),
        ujson
          .read(done(ask(projects, store, "project_claims")))("claims")
          .arr
          .map(c => c("role").str)
          .toVector
      ) ==> ((
        CommandOutcome.Refused(overAsBuilder(9501, "p/1") + ". Nothing was claimed."),
        Vector("reviewer")
      ))
    }
  }

  private val holder: String = "coder-opus/abc"

  private def claimed(projects: Projects, store: Store, as: String): CommandOutcome =
    ask(projects, store, "project_claim", "key" -> "p/a", "role" -> as, "holder" -> holder)

  private def moved(projects: Projects, store: Store, to: String): CommandOutcome =
    ask(projects, store, "project_move", "key" -> "p/a", "state" -> to)

  /** The refusal of a brief of `size` characters for the builder, cut from `from`. */
  private def overAsBuilder(size: Int, from: String): String = {
    val cut = size - 9500
    f"the brief of p/1 as builder would be $size%,d characters, $cut%,d over the 9,500 a brief " +
      "holds (the artifact with what its role always carries): cut at least " +
      f"$cut%,d character${if (cut == 1) "" else "s"} from $from"
  }

  /** [[claimedIn]]'s builder, carrying the note handled `rules`. */
  private val carrying = method(roles =
    Vector(
      right(
        RoleView.of(
          role("builder"),
          Set(change.name, note.name),
          1,
          carried = Set(right(Handle.of("rules"))),
          claimedIn = Map(change.name -> Set(state("building")))
        )
      )
    )
  )
}
