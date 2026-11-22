package grit.projects.plugin

import grit.core.claim.Claim
import grit.core.id.AgentType
import grit.core.plugin.CommandOutcome
import grit.projects.method.{Holders, MethodFixtures, RoleView}

import utest.*

/** `project_claim` for a holder its role does not admit ([[Holders]]): refused before anything
  * is claimed or moved, whether by key, by `oldest` or by `takeover`, naming the roles that do.
  */
object AdmittedClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{note, right, role}

  private val Session = "local/grit-0a1b2c3d"
  private val Coder = s"$Session/coder-opus/0123456789ab"

  private val coders = Holders.Agents
    .of(Set("coder-opus", "coder-haiku").map(t => right(AgentType.of(t))))
    .getOrElse(throw new java.lang.AssertionError("two types are some"))

  private def view(name: String, holders: Holders): RoleView =
    right(RoleView.of(role(name), Set(note.name), 1, holders = holders))

  /** A store of [[method]] under these roles, its project `p` holding two notes, `p/a`, `p/b`. */
  private def holding(roles: RoleView*): (Projects, Store) = {
    val projects = plugin()
    val store = Store(declared = Vector(method(roles = roles.toVector)))
    started(projects, store)
    kept(projects, store, "a", "note", "Text.")
    kept(projects, store, "b", "note", "Text.")
    (projects, store)
  }

  /** The four roles of the method these refusals name: coder for coding agents, manager for a
    * session, and grit-coder and reviewer for anyone.
    */
  private val Roles: Vector[RoleView] = Vector(
    view("coder", coders),
    view("manager", Holders.Session),
    view("grit-coder", Holders.Anyone),
    view("reviewer", Holders.Anyone)
  )

  private def claim(projects: Projects, store: Store, as: String, holder: String) =
    ask(projects, store, "project_claim", "key" -> "p/a", "role" -> as, "holder" -> holder)

  /** A takeover of `p/a` to session `claude-code:s-b`, in role `as`, for `holder`. */
  private def takeover(projects: Projects, store: Store, as: String, holder: String) =
    ask(
      projects,
      store,
      "project_claim",
      "key" -> "p/a",
      "role" -> as,
      "holder" -> holder,
      "takeover" -> "claude-code:s-b"
    )

  private def live(store: Store): Vector[Claim] =
    inTx(store.claims.live(at)).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  val tests = Tests {
    test(
      "a session's holder is refused a role kept for agents, and an agent's one kept for a session"
    ) {
      val (projects, store) = holding(Roles*)
      (
        claim(projects, store, "coder", Session),
        claim(projects, store, "manager", Coder),
        live(store)
      ) ==> ((
        CommandOutcome.Refused(
          s"$Session is a session's holder, and the role coder is held only by agents of type " +
            "coder-haiku or coder-opus. It may claim as grit-coder, manager or reviewer. " +
            "Nothing was changed."
        ),
        CommandOutcome.Refused(
          s"$Coder is a coder-opus agent's holder, and the role manager is held only by a " +
            "session, never by an agent. It may claim as coder, grit-coder or reviewer. " +
            "Nothing was changed."
        ),
        Vector()
      ))
    }
    test(
      "an agent kept by its hash, and a holder of neither form, are refused a role kept for one"
    ) {
      val (projects, store) = holding(Roles*)
      val hashed = s"$Session/explore-5e6f7a8b/0123456789ab"
      (
        claim(projects, store, "coder", hashed),
        claim(projects, store, "manager", "nick")
      ) ==> ((
        CommandOutcome.Refused(
          s"$hashed is an agent's holder, and the role coder is held only by agents of type " +
            "coder-haiku or coder-opus. It may claim as grit-coder or reviewer. Nothing was changed."
        ),
        CommandOutcome.Refused(
          "nick is neither a session's holder nor an agent's, and the role manager is held only " +
            "by a session, never by an agent. It may claim as grit-coder or reviewer. Nothing " +
            "was changed."
        )
      ))
    }
    test("a holder no role admits is told so") {
      val (projects, store) = holding(view("coder", coders), view("manager", Holders.Session))
      claim(projects, store, "manager", "nick") ==> CommandOutcome.Refused(
        "nick is neither a session's holder nor an agent's, and the role manager is held only " +
          "by a session, never by an agent. No role of method grit version 1 admits it. " +
          "Nothing was changed."
      )
    }
    test("oldest and takeover are refused alike, claiming and moving nothing") {
      val (projects, store) = holding(Roles*)
      done(claim(projects, store, "reviewer", Session))
      val before = live(store)
      val refusal = CommandOutcome.Refused(
        s"$Session is a session's holder, and the role coder is held only by agents of type " +
          "coder-haiku or coder-opus. It may claim as grit-coder, manager or reviewer. " +
          "Nothing was changed."
      )
      (
        ask(
          projects,
          store,
          "project_claim",
          "oldest" -> ujson.Obj("project" -> "p", "kind" -> "note", "state" -> "open"),
          "role" -> "coder",
          "holder" -> Session
        ),
        takeover(projects, store, "coder", Session),
        live(store) == before
      ) ==> ((refusal, refusal, true))
    }
    test("a holder its role admits is claimed: an agent of its types, and a session") {
      val (projects, store) = holding(Roles*)
      def claimed(said: String) =
        CommandOutcome.Done(ujson.write(ujson.Obj("claimed" -> said, "brief" -> "kept")))
      (
        claim(projects, store, "coder", Coder),
        ask(
          projects,
          store,
          "project_claim",
          "key" -> "p/b",
          "role" -> "manager",
          "holder" -> Session
        )
      ) ==> ((
        claimed(s"Claimed p/1 a (note, open) as coder for $Coder."),
        claimed(s"Claimed p/2 b (note, open) as manager for $Session.")
      ))
    }
  }
}
