package grit.projects.plugin

import grit.core.claim.Claim
import grit.core.document.{BriefClaim, DocText, Takeover}
import grit.core.id.{ClaimHolder, DocKey, SessionId}
import grit.core.plugin.CommandOutcome
import grit.core.visibility.Integrity
import grit.projects.method.{Cover, MethodFixtures, RoleView}

import utest.*

/** `project_claim` for a session: the session it names, a second session refused, and a takeover
  * moving a claim to another session by the person who made it.
  */
object SessionClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{note, right, role}

  private val holder = "desk/grit-1"

  private val agent = "desk/grit-1/coder-opus/abc"

  private def session(local: String): SessionId = right(SessionId.read(s"claude-code:$local"))

  /** A project `p` holding two notes, `p/a` and `p/b`. */
  private def holding(projects: Projects, store: Store): Unit = {
    started(projects, store)
    kept(projects, store, "a", "note", "Text.")
    kept(projects, store, "b", "note", "Text.")
  }

  /** `p/a` claimed as builder for `forHolder`, with `extra` arguments, as [[ask]] asks. */
  private def claim(
      projects: Projects,
      store: Store,
      forHolder: String,
      extra: (String, ujson.Value)*
  ): CommandOutcome =
    ask(
      projects,
      store,
      "project_claim",
      (Vector[(String, ujson.Value)]("key" -> "p/a", "role" -> "builder", "holder" -> forHolder) ++
        extra)*
    )

  private def live(store: Store): Vector[Claim] =
    inTx(store.claims.live(at)).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** Ten minutes after [[CommandFixtures.at]]: when [[takenOver]] asks, so a claim moved is seen
    * made anew.
    */
  private val later = at.plusSeconds(600)

  /** `p/a`'s claim taken over as builder for `forHolder` and its session `to`, at [[later]]. */
  private def takenOver(projects: Projects, store: Store, forHolder: String, to: String) =
    askAt(later)(
      projects,
      store,
      "project_claim",
      "key" -> "p/a",
      "role" -> "builder",
      "holder" -> forHolder,
      "takeover" -> to
    )

  /** `p/a`'s brief as builder, as `project_brief` renders it. */
  private def rendered(projects: Projects, store: Store): String =
    done(
      ask(projects, store, "project_brief", "key" -> "p/a", "role" -> "builder", "chars" -> 9500)
    )

  /** The claim a brief of `p/1` names, and how it is taken over. */
  private val takeover = BriefClaim(right(DocKey.of("p/1")), Some(Takeover("project_claim", "p/1")))

  /** `forHolder`'s brief after [[later]]: its text and the claim its data names. */
  private def briefed(projects: Projects, store: Store, forHolder: String) =
    briefOf(store, forHolder, later.plusSeconds(1)).flatMap(d =>
      BriefClaim.of(d).map(c => (DocText.value(d.text), c))
    )

  private def claimedSaying(said: String): CommandOutcome =
    CommandOutcome.Done(ujson.write(ujson.Obj("claimed" -> said, "brief" -> "kept")))

  val tests: Tests = Tests {

    test("takeover given by a session is refused, moving nothing") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      val before = live(store)
      val refused = askBelieved(Integrity.Member)(
        projects,
        store,
        "project_claim",
        "key" -> "p/a",
        "role" -> "builder",
        "holder" -> "laptop/grit-2",
        "takeover" -> "claude-code:s-b"
      )
      assert(
        refused == CommandOutcome.Refused(
          s"takeover is given only by ${ProjectCommands.PersonOnlyBy}."
        ),
        live(store) == before
      )
    }

    test("takeover of a claim someone else made is refused, moving nothing") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      val before = live(store)
      val refused = askAs(other)(
        projects,
        store,
        "project_claim",
        "key" -> "p/a",
        "role" -> "builder",
        "holder" -> "laptop/grit-2",
        "takeover" -> "claude-code:s-b"
      )
      assert(
        refused == CommandOutcome.Refused(
          s"p/1 a was claimed by $by, as builder for $holder; takeover moves only a claim you " +
            "made yourself. project_release frees it."
        ),
        live(store) == before
      )
    }

    test(
      "takeover moves the claim to the session and holder it names, whatever holds it, and its brief with it"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      val moved = takenOver(projects, store, "laptop/grit-2", "claude-code:s-b")
      assert(
        moved == claimedSaying(
          "Moved p/1 a (note, open) to laptop/grit-2 as builder, for session claude-code:s-b."
        ),
        live(store).map(c => (ClaimHolder.value(c.holder), c.session, c.made)) ==
          Vector(("laptop/grit-2", Some(session("s-b")), later)),
        briefOf(store, holder, later.plusSeconds(1)).isEmpty,
        briefed(projects, store, "laptop/grit-2") == Some((rendered(projects, store), takeover))
      )
    }

    test(
      "takeover by the claim's own holder moves it from one of its sessions to another, keeping its brief"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      val moved = takenOver(projects, store, holder, "claude-code:s-b")
      assert(
        moved == claimedSaying(
          s"Moved p/1 a (note, open) to $holder as builder, for session claude-code:s-b."
        ),
        live(store).map(c => (ClaimHolder.value(c.holder), c.session, c.made)) ==
          Vector((holder, Some(session("s-b")), later)),
        briefed(projects, store, holder) == Some((rendered(projects, store), takeover))
      )
    }

    test("takeover moves a covering claim, which covers nothing its move must step around") {
      val projects = plugin()
      val store = Store(declared =
        Vector(
          method(roles =
            Vector(right(RoleView.of(role("manager"), Set(note.name), 1, covers = Cover.Under)))
          )
        )
      )
      holding(projects, store)
      done(
        ask(
          projects,
          store,
          "project_claim",
          "key" -> "p/a",
          "role" -> "manager",
          "holder" -> holder,
          "session" -> "claude-code:s-a"
        )
      )
      val moved = ask(
        projects,
        store,
        "project_claim",
        "key" -> "p/a",
        "role" -> "manager",
        "holder" -> "laptop/grit-2",
        "takeover" -> "claude-code:s-b"
      )
      moved ==> claimedSaying(
        "Moved p/1 a (note, open) to laptop/grit-2 as manager, for session claude-code:s-b."
      )
    }

    test("project_claims names the session working each claim") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      ujson.read(done(ask(projects, store, "project_claims")))("claims")(0)("session") ==>
        ujson.Str("claude-code:s-a")
    }

    test("takeover of an artifact no one holds claims it for the session it names") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val claimed = claim(projects, store, holder, "takeover" -> "claude-code:s-b")
      assert(
        claimed == claimedSaying(s"Claimed p/1 a (note, open) as builder for $holder."),
        live(store).map(_.session) == Vector(Some(session("s-b")))
      )
    }

    test(
      "a second session claiming its holder's claim is refused naming the session and the takeover, changing nothing"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      val before = live(store)
      val lapses = at.plusSeconds(7200)
      assert(
        claim(projects, store, holder, "session" -> "claude-code:s-b") == CommandOutcome.Refused(
          s"p/1 a is claimed already as builder for $holder, and session claude-code:s-a is " +
            s"working on it (last active $at). The claim frees itself when that session ends, or " +
            s"at $lapses if it does nothing more. Nothing was changed. To move it to your session " +
            "now, the person who made the claim runs project_claim themselves, outside any " +
            "session, with: " +
            s"""{"key":"p/1","role":"builder","holder":"$holder","takeover":"claude-code:s-b"}"""
        ),
        live(store) == before
      )
    }

    test(
      "a manager's session given with an agent's holder is refused, saying to leave session out, changing nothing"
    ) {
      val projects = plugin()
      val store = Store(sessions = Map(session("s-m") -> (right(ClaimHolder.of(holder)), at)))
      holding(projects, store)
      assert(
        claim(projects, store, agent, "session" -> "claude-code:s-m") == CommandOutcome.Refused(
          s"session claude-code:s-m works as $holder, not $agent. Leave `session` out when you " +
            "claim for an agent or anyone else."
        ),
        live(store) == Vector.empty
      )
    }

    test("a claim naming no session, on one another session works, says that session works it") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claim(projects, store, holder, "session" -> "claude-code:s-a"))
      claim(projects, store, holder) ==> claimedSaying(
        s"Claimed p/1 a (note, open) as builder for $holder; session claude-code:s-a is working on it."
      )
    }

    test("a claim of the oldest artifact for a session is worked by that session") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(
        ask(
          projects,
          store,
          "project_claim",
          "oldest" -> ujson.Obj("project" -> "p", "kind" -> "note", "state" -> "open"),
          "role" -> "builder",
          "holder" -> holder,
          "session" -> "claude-code:s-a"
        )
      )
      live(store).map(c => (ClaimHolder.value(c.holder), c.session)) ==>
        Vector((holder, Some(session("s-a"))))
    }

    test("arguments that contradict or do not parse are refused before anything is read") {
      val projects = plugin()
      (
        refusedArgs(
          projects,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> holder,
          "session" -> "claude-code:s-a",
          "takeover" -> "claude-code:s-b"
        ),
        refusedArgs(
          projects,
          "project_claim",
          "oldest" -> ujson.Obj("project" -> "p", "kind" -> "note", "state" -> "open"),
          "role" -> "builder",
          "holder" -> holder,
          "takeover" -> "claude-code:s-b"
        ),
        refusedArgs(
          projects,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> holder,
          "session" -> "s-a"
        ),
        refusedArgs(
          projects,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> holder,
          "takeover" -> "s-b"
        )
      ) ==> (
        Some(
          "`session` and `takeover` are both given: give `session` to claim for your own " +
            "session, or `takeover` to move a claim to the session it names"
        ),
        Some("`takeover` moves the claim on one artifact: give `key`, not `oldest`"),
        Some("not a session's id, written <harness>:<id>"),
        Some("not a session's id, written <harness>:<id>")
      )
    }
  }
}
