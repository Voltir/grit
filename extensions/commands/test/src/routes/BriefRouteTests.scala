package grit.commands.routes

import java.time.Instant

import grit.core.claim.ClaimRole
import grit.core.context.Shown
import grit.core.document.{DocRef, Takeover}
import grit.core.edge.{EdgeContracts, EdgeSessions, ScriptedSessions}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, Harness, SessionId}
import grit.core.place.Place
import grit.core.session.{Event, Session, SessionDigest}
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `GET /brief`: a holder's brief, or the digest of what the caller's own session did lately. */
object BriefRouteTests extends TestSuite {

  private val MadeUp: Harness = got(Harness.of("made-up"))
  private val Holder: ClaimHolder = got(ClaimHolder.of("desktop/grit-dcfe7476"))
  private val Agent: ClaimHolder = got(
    ClaimHolder.of("desktop/grit-dcfe7476/coder-opus/5704e74050f4")
  )
  private val Fresh = "/brief?harness=made-up&session=s-1&dir=/work/grit&start=fresh"
  private val Resume = "/brief?harness=made-up&session=s-1&dir=/work/grit&start=resume"

  private def id(local: String): SessionId = got(SessionId.of(MadeUp, local))

  /** The session `local`, working as `holder`, last active `ago` seconds before [[Now]], having
    * said `said` as it closed.
    */
  private def worked(
      script: ScriptedSessions.Script,
      local: String,
      ago: Long,
      said: String,
      holder: ClaimHolder = Holder
  ): Unit = {
    val at = Now.minusSeconds(ago)
    val closing = got(Session.Summary.of(said)).pipe(Event.Kind.Closing(_))
    val _ = new ScriptedSessions(Set(Owner.account), script).record(
      Owner.account,
      EdgeContracts.Room,
      Vector(
        EdgeSessions.Batch(
          id(local),
          None,
          Place.under(grit.core.place.Namespace.Machine, Vector("desktop", "/work/grit")),
          Some(holder),
          Vector(
            got(Event.of(1, at.minusSeconds(60), Event.Kind.Started, Event.Shape.Empty)),
            got(Event.of(2, at, closing, Event.Shape.Empty))
          )
        )
      )
    )
  }

  private def digest(script: ScriptedSessions.Script, local: String, window: SessionDigest.Window) =
    script
      .session(id(local))
      .map(s =>
        SessionDigest.of(
          ScriptedSessions.lately(s),
          window,
          SessionDigest.Chars
        )
      )

  private def asked(script: ScriptedSessions.Script, target: String, vouched: Boolean = true) = {
    val got = routed(script = script, vouched = vouched).answer(request(Verb.Get, target))
    (got.status, got.body)
  }

  private def briefedAs(script: ScriptedSessions.Script) = script.called.collect {
    case ScriptedSessions.Call.Briefed(by, room, holder, own, at) =>
      (by, room, holder, own, at)
  }

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)

  val tests = Tests {
    test(
      "a resume's digest is its own session's, without what it said, though another of its holder's was active since; a fresh start has none"
    ) {
      val script = new ScriptedSessions.Script
      worked(script, "s-1", 600, "Did this one.")
      worked(script, "s-0", 60, "Did the other thing.")
      (asked(script, s"$Fresh&part=digest"), asked(script, s"$Resume&part=digest")) ==> (
        (Status.Ok, ""),
        (Status.Ok, digest(script, "s-1", SessionDigest.Window.HoldsLastSaid).getOrElse(""))
      )
    }

    test("an agent is briefed as its holder, its own session being session/agent") {
      val script = new ScriptedSessions.Script
      val _ = asked(script, s"$Fresh&agent=a-1&type=coder-opus")
      briefedAs(script).map(c => (c._3, c._4)) ==> Vector((Agent, id("s-1/a-1")))
    }

    test(
      "the brief is the holder's, fenced as a window shows it, and is all that is shown unless the digest is asked for"
    ) {
      val script = new ScriptedSessions.Script
      val none = asked(script, Fresh)
      val brief = ScriptedSessions.document("Do the thing.", Now.minusSeconds(3600))
      script.brief(Holder, brief)
      worked(script, "s-0", 600, "Did the earlier thing.")
      (none, asked(script, Fresh), asked(script, s"$Fresh&part=brief")) ==> (
        (Status.Ok, ""),
        (Status.Ok, Shown.briefText(brief)),
        (Status.Ok, Shown.briefText(brief))
      )
    }

    test(
      "a session whose holder's claim another session works is answered one line naming it and its takeover, never the brief"
    ) {
      val script = new ScriptedSessions.Script
      // An hour before Now, so its claim is live then.
      val made = Instant.parse("2026-11-13T11:00:00Z")
      val subject = got(DocKey.of("p/1"))
      val brief = ScriptedSessions.document("Manage the milestone.", made)
      val move = Takeover("kalandra-cli project_claim", "p/1")
      script.claimedBrief(
        Holder,
        brief,
        subject,
        Some(move),
        got(ClaimRole.of("manager")),
        Some(id("s-0"))
      )
      asked(script, Fresh) ==> (
        Status.Ok,
        "grit: no brief for this session. p/1 is claimed as manager for desktop/grit-dcfe7476, " +
          "and session made-up:s-0 is working on it (last active 2026-11-13T11:00:00Z). The claim " +
          "frees itself when that session ends, or at 2026-11-13T13:00:00Z if it does nothing " +
          "more. To move it here now, the person who made the claim runs kalandra-cli " +
          "project_claim themselves, outside any session, with: " +
          """{"key":"p/1","role":"manager","holder":"desktop/grit-dcfe7476","takeover":"made-up:s-1"}"""
      )
    }

    test("a brief of DocRef.BriefChars characters, the most one holds, is answered whole") {
      val script = new ScriptedSessions.Script
      val brief =
        ScriptedSessions.document("x" * (DocRef.BriefChars - 1) + "y", Now.minusSeconds(3600))
      script.brief(Holder, brief)
      asked(script, s"$Fresh&part=brief") ==> (Status.Ok, Shown.briefText(brief))
    }

    test("a digest of a session last active longer ago than Recent is left out") {
      val (old, recent) = (new ScriptedSessions.Script, new ScriptedSessions.Script)
      worked(old, "s-1", Briefing.Recent.toSeconds + 1, "Did the old thing.")
      worked(recent, "s-1", Briefing.Recent.toSeconds, "Did the recent thing.")
      (asked(old, s"$Resume&part=digest"), asked(recent, s"$Resume&part=digest")) ==> (
        (Status.Ok, ""),
        (Status.Ok, digest(recent, "s-1", SessionDigest.Window.HoldsLastSaid).getOrElse(""))
      )
    }

    test("a query it does not read is 400 naming why, and briefs no one") {
      val script = new ScriptedSessions.Script
      Vector(
        s"$Fresh&part=all",
        s"$Fresh&agent=a-1",
        s"$Fresh&type=coder-opus",
        "/brief?harness=made-up&session=s-1&dir=/work/grit&start=later",
        "/brief?harness=made-up&session=s-1&dir=/work/grit",
        "/brief?harness=made-up&dir=/work/grit&start=fresh"
      ).map(asked(script, _)) ==> Vector(
        (Status.BadRequest, "part: brief or digest, not all"),
        (Status.BadRequest, "agent and type come together: an agent's own id, and its type"),
        (Status.BadRequest, "agent and type come together: an agent's own id, and its type"),
        (Status.BadRequest, "start: fresh or resume, not later"),
        (Status.BadRequest, "no start"),
        (Status.BadRequest, "no session")
      )
      script.called ==> Vector.empty
    }

    test("a session or an agent holding '/' is 400, so no two queries name one session") {
      val script = new ScriptedSessions.Script
      Vector(
        "/brief?harness=made-up&session=s-1/a-1&dir=/work/grit&start=fresh",
        s"$Fresh&agent=a-1/b&type=coder"
      ).map(asked(script, _)) ==> Vector(
        (
          Status.BadRequest,
          "session: holds '/', which parts an agent's session from the session starting it: s-1/a-1"
        ),
        (
          Status.BadRequest,
          "agent: holds '/', which parts an agent's session from the session starting it: a-1/b"
        )
      )
      script.called ==> Vector.empty
    }

    test("an account no person is linked to is 403, and a database that cannot be read 503") {
      val script = new ScriptedSessions.Script
      val stranger = asked(script, Fresh, vouched = false)
      script.failing(Some("down"))
      (stranger, asked(script, Fresh)) ==> (
        (
          Status.Forbidden,
          s"no person is linked to ${grit.core.identity.Account.written(Owner.account)}"
        ),
        (Status.Unavailable, "the database is unavailable: down")
      )
    }

    test("only a GET is answered, and it records nothing") {
      val script = new ScriptedSessions.Script
      val post = routed(script = script).answer(request(Verb.Post, Fresh))
      val _ = asked(script, s"$Fresh&part=digest")
      (post.status, script.called.collect { case r: ScriptedSessions.Call.Recorded => r }) ==>
        (Status.NotAllowed(Vector(Verb.Get)), Vector.empty)
    }
  }
}
