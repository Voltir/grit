package grit.commands.routes

import grit.core.context.Shown
import grit.core.edge.{EdgeContracts, EdgeSessions, ScriptedSessions}
import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.place.Place
import grit.core.session.{Event, Session, SessionDigest}
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `GET /brief`: a holder's brief, or the digest of what its newest other session did lately. */
object BriefRouteTests extends TestSuite {

  private val ClaudeCode: Harness = got(Harness.of("claude-code"))
  private val Holder: ClaimHolder = got(ClaimHolder.of("desktop/grit-dcfe7476"))
  private val Agent: ClaimHolder = got(
    ClaimHolder.of("desktop/grit-dcfe7476/coder-opus/0e77e4b3dbc2")
  )
  private val Fresh = "/brief?harness=claude-code&session=s-1&dir=/work/grit&start=fresh"
  private val Resume = "/brief?harness=claude-code&session=s-1&dir=/work/grit&start=resume"

  private def id(local: String): SessionId = got(SessionId.of(ClaudeCode, local))

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
          Place.under(grit.core.place.Namespace.Worker, Vector("desktop", "/work/grit")),
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
          Session.Lately(s, Vector.empty, 0, Vector.empty, Vector.empty),
          window,
          SessionDigest.Chars
        )
      )

  private def asked(script: ScriptedSessions.Script, target: String, vouched: Boolean = true) = {
    val got = routed(script = script, vouched = vouched).answer(request(Verb.Get, target))
    (got.status, got.body)
  }

  private def briefedAs(script: ScriptedSessions.Script) = script.called.collect {
    case ScriptedSessions.Call.Briefed(by, room, holder, besides, at) =>
      (by, room, holder, besides, at)
  }

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)

  val tests = Tests {
    test(
      "a fresh start's digest is the newest other session's, with what it said; a resume's is its own, without"
    ) {
      val script = new ScriptedSessions.Script
      worked(script, "s-0", 600, "Did the earlier thing.")
      worked(script, "s-1", 60, "Did this one.")
      (
        asked(script, s"$Fresh&part=digest"),
        asked(script, s"$Resume&part=digest"),
        briefedAs(script)
      ) ==> (
        (Status.Ok, digest(script, "s-0", SessionDigest.Window.LacksLastSaid).getOrElse("")),
        (Status.Ok, digest(script, "s-1", SessionDigest.Window.HoldsLastSaid).getOrElse("")),
        Vector(
          (Owner.account, EdgeContracts.Room, Holder, Some(id("s-1")), Now),
          (Owner.account, EdgeContracts.Room, Holder, None, Now)
        )
      )
    }

    test("an agent is briefed as its holder, its own session being session/agent") {
      val script = new ScriptedSessions.Script
      val _ = asked(script, s"$Fresh&agent=a-1&type=coder-opus")
      briefedAs(script).map(c => (c._3, c._4)) ==> Vector((Agent, Some(id("s-1/a-1"))))
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

    test("a digest of a session last active longer ago than Recent is left out") {
      val script = new ScriptedSessions.Script
      worked(script, "s-0", Briefing.Recent.toSeconds + 1, "Did the old thing.")
      val old = asked(script, s"$Fresh&part=digest")
      worked(script, "s-2", Briefing.Recent.toSeconds, "Did the recent thing.")
      (old, asked(script, s"$Fresh&part=digest")) ==> (
        (Status.Ok, ""),
        (Status.Ok, digest(script, "s-2", SessionDigest.Window.LacksLastSaid).getOrElse(""))
      )
    }

    test("a query it does not read is 400 naming why, and briefs no one") {
      val script = new ScriptedSessions.Script
      Vector(
        s"$Fresh&part=all",
        s"$Fresh&agent=a-1",
        s"$Fresh&type=coder-opus",
        "/brief?harness=claude-code&session=s-1&dir=/work/grit&start=later",
        "/brief?harness=claude-code&session=s-1&dir=/work/grit",
        "/brief?harness=claude-code&dir=/work/grit&start=fresh"
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
