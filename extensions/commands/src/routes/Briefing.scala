package grit.commands.routes

import java.time.Instant

import scala.concurrent.duration.*

import grit.commands.worker.{Holders, WorkerName}
import grit.core.context.Shown
import grit.core.edge.{BriefError, EdgeSessions}
import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.Account
import grit.core.place.Directory
import grit.core.session.SessionDigest
import grit.edge.http.{Request, Status}

/** What `/holder` and `/brief` read from a query, and how `/brief` answers. */
private[commands] object Briefing {

  /** `holder`: where the holder a session works as is asked for. */
  val Holder: String = "holder"

  /** `brief`: where what a session opens with is asked for. */
  val Brief: String = "brief"

  /** 24 hours: how recently a session must have last done something for `/brief` to show its
    * digest; an older one is old work, left out.
    */
  val Recent: FiniteDuration = 24.hours

  /** The holder `/holder`'s query names on `worker` ([[Holders.of]]), or why not:
    *   - `harness`, the harness's name ([[Harness.of]]), and `dir`, the session's directory
    *     ([[Directory.of]]), each sent;
    *   - for an agent, its `type` and the `session` (the harness's own id, [[Holders.session]])
    *     that starts it, sent together, `dir` being that session's directory;
    *   - no other parameter, and none sent twice.
    */
  def holderOf(request: Request, worker: WorkerName): Either[String, ClaimHolder] =
    for {
      q <- Queried.of(request, Vector("harness", "dir", "session", "type"))
      harness <- q.need("harness")(Harness.of)
      dir <- q.need("dir")(Directory.of)
      session <- q.read("session")(Holders.session(harness, _))
      kind <- q.read("type")(Right(_))
      agent <- (kind, session) match {
        case (Some(k), Some(s)) => Right(Some(Holders.Agent(k, s)))
        case (None, None) => Right(None)
        case _ => Left(TypeAndSession)
      }
    } yield Holders.of(worker, dir, agent)

  /** Which part of what a session opens with `/brief` answers. */
  enum Part {

    /** The holder's brief, fenced as a window shows it ([[Shown.briefText]]). */
    case Brief

    /** What the holder's newest session did lately ([[SessionDigest.of]]). */
    case Digest
  }

  /** What `/brief` asks [[EdgeSessions.briefed]] for: `holder`'s opening, `besides` the caller's
    * own session, and which `part` of it to answer, a digest shown for `window`.
    */
  final case class Asked(
      holder: ClaimHolder,
      besides: Option[SessionId],
      window: SessionDigest.Window,
      part: Part
  )

  /** What `/brief`'s query asks for on `worker`, or why not:
    *   - `harness`, `session` (the harness's own id for the caller's session) and `dir`, its
    *     directory, as `/holder` reads them ([[holderOf]]);
    *   - for an agent's start, `agent`, its own id, and `type`, sent together: its holder is the one
    *     `session` starts of that type, and its own session `session/agent`
    *     ([[Holders.agentSession]]);
    *   - `start`: `fresh` when the caller's window holds nothing of its session (a start, a
    *     clear, an agent's start), so the newest session besides its own is shown, with what it
    *     last said; or `resume` when it goes on (a resume, a compaction), so its own session may be
    *     shown, without what it last said, which its window holds;
    *   - `part`: `brief` (the default) or `digest`, so each fits in its own answer;
    *   - no other parameter, and none sent twice.
    */
  def briefOf(request: Request, worker: WorkerName): Either[String, Asked] =
    for {
      q <- Queried.of(
        request,
        Vector("harness", "session", "dir", "agent", "type", "start", "part")
      )
      harness <- q.need("harness")(Harness.of)
      session <- q.need("session")(Holders.session(harness, _))
      dir <- q.need("dir")(Directory.of)
      agent <- q.read("agent")(Right(_))
      kind <- q.read("type")(Right(_))
      started <- Holders.agent(session, agent, kind)
      fresh <- q.need("start") {
        case "fresh" => Right(true)
        case "resume" => Right(false)
        case other => Left(s"fresh or resume, not $other")
      }
      part <- q.read("part") {
        case "brief" => Right(Part.Brief)
        case "digest" => Right(Part.Digest)
        case other => Left(s"brief or digest, not $other")
      }
    } yield Asked(
      Holders.of(worker, dir, started.map(_._2)),
      Option.when(fresh)(started.fold(session)(_._1)),
      if (fresh) SessionDigest.Window.LacksLastSaid else SessionDigest.Window.HoldsLastSaid,
      part.getOrElse(Part.Brief)
    )

  /** What `/brief` answers for `asked` once briefed `came` to that, at `now`: 200 with the part
    * asked for, empty when there is none, a digest left out when its session last did something
    * longer than [[Recent]] before `now`; at most [[grit.core.document.DocRef.BriefChars]] of a
    * brief, fenced, or [[SessionDigest.Chars]] of a digest. 403 for an account no person is
    * linked to; 503 when the database could not be read.
    */
  def briefed(
      asked: Asked,
      came: Either[BriefError, EdgeSessions.Briefed],
      now: Instant
  ): (Status, String) = came match {
    case Left(BriefError.NotVouched(by)) =>
      (Status.Forbidden, s"no person is linked to ${Account.written(by)}")
    case Left(BriefError.Unavailable(cause)) =>
      (Status.Unavailable, s"the database is unavailable: $cause")
    case Right(b) =>
      asked.part match {
        case Part.Brief => (Status.Ok, b.brief.fold("")(Shown.briefText))
        case Part.Digest =>
          val since = now.minusMillis(Recent.toMillis)
          (
            Status.Ok,
            b.lately
              .filterNot(_.session.lastEvent.isBefore(since))
              .fold("")(SessionDigest.of(_, asked.window, SessionDigest.Chars))
          )
      }
  }

  private val TypeAndSession =
    "type and session come together: an agent's type, and the session starting it"
}
