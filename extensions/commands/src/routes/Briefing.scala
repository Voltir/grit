package grit.commands.routes

import java.time.Instant

import scala.concurrent.duration.*

import grit.commands.machine.{ClaimHolders, MachineName}
import grit.core.claim.{Claim, ClaimRole, Claims}
import grit.core.context.Shown
import grit.core.document.Takeover
import grit.core.edge.{BriefError, EdgeSessions}
import grit.core.id.{ClaimHolder, DocKey, Harness, SessionId}
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

  /** The holder `/holder`'s query names on `machine` ([[ClaimHolders.of]]), or why not:
    *   - `harness`, the harness's name ([[Harness.of]]), and `dir`, the session's directory
    *     ([[Directory.of]]), each sent;
    *   - for an agent, its `type` and the `session` (the harness's own id, [[ClaimHolders.session]])
    *     that starts it, sent together, `dir` being that session's directory;
    *   - no other parameter, and none sent twice.
    */
  def holderOf(request: Request, machine: MachineName): Either[String, ClaimHolder] =
    for {
      q <- Queried.of(request, Vector("harness", "dir", "session", "type"))
      harness <- q.need("harness")(Harness.of)
      dir <- q.need("dir")(Directory.of)
      session <- q.read("session")(ClaimHolders.session(harness, _))
      kind <- q.read("type")(Right(_))
      agent <- ClaimHolders
        .paired(kind, session, TypeAndSession)
        .map(_.map(ClaimHolders.Agent(_, _)))
    } yield ClaimHolders.of(machine, dir, agent)

  /** Which part of what a session opens with `/brief` answers. */
  enum Part {

    /** The holder's brief, fenced as a window shows it ([[Shown.briefText]]). */
    case Brief

    /** What the caller's own session did lately ([[SessionDigest.of]]), without what it last
      * said, which its window holds.
      */
    case Digest

    /** Nothing: the digest asked at a fresh start, where the brief is the handover. */
    case Neither
  }

  /** What `/brief` asks [[EdgeSessions.briefed]] for: the opening of `session`, the caller's own,
    * working as `holder`, and which `part` of it to answer.
    */
  final case class BriefRequest(holder: ClaimHolder, session: SessionId, part: Part)

  /** What `/brief`'s query asks for on `machine`, or why not:
    *   - `harness`, `session` (the harness's own id for the caller's session) and `dir`, its
    *     directory, as `/holder` reads them ([[holderOf]]);
    *   - for an agent's start, `agent`, its own id, and `type`, sent together: its holder is the one
    *     `session` starts of that type, and its own session `session/agent`
    *     ([[ClaimHolders.agentSession]]);
    *   - `start`: `fresh` when the caller's window holds nothing of its session (a start, a
    *     clear, an agent's start), so no digest is shown ([[Part.Neither]]); or `resume` when it
    *     goes on (a resume, a compaction), so its own session's is;
    *   - `part`: `brief` (the default) or `digest`, so each fits in its own answer;
    *   - no other parameter, and none sent twice.
    */
  def briefOf(request: Request, machine: MachineName): Either[String, BriefRequest] =
    for {
      q <- Queried.of(
        request,
        Vector("harness", "session", "dir", "agent", "type", "start", "part")
      )
      harness <- q.need("harness")(Harness.of)
      session <- q.need("session")(ClaimHolders.session(harness, _))
      dir <- q.need("dir")(Directory.of)
      agent <- q.read("agent")(Right(_))
      kind <- q.read("type")(Right(_))
      started <- ClaimHolders.agent(session, agent, kind)
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
    } yield BriefRequest(
      ClaimHolders.of(machine, dir, started.map(_._2)),
      started.fold(session)(_._1),
      part match {
        case Some(Part.Digest) if fresh => Part.Neither
        case other => other.getOrElse(Part.Brief)
      }
    )

  /** What `/brief` answers for `asked` once briefed `came` to that, at `now`: 200 with the part
    * asked for, empty when there is none, a digest left out when its session last did something
    * longer than [[Recent]] before `now`; at most [[grit.core.document.DocRef.BriefChars]] of a
    * brief, fenced, or [[SessionDigest.Chars]] of a digest; for a brief another session's claim
    * holds, its one line ([[held]]). 403 for an account no person is linked to; 503 when the
    * database could not be read.
    */
  def briefed(
      asked: BriefRequest,
      came: Either[BriefError, EdgeSessions.Briefed],
      now: Instant
  ): (Status, String) = came match {
    case Left(BriefError.NotVouched(by)) => RouteReply.notVouched(by)
    case Left(BriefError.Unavailable(cause)) => RouteReply.unavailable(cause)
    case Right(b) =>
      asked.part match {
        case Part.Brief =>
          b.brief match {
            case EdgeSessions.Briefed.Brief.Absent => (Status.Ok, "")
            case EdgeSessions.Briefed.Brief.Kept(d) => (Status.Ok, Shown.briefText(d))
            case EdgeSessions.Briefed.Brief.Held(c, t) => (Status.Ok, held(c, t, asked))
          }
        case Part.Digest =>
          val since = now.minusMillis(Recent.toMillis)
          (
            Status.Ok,
            b.lately
              .filterNot(_.session.lastEvent.isBefore(since))
              .fold("")(
                SessionDigest.of(_, SessionDigest.Window.HoldsLastSaid, SessionDigest.Chars)
              )
          )
        case Part.Neither => (Status.Ok, "")
      }
  }

  /** The one line a session `asked` opens with in place of its brief while another session works
    * `claim`: whose it is, who works it and when it frees itself, and how a person moves it to
    * this session (`takeover`), or that releasing it frees it.
    */
  def held(claim: Claim, takeover: Option[Takeover], asked: BriefRequest): String = {
    val key = takeover.fold(DocKey.value(claim.subject))(_.key)
    val named = s"grit: no brief for this session. $key is claimed as " +
      s"${ClaimRole.value(claim.role)} for ${ClaimHolder.value(claim.holder)}"
    val working = claim.session.fold(named)(s => s"$named, ${Claims.working(claim, s)}")
    val moving = takeover.fold("Releasing the claim frees it now.")(
      Takeover.said(_, claim.role, asked.holder, asked.session, "here")
    )
    s"$working. $moving"
  }

  private val TypeAndSession =
    "type and session come together: an agent's type, and the session starting it"
}
