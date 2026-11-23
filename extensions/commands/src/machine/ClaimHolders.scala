package grit.commands.machine

import grit.core.id.{AgentType, ClaimHolder, Harness, SessionId, ShortHash, Slug}
import grit.core.place.Directory
import grit.std.either.Proven

/** The claim holder a harness's session works as, so a claim can name it before the session's
  * work begins (ADR 0049), and the session's events keep that claim from lapsing (ADR 0050).
  */
object ClaimHolders {

  /** 40: the most characters of a directory's last segment a holder keeps, slugged, and of the
    * slug before a hashed agent type's hash.
    */
  val Slugged: Int = 40

  /** An agent of type `kind`, as its harness names it, that the session `session` starts. */
  final case class Agent(kind: String, session: SessionId)

  /** The session `local` of `harness`, as a request names a harness's own session, or why not:
    * `local` holds `/`, which only an agent's session does ([[agentSession]]), so that no two
    * requests' ids name one session; or [[SessionId.of]].
    */
  def session(harness: Harness, local: String): Either[String, SessionId] =
    unparted(local).flatMap(SessionId.of(harness, _))

  /** The session an agent that `session` starts is recorded as, `agent` its harness's own id for
    * it: `<session's id>/<agent>`, of `session`'s harness; or why not: `agent` holds `/`, or
    * [[SessionId.of]]. Its parent is recorded apart ([[grit.core.edge.EdgeSessions.Batch]]'s
    * `parent`), never read back from the id.
    */
  def agentSession(session: SessionId, agent: String): Either[String, SessionId] =
    unparted(agent).flatMap(a => SessionId.of(session.harness, s"${session.local}/$a"))

  /** The agent `session` starts, as a request names it by its own id `agent` and its `kind`,
    * sent together: the session it is recorded as ([[agentSession]]) and the agent; `None` when
    * neither is sent; or why not: one sent without the other, or `agent: ` and why
    * [[agentSession]] refuses it.
    */
  def agent(
      session: SessionId,
      agent: Option[String],
      kind: Option[String]
  ): Either[String, Option[(SessionId, Agent)]] =
    paired(agent, kind, "agent and type come together: an agent's own id, and its type")
      .flatMap {
        case None => Right(None)
        case Some((a, k)) =>
          agentSession(session, a).left
            .map(w => s"agent: $w")
            .map(id => Some((id, Agent(k, session))))
      }

  /** Both or neither: `None` for neither, the pair for both, `apart` for one alone. */
  private[commands] def paired[A, B](
      a: Option[A],
      b: Option[B],
      apart: String
  ): Either[String, Option[(A, B)]] = (a, b) match {
    case (None, None) => Right(None)
    case (Some(x), Some(y)) => Right(Some((x, y)))
    case _ => Left(apart)
  }

  /** The holder a session in `dir` on `machine` works as, or, given `agent`, the one that agent
    * works as, `dir` then being the starting session's directory, not the agent's own. One
    * directory on two machines gives two; two types, two, short of a hash collision.
    */
  def of(machine: MachineName, dir: Directory, agent: Option[Agent]): ClaimHolder = {
    val path = Directory.value(dir)
    // ClaimHolder.session and ClaimHolder.agent refuse a part that is empty or holds `/`, an
    // agent started by a holder not a session's, and what ClaimHolder.of refuses: text that is
    // empty, over ClaimHolder.Longest (160) characters, or holds a control character. The
    // machine's name is a slug of 1 to MachineName.Longest (32) characters; every other part is
    // a nonempty slug ([a-z0-9-]) or hex digits (`named` gives at least 8), so none is empty or
    // holds `/` or a control character; the head is `session`'s, so `agent` takes it; and the
    // whole is at most 32 + 1 + 49 (a segment: Slugged 40, `-`, 8 hex) + 1 + 49 (a type: kept
    // whole at most AgentType.Longest 40, hashed at most 49) + 1 + 12 (the session's hash) = 145
    // characters.
    Proven.right(
      ClaimHolder
        .session(MachineName.value(machine), named(path.split('/').lastOption.getOrElse(""), path))
        .flatMap(head =>
          agent.fold(Right(head)) { a =>
            ClaimHolder.agent(
              head,
              AgentType.of(a.kind).fold(_ => named(a.kind, a.kind), AgentType.value),
              ShortHash.of(a.session.written, 12)
            )
          }
        )
    )
  }

  // A segment, and a type that is not kept as sent, is its slug, `-` and the first 8 hex digits of
  // its SHA-256, or that hash alone when it slugs to nothing; a session's hash is the first 12 of
  // `<harness>:<id>`'s. A type is kept as sent only when it is not empty, slugging leaves it
  // unchanged, and it is neither 8 hex digits alone nor ends in `-` and 8 hex digits, so no
  // hashed type equals a kept one.
  private def named(text: String, hashed: String): String = {
    val s = Slug.slugged(text, Slugged)
    val h = ShortHash.of(hashed, 8)
    if (s.isEmpty) h else s"$s-$h"
  }

  private def unparted(id: String): Either[String, String] =
    Either.cond(
      !id.contains('/'),
      id,
      s"holds '/', which parts an agent's session from the session starting it: $id"
    )

}
