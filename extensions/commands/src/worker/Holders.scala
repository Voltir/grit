package grit.commands.worker

import grit.core.id.{ClaimHolder, SessionId, Sha256}
import grit.core.place.Directory

/** The claim holder a harness's session works as, so a claim can name it before the session's
  * work begins (ADR 0049), and the session's events keep that claim from lapsing (ADR 0050).
  */
object Holders {

  /** 40: the most characters of a directory's last segment, or of an agent's type, a holder
    * keeps, slugged.
    */
  val Slugged: Int = 40

  /** An agent of type `kind`, as its harness names it, that the session `session` starts. */
  final case class Agent(kind: String, session: SessionId)

  /** The session an agent that `session` starts is recorded as, `agent` its harness's own id for
    * it: `<session's id>/<agent>`, of `session`'s harness; or why not ([[SessionId.of]]).
    */
  def agentSession(session: SessionId, agent: String): Either[String, SessionId] =
    SessionId.of(session.harness, s"${session.local}/$agent")

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
    (agent, kind) match {
      case (None, None) => Right(None)
      case (Some(a), Some(k)) =>
        agentSession(session, a).left
          .map(w => s"agent: $w")
          .map(id => Some((id, Agent(k, session))))
      case _ => Left("agent and type come together: an agent's own id, and its type")
    }

  /** The holder a session in `dir` on `worker` works as, or, given `agent`, the one that agent
    * works as, `dir` then being the starting session's directory, not the agent's own:
    * `<worker>/<segment>-<dir's hash>[/<type>/<session's hash>]`. `segment` is `dir`'s last
    * segment and `type` the agent's, each slugged (lower case, every run of other characters one
    * `-`) and cut to [[Slugged]]. A type is kept as it is sent only when it is not empty, slugging
    * and cutting leave it unchanged, and it is not 8 hex digits alone nor ends in `-` and 8 hex
    * digits; any other is slugged, cut and followed by `-` and its own hash, or is that hash alone
    * when it slugs to nothing, as a segment is. So two types never share a holder, short of a
    * hash collision.
    * A directory's or a type's hash is the first 8 hex digits of its SHA-256, a session's the
    * first 12 of `<harness>:<id>`'s. Equal inputs give equal holders; one directory on two
    * workers, two.
    */
  def of(worker: WorkerName, dir: Directory, agent: Option[Agent]): ClaimHolder = {
    val path = Directory.value(dir)
    val head =
      s"${WorkerName.value(worker)}/${named(path.split('/').lastOption.getOrElse(""), path)}"
    val text = agent.fold(head) { a =>
      val kind = if (literal(a.kind)) a.kind else named(a.kind, a.kind)
      s"$head/$kind/${hash(a.session.written, 12)}"
    }
    // The Left is unreachable, so this throw never runs (STYLE.md rule 3): ClaimHolder.of refuses
    // only text that is empty, over ClaimHolder.Longest (160) characters, or holds a control
    // character. `text` starts with the worker's name, a slug of 1 to WorkerName.Longest (32)
    // characters; every other part is a slug ([a-z0-9-]), hex digits or `/`, so it holds no
    // control character; and it is at most 32 + 1 + 49 (a segment: Slugged 40, `-`, 8 hex) + 1
    // + 49 (a type: kept as sent at most Slugged 40, hashed at most 49) + 1 + 12 (the session's
    // hash) = 145 characters.
    ClaimHolder.of(text).fold(why => throw new IllegalStateException(why), identity)
  }

  /** `text` slugged and cut, then `-` and the hash of `hashed`; the hash alone when the slug is
    * empty.
    */
  private def named(text: String, hashed: String): String = {
    val s = slug(text).take(Slugged).stripSuffix("-")
    val h = hash(hashed, 8)
    if (s.isEmpty) h else s"$s-$h"
  }

  private def slug(text: String): String =
    text
      .toLowerCase(java.util.Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "-")
      .stripPrefix("-")
      .stripSuffix("-")

  /** Whether a type is kept as sent: [[named]] never gives it, so no hashed type equals it. */
  private def literal(kind: String): Boolean =
    kind.nonEmpty && slug(kind).take(Slugged) == kind && !Hashed.matches(kind)

  /** What [[named]] gives: 8 hex digits, after a slug and `-` or alone. */
  private val Hashed = "(?:.*-)?[0-9a-f]{8}".r

  private def hash(text: String, digits: Int): String = Sha256.text(text).hex.take(digits)
}
