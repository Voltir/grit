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

  /** The holder a session in `dir` on `worker` works as, or, given `agent`, the one that agent
    * works as, `dir` then being the starting session's directory, not the agent's own:
    * `<worker>/<segment>-<dir's hash>[/<type>/<session's hash>]`. `segment` is `dir`'s last
    * segment and `type` the agent's, each slugged (lower case, every run of other characters one
    * `-`) and cut to [[Slugged]]; a type slugging or cutting changed is followed by `-` and its
    * own hash, so two types never share a holder; either, slugged to nothing, is its hash alone.
    * A directory's or a type's hash is the first 8 hex digits of its SHA-256, a session's the
    * first 12 of `<harness>:<id>`'s. Equal inputs give equal holders; one directory on two
    * workers, two.
    */
  def of(worker: WorkerName, dir: Directory, agent: Option[Agent]): ClaimHolder = {
    val path = Directory.value(dir)
    val head =
      s"${WorkerName.value(worker)}/${named(path.split('/').lastOption.getOrElse(""), path)}"
    val text = agent.fold(head) { a =>
      val kind = if (slug(a.kind).take(Slugged) == a.kind) a.kind else named(a.kind, a.kind)
      s"$head/$kind/${hash(a.session.written, 12)}"
    }
    // At most 32 + 1 + 49 + 1 + 49 + 1 + 12 = 145 characters of one line, which ClaimHolder takes.
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

  private def hash(text: String, digits: Int): String = Sha256.text(text).hex.take(digits)
}
