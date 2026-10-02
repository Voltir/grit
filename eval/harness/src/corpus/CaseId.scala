package grit.eval.harness.corpus

import grit.core.id.{EntryId, SourceId}
import grit.core.inbox.InboundId
import grit.core.store.Origin

/** A heard message's identity outside any one database: its Slack channel and its ts, which a
  * reset or a re-backfill keeps; written `C0123/1727000000.123456`. Neither part is empty or
  * holds a `/`.
  */
final case class CaseId private (channel: String, ts: SourceId) {
  def written: String = s"$channel/${SourceId.value(ts)}"
}

object CaseId {

  /** The case written as `written` ([[CaseId.written]]); why not, when it is not two non-empty
    * parts split by one `/`.
    */
  def read(written: String): Either[String, CaseId] =
    written.split("/", -1) match {
      case Array(channel, ts) => make(channel, ts).toRight(s"not a case id: $written")
      case _ => Left(s"not a case id: $written")
    }

  /** The case `entry` is, in a conversation whose origin is `origin`; `None` when it is not a
    * Slack message's inbound entry ([[InboundId]]), or its channel or ts holds a `/`.
    */
  def of(origin: Origin, entry: EntryId): Option[CaseId] = origin match {
    case Origin.Slack(_, channel, _) =>
      InboundId.source(entry).flatMap((_, source) => make(channel, SourceId.value(source)))
    case Origin.Tui(_, _) | Origin.Task(_, _) => None
  }

  /** The case of the message that began `origin`'s thread, the thread's root, whose ts names
    * it; `None` when `origin` is not a Slack thread, or its channel or ts holds a `/`.
    */
  def opening(origin: Origin): Option[CaseId] = origin match {
    case Origin.Slack(_, channel, threadTs) => make(channel, threadTs)
    case Origin.Tui(_, _) | Origin.Task(_, _) => None
  }

  /** By their written forms. */
  given Ordering[CaseId] = Ordering.by(_.written)

  private def make(channel: String, ts: String): Option[CaseId] =
    Option.when(Vector(channel, ts).forall(p => p.nonEmpty && !p.contains('/')))(
      new CaseId(channel, SourceId(ts))
    )
}
