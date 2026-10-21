package grit.slack.event

import grit.core.id.Href

/** A message `link` names, as Slack writes one
  * (`https://{workspace}.slack.com/archives/{channel}/p{16 digits}`, optionally
  * `?thread_ts={ts}`): its channel, and the thread it is in, its own ts when it is a thread's
  * root or stands alone.
  */
final case class MessageLink(channel: ChannelId, thread: Ts)

object MessageLink {

  /** The message `link` names, spaces around it ignored; `None` for any other text. */
  def read(link: String): Option[MessageLink] =
    link.trim match {
      case Link(channel, seconds, micros, query) =>
        val thread = Option(query).fold(Vector.empty[String])(_.split('&').toVector).collectFirst {
          case p if p.startsWith("thread_ts=") => p.stripPrefix("thread_ts=")
        }
        thread match {
          case None => Some(MessageLink(ChannelId(channel), Ts(s"$seconds.$micros")))
          case Some(Stamp(ts)) => Some(MessageLink(ChannelId(channel), Ts(ts)))
          case Some(_) => None
        }
      case _ => None
    }

  /** The link to message `ts` of `channel`, in the thread rooted at `thread`, in the workspace
    * at `workspace` (`auth.test`'s `url`, a trailing `/` or none), as Slack writes it and
    * [[read]] reads it back: `{workspace}/archives/{channel}/p{ts, its dot dropped}`, then
    * `?thread_ts={thread}&cid={channel}` for a reply (`ts` not `thread`). `None` only when that
    * reads as no [[Href]], which a workspace's href never makes it.
    */
  def to(workspace: Href, channel: ChannelId, thread: Ts, ts: Ts): Option[Href] = {
    val at = Href.value(workspace).stripSuffix("/")
    val c = ChannelId.value(channel)
    val message = s"$at/archives/$c/p${Ts.value(ts).replace(".", "")}"
    Href.of(
      if (ts == thread) message else s"$message?thread_ts=${Ts.value(thread)}&cid=$c"
    )
  }

  private val Link =
    "https://[a-z0-9-]+(?:\\.[a-z0-9-]+)*\\.slack\\.com/archives/([CGD][A-Z0-9]+)/p([0-9]{10})([0-9]{6})(?:\\?(.*))?".r

  private val Stamp = "([0-9]{10}\\.[0-9]{6})".r
}
