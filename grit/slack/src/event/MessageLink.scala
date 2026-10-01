package grit.slack.event

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

  private val Link =
    "https://[a-z0-9-]+(?:\\.[a-z0-9-]+)*\\.slack\\.com/archives/([CGD][A-Z0-9]+)/p([0-9]{10})([0-9]{6})(?:\\?(.*))?".r

  private val Stamp = "([0-9]{10}\\.[0-9]{6})".r
}
