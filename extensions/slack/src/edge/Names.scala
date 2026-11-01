package grit.slack.edge

import java.time.ZoneId
import java.util.concurrent.ConcurrentHashMap

import grit.slack.client.{Member, Slack}
import grit.slack.event.{ChannelId, TeamId, UserId}

/** The Slack names of grit's team's users and of channels, as an edge shows them: each asked of
  * Slack once and kept, what Slack could not say said through `said`.
  */
private[slack] final class Names(slack: Slack, team: TeamId, said: String => Unit) {
  import Names.Named

  // Caches, each written only through its own atomic operations. What they hold is looked up
  // again from Slack when missing, so a reader that races a writer sees either value, and both
  // are right.
  @caps.unsafe.untrackedCaptures
  private val names = new ConcurrentHashMap[UserId, Named]()

  @caps.unsafe.untrackedCaptures
  private val channelNames = new ConcurrentHashMap[ChannelId, String]()

  /** Keeps `named` as `user`'s name and zone, as Slack gave them in another answer. */
  def remember(user: UserId, named: Named): Unit = {
    val _ = names.put(user, named)
  }

  /** `channel` as a person reads it: `#{name} ({id})`, its id alone when Slack gives no name. */
  def shown(channel: ChannelId): String = {
    val id = ChannelId.value(channel)
    channelNameOf(channel).fold(id)(name => s"#$name ($id)")
  }

  /** `user`'s Slack name and zone, asked of Slack once per user; `None` when they have no name,
    * or Slack could not say (the id stands in, and they are asked again next time).
    */
  def namedOf(user: UserId): Option[Named] =
    Option(names.get(user)).orElse {
      slack.member(team, user) match {
        case Right(Member(Some(n), _, zone)) =>
          val named = Named(n, zone)
          val _ = names.put(user, named)
          Some(named)
        case Right(Member(None, _, _)) => None
        case Left(e) =>
          said(s"slack: no name for ${UserId.value(user)}: $e")
          None
      }
    }

  /** `channel`'s Slack name, asked of Slack once per channel; `None` when it has none grit may
    * see, or Slack could not say (the id stands in, and it is asked again next time).
    */
  def channelNameOf(channel: ChannelId): Option[String] =
    Option(channelNames.get(channel)).orElse {
      slack.channelName(channel) match {
        case Right(Some(n)) =>
          val _ = channelNames.put(channel, n)
          Some(n)
        case Right(None) => None
        case Left(e) =>
          said(s"slack: no name for channel ${ChannelId.value(channel)}: $e")
          None
      }
    }
}

private[slack] object Names {

  /** What the edge keeps of a Slack user it has asked about: the name they show, and the zone
    * Slack places them in, if any.
    */
  final case class Named(name: String, zone: Option[ZoneId])
}
