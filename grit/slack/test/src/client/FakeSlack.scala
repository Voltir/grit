package grit.slack.client

import grit.slack.event.{ChannelId, Payloads, TeamId, Ts, UserId}
import grit.slack.text.Post

/** A [[Slack]] for tests, keeping what [[Slack]] says in memory: the handler `listen` was
  * given ([[deliver]] hands it a payload, as Socket Mode would, and says whether it was
  * acknowledged), the posts made, each with its tag, and grit's reactions. Every post is
  * made at a new ts `p1`, `p2`, …
  */
final class FakeSlack extends Slack {

  final case class Posted(channel: ChannelId, thread: Ts, post: Post, tag: Tag, ts: Ts)

  @caps.unsafe.untrackedCaptures
  var handler: Option[String -> Boolean] = None

  @caps.unsafe.untrackedCaptures
  var posts = Vector.empty[Posted]

  /** grit's reactions now, by message. */
  @caps.unsafe.untrackedCaptures
  var reactions = Set.empty[(ChannelId, Ts, String)]

  /** The names people show in Slack. */
  @caps.unsafe.untrackedCaptures
  var names = Map(UserId(Payloads.Ana) -> "Ana Lima")

  /** The channels that are not public. */
  @caps.unsafe.untrackedCaptures
  var privateChannels = Set.empty[ChannelId]

  /** When set, every post fails as Slack being unreachable would. */
  @caps.unsafe.untrackedCaptures
  var down = false

  /** Hands `payload` to the listening handler; whether it was acknowledged. */
  def deliver(payload: String): Boolean = handler.exists(_(payload))

  def self(): Either[SlackError, Self] = Right(Self(TeamId(Payloads.Team), UserId(Payloads.Bot)))

  def listen(handle: String => Boolean): Either[SlackError, Unit] = {
    // Kept past the call, as Socket Mode keeps its listener: the test that made this fake
    // is the only caller of deliver, and it outlives neither the handler nor the fake.
    handler = Some(caps.unsafe.unsafeAssumePure(handle))
    Right(())
  }

  def post(channel: ChannelId, thread: Ts, post: Post, tag: Tag): Either[SlackError, Ts] =
    if (down) Left(SlackError.Unreachable("down"))
    else {
      val ts = Ts(s"p${posts.size + 1}")
      posts = posts :+ Posted(channel, thread, post, tag, ts)
      Right(ts)
    }

  def tagged(channel: ChannelId, thread: Ts, tag: Tag): Either[SlackError, Vector[Ts]] =
    if (down) Left(SlackError.Unreachable("down"))
    else
      Right(posts.filter(p => p.channel == channel && p.thread == thread && p.tag == tag).map(_.ts))

  def react(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] = {
    reactions = reactions + ((channel, ts, emoji))
    Right(())
  }

  def unreact(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] = {
    reactions = reactions - ((channel, ts, emoji))
    Right(())
  }

  def name(user: UserId): Either[SlackError, Option[String]] = Right(names.get(user))

  def public(channel: ChannelId): Either[SlackError, Boolean] = Right(
    !privateChannels.contains(channel)
  )
}
