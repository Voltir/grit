package grit.slack.client

import java.time.Instant

import scala.concurrent.duration.Duration

import grit.slack.event.{ChannelId, Event, Events, Listed, Payloads, TeamId, Ts, UserId}
import grit.slack.text.Post

/** A [[Slack]] for tests, keeping what [[Slack]] says in memory and held to it by
  * [[SlackContract]]: the handler `listen` was given ([[deliver]] hands it a payload, as
  * Socket Mode would, and says whether it was acknowledged), the posts made, each with its tag,
  * and grit's reactions. Every post is made at a ts a microsecond after the latest it knows.
  * A message it knows is one listed, posted, or delivered as a person's message; a thread it
  * knows is one such message begins.
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

  /** Who grit is. */
  @caps.unsafe.untrackedCaptures
  var me = Self(TeamId(Payloads.Team), UserId(Payloads.Bot))

  /** The people in the workspace, each with the name they show in Slack, if any; anyone else
    * is no such user.
    */
  @caps.unsafe.untrackedCaptures
  var names: Map[UserId, Option[String]] = Map(UserId(Payloads.Ana) -> Some("Ana Lima"))

  /** The channels there are, each with the name it shows in Slack; any other does not exist. */
  @caps.unsafe.untrackedCaptures
  var channelNames = Map(ChannelId("C123ABC456") -> "standup")

  /** The channels Slack cannot be asked about. */
  @caps.unsafe.untrackedCaptures
  var unreachable = Set.empty[ChannelId]

  /** The threads whose root Slack cannot be asked for. */
  @caps.unsafe.untrackedCaptures
  var rootless = Set.empty[Ts]

  /** The channels that are not public. */
  @caps.unsafe.untrackedCaptures
  var privateChannels = Set.empty[ChannelId]

  /** The channels grit's bot is not a member of. */
  @caps.unsafe.untrackedCaptures
  var notIn = Set.empty[ChannelId]

  /** Each channel's messages as Slack lists them, in any order, a message possibly more than
    * once (in the channel's history and in its thread's replies).
    */
  @caps.unsafe.untrackedCaptures
  var histories: Map[ChannelId, Vector[Listed]] = Map.empty

  /** How many of the next Web API requests are answered rate-limited, 0 s to wait. Counted
    * across calls; `history` waits out up to [[SocketSlack.Retries]] in a row, each spending
    * one, as [[SocketSlack]] does.
    */
  @caps.unsafe.untrackedCaptures
  var limited = 0

  /** When set, every call but `listen` fails as Slack being unreachable would. */
  @caps.unsafe.untrackedCaptures
  var down = false

  /** The messages delivered as a person's message: their own ts, and their thread's. */
  @caps.unsafe.untrackedCaptures
  var delivered = Set.empty[(ChannelId, Ts)]

  /** Whether it was closed. */
  @caps.unsafe.untrackedCaptures
  var closed = false

  def close(): Unit = closed = true

  /** Hands `payload` to the listening handler; whether it was acknowledged. */
  def deliver(payload: String): Boolean = {
    Events.read(payload, me.bot) match {
      case Right(said: Event.Said) =>
        delivered = delivered + ((said.channel, said.ts)) + ((said.channel, said.thread))
      case _ => ()
    }
    handler.exists(_(payload))
  }

  /** Whether `ts` is a message in `channel` it knows. */
  private def knows(channel: ChannelId, ts: Ts): Boolean =
    histories.getOrElse(channel, Vector.empty).exists(l => l.ts == ts || l.thread.contains(ts)) ||
      posts.exists(p => p.channel == channel && (p.ts == ts || p.thread == ts)) ||
      delivered.contains((channel, ts))

  /** `body`, as one Web API request is answered. */
  private def request[A](body: => Either[SlackError, A]): Either[SlackError, A] =
    if (down) Left(SlackError.Unreachable("down"))
    else if (limited > 0) {
      limited -= 1
      Left(SlackError.Limited(Duration.Zero))
    } else body

  def self(): Either[SlackError, Self] = request(Right(me))

  def listen(handle: String => Boolean): Either[SlackError, Unit] = {
    // Kept past the call, as Socket Mode keeps its listener: the test that made this fake
    // is the only caller of deliver, and it outlives neither the handler nor the fake.
    handler = Some(caps.unsafe.unsafeAssumePure(handle))
    Right(())
  }

  def post(channel: ChannelId, thread: Ts, post: Post, tag: Tag): Either[SlackError, Ts] =
    request {
      if (notIn.contains(channel)) Left(SlackError.Refused("not_in_channel"))
      else {
        val ts = next()
        posts = posts :+ Posted(channel, thread, post, tag, ts)
        Right(ts)
      }
    }

  /** Kept as a post whose thread is its own ts, as Slack lists a message no one replied to. */
  def postTopLevel(channel: ChannelId, post: Post, tag: Tag): Either[SlackError, Ts] =
    request {
      if (notIn.contains(channel)) Left(SlackError.Refused("not_in_channel"))
      else {
        val ts = next()
        posts = posts :+ Posted(channel, ts, post, tag, ts)
        Right(ts)
      }
    }

  /** A microsecond after the latest ts it knows. */
  private def next(): Ts = {
    val known = histories.values.flatten.map(_.ts) ++ posts.map(_.ts)
    val latest = known.flatMap(ts => scala.util.Try(BigDecimal(Ts.value(ts))).toOption).maxOption
    Ts((latest.getOrElse(BigDecimal(0)) + FakeSlack.Micro).setScale(6).toString)
  }

  def tagged(channel: ChannelId, thread: Ts, tag: Tag): Either[SlackError, Vector[Ts]] =
    request {
      if (!knows(channel, thread)) Left(SlackError.Refused("thread_not_found"))
      else
        Right(
          posts.filter(p => p.channel == channel && p.thread == thread && p.tag == tag).map(_.ts)
        )
    }

  /** A post of grit's whose ts is `thread`, as grit's bot's with its tag and plain text; else
    * a listed message whose ts it is, with no tag.
    */
  def root(channel: ChannelId, thread: Ts): Either[SlackError, Option[Root]] =
    request {
      if (rootless.contains(thread)) Left(SlackError.Unreachable("gone"))
      else {
        val posted = posts
          .find(p => p.channel == channel && p.ts == thread)
          .map(p => Root(Some(me.bot), Some(p.tag), p.post.fallback))
        val listed = histories
          .getOrElse(channel, Vector.empty)
          .find(_.ts == thread)
          .map(l => Root(l.user, None, l.text))
        Right(posted.orElse(listed))
      }
    }

  def react(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] =
    request {
      if (!knows(channel, ts)) Left(SlackError.Refused("message_not_found"))
      else {
        reactions = reactions + ((channel, ts, emoji))
        Right(())
      }
    }

  def unreact(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] =
    request {
      reactions = reactions - ((channel, ts, emoji))
      Right(())
    }

  def name(user: UserId): Either[SlackError, Option[String]] =
    request(names.get(user).toRight(SlackError.Refused("user_not_found")))

  def history(channel: ChannelId, since: Instant): Either[SlackError, Vector[Listed]] = {
    @scala.annotation.tailrec
    def patient(left: Int): Either[SlackError, Unit] = request(Right(())) match {
      case Left(SlackError.Limited(_)) if left > 0 => patient(left - 1)
      case other => other
    }
    val from = BigDecimal(SocketSlack.oldest(since))
    def at(ts: Ts): BigDecimal = BigDecimal(Ts.value(ts))
    patient(SocketSlack.Retries).flatMap { _ =>
      if (unreachable.contains(channel)) Left(SlackError.Unreachable("gone"))
      else if (notIn.contains(channel)) Left(SlackError.Refused("not_in_channel"))
      else
        Right(
          histories
            .getOrElse(channel, Vector.empty)
            .filter { m =>
              val root = m.thread.getOrElse(m.ts)
              at(m.ts) >= from &&
              (at(root) >= from || m.subtype.contains("thread_broadcast"))
            }
            .distinctBy(_.ts)
            .sortBy(m => at(m.ts))
        )
    }
  }

  def channelName(channel: ChannelId): Either[SlackError, Option[String]] =
    request {
      if (unreachable.contains(channel)) Left(SlackError.Unreachable("gone"))
      else Right(channelNames.get(channel))
    }

  def public(channel: ChannelId): Either[SlackError, Boolean] =
    request {
      if (unreachable.contains(channel)) Left(SlackError.Unreachable("gone"))
      else Right(channelNames.contains(channel) && !privateChannels.contains(channel))
    }
}

object FakeSlack {

  /** One microsecond, as a ts counts it. */
  private val Micro = BigDecimal("0.000001")
}
