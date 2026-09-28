package grit.slack.client

import scala.concurrent.duration.FiniteDuration

import grit.slack.event.{ChannelId, TeamId, Ts, UserId}
import grit.slack.text.Post

/** Slack's bot token, `xoxb-…`. It shows only as `BotToken(****)`. */
final class BotToken private (private[client] val value: String) {
  override def toString: String = "BotToken(****)"
}

object BotToken {

  /** `raw`, trimmed, as a bot token; why not, never quoting it, when it does not start
    * `xoxb-`.
    */
  def of(raw: String): Either[String, BotToken] =
    Option(raw.trim)
      .filter(_.startsWith("xoxb-"))
      .map(new BotToken(_))
      .toRight("a bot token starts xoxb-")
}

/** Slack's app-level token, `xapp-…`, which opens Socket Mode. It shows only as
  * `AppToken(****)`.
  */
final class AppToken private (private[client] val value: String) {
  override def toString: String = "AppToken(****)"
}

object AppToken {

  /** `raw`, trimmed, as an app-level token; why not, never quoting it, when it does not start
    * `xapp-`.
    */
  def of(raw: String): Either[String, AppToken] =
    Option(raw.trim)
      .filter(_.startsWith("xapp-"))
      .map(new AppToken(_))
      .toRight("an app-level token starts xapp-")
}

/** Who grit is in its workspace: the team, and grit's bot user. */
final case class Self(team: TeamId, bot: UserId)

/** What a post of grit's carries (Slack message metadata), to be found again. */
enum Tag {

  /** Part `part`, from 0, of the reply of the turn whose workflow id is `turn`. */
  case Reply(turn: String, part: Int)

  /** The line telling the person who wrote message `message` that it was not taken. */
  case Refused(message: Ts)
}

/** Slack as grit uses it: one workspace, through grit's bot. */
trait Slack extends caps.SharedCapability {

  /** Who grit is. */
  def self(): Either[SlackError, Self]

  /** Hands each Events API payload to `handle`, possibly several at once on the client's
    * threads, and acknowledges it only once `handle` returns `true`; `false` leaves it
    * unacknowledged, and Slack delivers it again, a few times at most. Returns once connected,
    * and reconnects by itself until closed.
    */
  def listen(handle: String => Boolean): Either[SlackError, Unit]

  /** Posts `post` as a reply in `thread` of `channel`, carrying `tag`; the new message's ts. */
  def post(channel: ChannelId, thread: Ts, post: Post, tag: Tag): Either[SlackError, Ts]

  /** The messages in `thread` of `channel` that carry `tag`, oldest first. */
  def tagged(channel: ChannelId, thread: Ts, tag: Tag): Either[SlackError, Vector[Ts]]

  /** Adds grit's `emoji` reaction to message `ts`; one already there is not an error. */
  def react(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit]

  /** Removes grit's `emoji` reaction from message `ts`; one already gone is not an error. */
  def unreact(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit]

  /** The name `user` shows in Slack: their display name, else their real name; `None` when
    * they have neither.
    */
  def name(user: UserId): Either[SlackError, Option[String]]

  /** Whether `channel` is a public channel. `false` for any other conversation, and for one
    * grit may not look at.
    */
  def public(channel: ChannelId): Either[SlackError, Boolean]
}

/** A Slack call that did not do what was asked. */
enum SlackError {

  /** Slack answered with `error`, its code (`channel_not_found`, `not_in_channel`, …). */
  case Refused(error: String)

  /** Rate-limited: try again after `after`. */
  case Limited(after: FiniteDuration)

  /** Slack could not be reached, or its answer could not be read. */
  case Unreachable(cause: String)
}
