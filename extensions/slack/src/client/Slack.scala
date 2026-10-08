package grit.slack.client

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.identity.Standing
import grit.slack.event.{ChannelId, Listed, ResponseUrl, TeamId, Ts, UserId}
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

  /** A post `slack_post` made for the request at slot `request` ([[grit.core.id.CallSlot.key]]). */
  case Sent(request: String)

  /** A review's prompt for the heard message whose entry id is `entry`. */
  case Prompt(entry: String)
}

/** A Slack user as [[Slack.member]] and [[Slack.members]] read them: the name they show, if
  * any, and their standing in the team asked about, as [[Slack.member]] decides it.
  */
final case class Member(name: Option[String], standing: Standing)

/** The message a thread begins with, as [[Slack.root]] reads it: who wrote it (`None` for one
  * with no user), the [[Tag]] grit posted it with, if any, and its text as Slack gives it,
  * escaped as Slack escapes it.
  */
final case class Root(user: Option[UserId], tag: Option[Tag], text: String)

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

  /** Posts `post` as a reply in `thread` of `channel`, carrying `tag`; the new message's ts.
    * `Refused("not_in_channel")` when grit's bot is not a member.
    */
  def post(channel: ChannelId, thread: Ts, post: Post, tag: Tag): Either[SlackError, Ts]

  /** Posts `post` at `channel`'s top level, carrying `tag`; the new message's ts, which is
    * also the thread it starts. `Refused("not_in_channel")` when grit's bot is not a member.
    */
  def postTopLevel(channel: ChannelId, post: Post, tag: Tag): Either[SlackError, Ts]

  /** The messages in `thread` of `channel` that carry `tag`, oldest first;
    * `Refused("thread_not_found")` when no message there has the ts `thread`.
    */
  def tagged(channel: ChannelId, thread: Ts, tag: Tag): Either[SlackError, Vector[Ts]]

  /** The message `thread` of `channel` begins with; `None` when there is none. */
  def root(channel: ChannelId, thread: Ts): Either[SlackError, Option[Root]]

  /** The link to message `ts` of `channel` that opens it in Slack. `Refused("message_not_found")`
    * when there is no such message, `Refused("channel_not_found")` when grit cannot see the
    * channel.
    */
  def permalink(channel: ChannelId, ts: Ts): Either[SlackError, String]

  /** Adds grit's `emoji` reaction to message `ts`; one already there is not an error.
    * `Refused("message_not_found")` when there is no such message.
    */
  def react(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit]

  /** Removes grit's `emoji` reaction from message `ts`; one already gone is not an error. */
  def unreact(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit]

  /** `user` as a [[Member]] of `team` (`users.info`): the name they show, their display name
    * else their real name; and [[Standing.Full]] when they are a full member of `team` (of that
    * team, and not a guest, a member of another organisation, a bot or an app, invited or
    * deactivated), with the email Slack verified for them when the address is confirmed and the
    * app may read emails (`users:read.email`), whatever its domain; [[Standing.Outside]]
    * otherwise, and when Slack knows no such user (`user_not_found`). `Left` only when Slack
    * could not be asked: a rate limit, a scope or token it refuses, the network.
    */
  def member(team: TeamId, user: UserId): Either[SlackError, Member]

  /** Every user Slack lists in grit's workspace (`users.list`), each as [[member]] reads one in
    * `team`, read whole, every page; a rate limit is waited out, up to 5 times a page, as in
    * [[history]]. `Left` when any page could not be read, never part of the listing.
    */
  def members(team: TeamId): Either[SlackError, Map[UserId, Member]]

  /** What `channel` is to grit ([[ChannelKind]]); Slack being unreachable is still
    * `Unreachable`.
    */
  def kind(channel: ChannelId): Either[SlackError, ChannelKind]

  /** Every message in `channel` at or after `since`, to the microsecond, with every reply in a
    * thread rooted in that span, oldest first, each once; a reply in a thread rooted earlier
    * only when it was also sent to the channel. A rate limit is waited out, up to 5 times a call, before
    * it is `Limited`; `Refused("not_in_channel")` when grit's bot is not in the channel.
    */
  def history(channel: ChannelId, since: Instant): Either[SlackError, Vector[Listed]]

  /** Answers the slash command whose answers go to `url`, seen by its asker alone, in `text` as
    * written: nothing in it becomes a mention, a link or markup. Slack takes up to 5 answers at
    * a url, for 30 minutes after its command; one past either is `Refused` with Slack's word
    * for it (`expired_url`).
    */
  def respond(url: ResponseUrl, text: String): Either[SlackError, Unit]

  /** Stops listening and disconnects; nothing is asked of it after. */
  def close(): Unit

  /** The name `channel` shows in Slack, without its `#`; `None` for a conversation with none,
    * one grit may not look at, or none at all; Slack being unreachable is still `Unreachable`.
    */
  def channelName(channel: ChannelId): Either[SlackError, Option[String]]
}

/** What a conversation is, as Slack reports it to grit's bot. */
enum ChannelKind {

  /** A public channel. */
  case Public

  /** A private channel grit's bot is in. */
  case Private

  /** No channel grit can see: a direct message or a group one, a private channel grit's bot
    * is not in, or no conversation at all.
    */
  case Unseen
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
