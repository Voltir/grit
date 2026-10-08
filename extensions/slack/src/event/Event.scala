package grit.slack.event

import java.time.Instant

/** What Slack told grit, as grit acts on it. */
enum Event {

  /** A person's message `ts` in `channel` of `team`, the workspace grit's app is installed in,
    * in the thread rooted at `thread` (its own `ts` when it is in none), by `user` of their own
    * team `author` (another workspace's in a channel shared with it), with `text` as Slack
    * sent it (mrkdwn escapes and `<@U…>` mentions, [[grit.slack.text.Incoming]]); `mentions`
    * when it mentions grit; said `at`, the time its `ts` names.
    */
  case Said(
      team: TeamId,
      author: TeamId,
      channel: ChannelId,
      ts: Ts,
      thread: Ts,
      user: UserId,
      text: String,
      mentions: Boolean,
      at: Instant
  )

  /** A person's direct message to grit (`channel_type` `im`): `ts` in the direct-message
    * channel `channel` of `team`, in the thread rooted at `thread` (its own `ts` when in none),
    * by `user` of their own team `author` (the event's `user_team`, else `team`), with `text`
    * as Slack sent it, said `at`. Always addressed to grit.
    */
  case Told(
      team: TeamId,
      author: TeamId,
      channel: ChannelId,
      ts: Ts,
      thread: Ts,
      user: UserId,
      text: String,
      at: Instant
  )

  /** `user`, anyone's (grit's bot included), `added` the reaction `emoji` to message `ts` of
    * `channel` in `team`, or removed it when not: `emoji` as Slack names it, without colons,
    * a skin tone after `::` (`+1::skin-tone-2`); `at`, the time the event's `event_ts` names.
    */
  case Reacted(
      team: TeamId,
      channel: ChannelId,
      ts: Ts,
      user: UserId,
      emoji: String,
      added: Boolean,
      at: Instant
  )

  /** Slack says something about `user` of `team`, the team they are of, changed: their
    * profile, their standing, or their account deactivated (`user_change`). What changed is not
    * read from the event: Slack is asked again.
    */
  case UserChanged(team: TeamId, user: UserId)

  /** Something grit does not act on, and why: a bot's message (grit's own included), an
    * edit or another change to a message, a message outside a channel or a direct message with
    * grit (a group direct message included), a reaction to anything but a message, an event of
    * another type.
    */
  case Ignored(why: String)
}

/** A message as a channel's history lists it, before grit reads it ([[Events.listed]]): its
  * `ts`, the thread it is in (`None` when it is in none), who wrote it (`None` for a message
  * with no user, as some bots' and Slack's own are), whether a bot sent it, its `subtype`, its
  * text as Slack sent it, and the team its author is of, when the listing says.
  */
final case class Listed(
    ts: Ts,
    thread: Option[Ts],
    user: Option[UserId],
    bot: Boolean,
    subtype: Option[String],
    text: String,
    team: Option[TeamId] = None
)

object Events {

  /** `m`, listed in `channel` of `team`, read by [[read]]'s rules for a `message` event in a
    * channel, grit's own bot user being `bot`, its author of the team it names, else of `team`;
    * why not, as [[read]] says.
    */
  def listed(m: Listed, team: TeamId, channel: ChannelId, bot: UserId): Either[String, Event] = {
    // The live event this listing would have been, so both are read by one set of rules.
    val event = ujson.Obj(
      "type" -> "message",
      "channel" -> ChannelId.value(channel),
      "channel_type" -> "channel",
      "ts" -> Ts.value(m.ts),
      "text" -> m.text
    )
    m.thread.foreach(t => event("thread_ts") = Ts.value(t))
    m.user.foreach(u => event("user") = UserId.value(u))
    m.subtype.foreach(st => event("subtype") = st)
    m.team.foreach(t => event("team") = TeamId.value(t))
    if (m.bot) event("bot_id") = "listed"
    said(event, team, bot)
  }

  /** The event an Events API payload (a Socket Mode envelope's `payload`) carries, grit's own
    * bot user being `bot`: `app_mention` and `message` events as [[Event.Said]] (a `message`
    * only from a person, in a channel, public or private, new or broadcast from a thread, or sharing a file),
    * a person's `message` in a direct message with grit (`im`) as [[Event.Told]], one in a group
    * direct message (`mpim`) [[Event.Ignored]],
    * `reaction_added` and `reaction_removed` on a message as [[Event.Reacted]], `user_change`
    * as [[Event.UserChanged]], everything else [[Event.Ignored]]. A message's author is of the team its `user_team` names, else its
    * `team`, else the callback's `team_id`, the workspace grit's app is installed in. Why not, when it is not an event callback, or an event
    * grit reads lacks a field it needs or has a ts or event_ts that names no time.
    */
  def read(payload: String, bot: UserId): Either[String, Event] =
    scala.util
      .Try(ujson.read(payload))
      .toEither
      .left
      .map(e => s"not JSON: ${e.getMessage}")
      .flatMap { outer =>
        str(outer, "type") match {
          case Some("event_callback") =>
            for {
              team <- str(outer, "team_id").toRight("an event callback without team_id")
              event <- outer.obj.get("event").toRight("an event callback without an event")
              read <- said(event, TeamId(team), bot)
            } yield read
          case other => Left(s"not an event callback: ${other.getOrElse("no type")}")
        }
      }

  /** The slash command a Socket Mode envelope's `payload` carries, as Slack's form fields:
    * `command`, `text` (none is no words), `team_id`, `channel_id`, `user_id` and
    * `response_url`. Why not, naming the first field it lacks, or when it is not JSON.
    */
  def command(payload: String): Either[String, Commanded] =
    scala.util
      .Try(ujson.read(payload))
      .toEither
      .left
      .map(e => s"not JSON: ${e.getMessage}")
      .flatMap { p =>
        def field(name: String): Either[String, String] =
          str(p, name).toRight(s"a slash command without $name")
        for {
          command <- field("command")
          team <- field("team_id")
          channel <- field("channel_id")
          user <- field("user_id")
          url <- field("response_url")
        } yield Commanded(
          TeamId(team),
          ChannelId(channel),
          UserId(user),
          command,
          str(p, "text").getOrElse(""),
          ResponseUrl(url)
        )
      }

  /** The `channel_type`s of a `message` in a channel: a public one, and a private one. Any
    * other kind (`im`, `mpim`, `app_home`, one Slack adds later) is not a channel's.
    */
  private val Channels = Set("channel", "group")

  /** The subtypes of a `message` that are a person saying something new. */
  private val Spoken = Set("thread_broadcast", "file_share")

  private def said(event: ujson.Value, team: TeamId, bot: UserId): Either[String, Event] =
    str(event, "type") match {
      case Some("app_mention") => message(event, team, bot, mention = true)
      case Some(kind @ ("reaction_added" | "reaction_removed")) => reaction(event, team, kind)
      case Some("user_change") => userChanged(event, team)
      case Some("message") =>
        (str(event, "subtype"), str(event, "bot_id"), str(event, "channel_type")) match {
          case (Some(sub), _, _) if !Spoken.contains(sub) =>
            Right(Event.Ignored(s"a message's $sub"))
          case (_, Some(_), _) => Right(Event.Ignored("a bot's message"))
          case (_, _, Some("im")) =>
            message(event, team, bot, mention = false).map {
              case m: Event.Said =>
                Event.Told(m.team, m.author, m.channel, m.ts, m.thread, m.user, m.text, m.at)
              case other => other
            }
          case (_, _, Some("mpim")) => Right(Event.Ignored("a group direct message is not heard"))
          case (_, _, Some(kind)) if !Channels.contains(kind) =>
            Right(Event.Ignored(s"a message outside a channel ($kind)"))
          case _ => message(event, team, bot, mention = false)
        }
      case other => Right(Event.Ignored(s"an event of type ${other.getOrElse("none")}"))
    }

  private def message(
      event: ujson.Value,
      team: TeamId,
      bot: UserId,
      mention: Boolean
  ): Either[String, Event] = {
    val (article, kind) = if (mention) ("an", "app_mention") else ("a", "message")
    def field(name: String): Either[String, String] =
      str(event, name).toRight(s"$article $kind event without $name")
    for {
      user <- field("user")
      channel <- field("channel")
      ts <- field("ts")
      at <- time(ts).toRight(s"$article $kind event whose ts names no time: $ts")
      text <- field("text")
    } yield
      if (UserId(user) == bot || str(event, "bot_id").nonEmpty) Event.Ignored("a bot's message")
      else
        Event.Said(
          team,
          str(event, "user_team").orElse(str(event, "team")).fold(team)(TeamId(_)),
          ChannelId(channel),
          Ts(ts),
          Ts(str(event, "thread_ts").getOrElse(ts)),
          UserId(user),
          text,
          mention || text.contains(s"<@${UserId.value(bot)}>"),
          at
        )
  }

  /** A `reaction_added` or `reaction_removed` event, `kind`, as [[Event.Reacted]] when its item
    * is a message.
    */
  private def reaction(event: ujson.Value, team: TeamId, kind: String): Either[String, Event] = {
    val item = event.objOpt.flatMap(_.get("item"))
    def field(from: Option[ujson.Value], name: String): Either[String, String] =
      from.flatMap(str(_, name)).toRight(s"a $kind event without $name")
    item.flatMap(str(_, "type")) match {
      case Some("message") =>
        for {
          user <- field(Some(event), "user")
          emoji <- field(Some(event), "reaction")
          channel <- field(item, "channel")
          ts <- field(item, "ts")
          stamp <- field(Some(event), "event_ts")
          at <- time(stamp).toRight(s"a $kind event whose event_ts names no time: $stamp")
        } yield Event.Reacted(
          team,
          ChannelId(channel),
          Ts(ts),
          UserId(user),
          emoji,
          kind == "reaction_added",
          at
        )
      case other => Right(Event.Ignored(s"a reaction to a ${other.getOrElse("nothing")}"))
    }
  }

  /** A `user_change` event as [[Event.UserChanged]], the user of the team their `team_id`
    * names, else of `team`.
    */
  private def userChanged(event: ujson.Value, team: TeamId): Either[String, Event] = {
    val user = event.objOpt.flatMap(_.get("user"))
    user
      .flatMap(str(_, "id"))
      .toRight("a user_change event without user.id")
      .map(id =>
        Event.UserChanged(user.flatMap(str(_, "team_id")).fold(team)(TeamId(_)), UserId(id))
      )
  }

  /** The time a ts names: seconds since the epoch, a point, then up to nine digits of the
    * second.
    */
  private[slack] def time(ts: String): Option[Instant] = ts match {
    case Stamp(seconds, fraction) =>
      seconds.toLongOption.map(s =>
        Instant.ofEpochSecond(s, Option(fraction).fold(0L)(f => (f + "000000000").take(9).toLong))
      )
    case _ => None
  }

  private val Stamp = "([0-9]{1,18})(?:\\.([0-9]{1,9}))?".r

  private def str(v: ujson.Value, key: String): Option[String] =
    v.objOpt.flatMap(_.get(key)).flatMap(_.strOpt)
}
