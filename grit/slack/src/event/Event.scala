package grit.slack.event

/** What Slack told grit, as grit acts on it. */
enum Event {

  /** A person's message `ts` in `channel` of `team`, in the thread rooted at `thread` (its own
    * `ts` when it is in none), by `user`, with `text` as Slack sent it (mrkdwn escapes and
    * `<@U…>` mentions, [[grit.slack.text.Incoming]]); `mentions` when it mentions grit.
    */
  case Said(
      team: TeamId,
      channel: ChannelId,
      ts: Ts,
      thread: Ts,
      user: UserId,
      text: String,
      mentions: Boolean
  )

  /** Something grit does not act on, and why: a bot's message (grit's own included), an
    * edit or another change to a message, a message outside a channel, an event of another
    * type.
    */
  case Ignored(why: String)
}

object Events {

  /** The event an Events API payload (a Socket Mode envelope's `payload`) carries, grit's own
    * bot user being `bot`: `app_mention` and `message` events as [[Event.Said]] (a `message`
    * only from a person, in a channel, new or broadcast from a thread, or sharing a file),
    * everything else [[Event.Ignored]]. Why not, when it is not an event callback, or an event
    * grit reads lacks a field it needs.
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

  /** The subtypes of a `message` that are a person saying something new. */
  private val Spoken = Set("thread_broadcast", "file_share")

  private def said(event: ujson.Value, team: TeamId, bot: UserId): Either[String, Event] =
    str(event, "type") match {
      case Some("app_mention") => message(event, team, bot, mention = true)
      case Some("message") =>
        (str(event, "subtype"), str(event, "bot_id"), str(event, "channel_type")) match {
          case (Some(sub), _, _) if !Spoken.contains(sub) =>
            Right(Event.Ignored(s"a message's $sub"))
          case (_, Some(_), _) => Right(Event.Ignored("a bot's message"))
          case (_, _, Some(kind)) if kind != "channel" =>
            Right(Event.Ignored(s"a message in a $kind"))
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
      text <- field("text")
    } yield
      if (UserId(user) == bot || str(event, "bot_id").nonEmpty) Event.Ignored("a bot's message")
      else
        Event.Said(
          team,
          ChannelId(channel),
          Ts(ts),
          Ts(str(event, "thread_ts").getOrElse(ts)),
          UserId(user),
          text,
          mention || text.contains(s"<@${UserId.value(bot)}>")
        )
  }

  private def str(v: ujson.Value, key: String): Option[String] =
    v.objOpt.flatMap(_.get(key)).flatMap(_.strOpt)
}
