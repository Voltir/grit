package grit.slack.event

/** Events API payloads in the shape Slack's docs give for `app_mention` and
  * `message.channels`, wrapped in an event callback: what a Socket Mode envelope's `payload`
  * holds. Shared by the tests of what reads them and the fake that delivers them.
  */
object Payloads {

  val Team = "T123ABC456"

  /** grit's bot user. */
  val Bot = "U0LAN0Z89"

  /** A person. */
  val Ana = "U061F7AUR"

  /** The response url of [[command]]'s payloads. */
  val Hook = "https://hooks.slack.com/commands/T123ABC456/1234567890/abcdefXYZ"

  /** A slash command `name`, with `text`, asked by `user` in `channel`: the fields Slack's
    * docs give for a slash command's request, as a Socket Mode envelope's `payload` holds
    * them.
    */
  def command(
      text: String,
      name: String = "/grit",
      user: String = Ana,
      channel: String = "C123ABC456"
  ): String =
    ujson
      .Obj(
        "token" -> "XXYYZZ",
        "team_id" -> Team,
        "team_domain" -> "acme",
        "channel_id" -> channel,
        "channel_name" -> (if (channel.startsWith("D")) "directmessage" else "standup"),
        "user_id" -> user,
        "user_name" -> "ana",
        "command" -> name,
        "text" -> text,
        "api_app_id" -> "A123ABC456",
        "is_enterprise_install" -> "false",
        "response_url" -> Hook,
        "trigger_id" -> "13345224609.738474920.8088930838d88f008e0"
      )
      .render()

  private def callback(event: ujson.Obj): String =
    ujson
      .Obj(
        "token" -> "XXYYZZ",
        "team_id" -> Team,
        "api_app_id" -> "A123ABC456",
        "event" -> event,
        "type" -> "event_callback",
        "event_id" -> "Ev123ABC456",
        "event_time" -> 1515449522
      )
      .render()

  /** A mention of grit at the top of a channel. */
  def mention(
      ts: String,
      text: String = s"<@$Bot> is it everything a river should be?",
      user: String = Ana
  ): String =
    callback(
      ujson.Obj(
        "type" -> "app_mention",
        "user" -> user,
        "text" -> text,
        "ts" -> ts,
        "channel" -> "C123ABC456",
        "event_ts" -> ts
      )
    )

  /** A mention of grit inside the thread rooted at `thread`. */
  def mentionIn(
      thread: String,
      ts: String,
      text: String = s"<@$Bot> and this?",
      user: String = Ana
  ): String =
    callback(
      ujson.Obj(
        "type" -> "app_mention",
        "user" -> user,
        "text" -> text,
        "ts" -> ts,
        "thread_ts" -> thread,
        "channel" -> "C123ABC456",
        "event_ts" -> ts
      )
    )

  /** A person's message in a public channel, in the thread rooted at `thread` when given. */
  def message(
      ts: String,
      text: String,
      thread: Option[String] = None,
      user: String = Ana,
      extra: Seq[(String, ujson.Value)] = Nil
  ): String = {
    val event = ujson.Obj(
      "type" -> "message",
      "channel" -> "C123ABC456",
      "user" -> user,
      "text" -> text,
      "ts" -> ts,
      "channel_type" -> "channel"
    )
    thread.foreach(t => event("thread_ts") = t)
    extra.foreach((k, v) => event(k) = v)
    callback(event)
  }

  /** grit's direct-message channel with [[Ana]]. */
  val AnasDm = "D0ANA00001"

  /** A person's message in their direct message with grit (`message.im`), in the thread rooted
    * at `thread` when given, by `user` of the team `userTeam` names when given.
    */
  def direct(
      ts: String,
      text: String,
      thread: Option[String] = None,
      user: String = Ana,
      userTeam: Option[String] = None,
      extra: Seq[(String, ujson.Value)] = Nil
  ): String = {
    val event = ujson.Obj(
      "type" -> "message",
      "channel" -> AnasDm,
      "user" -> user,
      "text" -> text,
      "ts" -> ts,
      "channel_type" -> "im"
    )
    thread.foreach(t => event("thread_ts") = t)
    userTeam.foreach(t => event("user_team") = t)
    extra.foreach((k, v) => event(k) = v)
    callback(event)
  }

  /** A post grit's bot made with its bot token at the top level of `channel`
    * (`chat.postMessage`, as `slack_post` makes one), as Slack delivers it: by grit's bot user,
    * with the bot's and the app's ids and no subtype; as an `app_mention` when `mention`.
    */
  def botPost(
      ts: String,
      text: String,
      mention: Boolean,
      channel: String = "C123ABC456"
  ): String = {
    val event = ujson.Obj(
      "type" -> (if (mention) "app_mention" else "message"),
      "user" -> Bot,
      "bot_id" -> "B0LAN0Z89",
      "app_id" -> "A123ABC456",
      "text" -> text,
      "ts" -> ts,
      "channel" -> channel,
      "event_ts" -> ts
    )
    if (!mention) event("channel_type") = "channel"
    callback(event)
  }

  /** `user` adding `emoji` to message `ts` of `channel` (or removing it, when not `added`),
    * as Slack's docs give `reaction_added` and `reaction_removed`, the event at `at`.
    */
  def reaction(
      ts: String,
      emoji: String,
      added: Boolean = true,
      user: String = Ana,
      channel: String = "C123ABC456",
      at: String = "1515449600.000100"
  ): String =
    callback(
      ujson.Obj(
        "type" -> (if (added) "reaction_added" else "reaction_removed"),
        "user" -> user,
        "reaction" -> emoji,
        "item_user" -> Bot,
        "item" -> ujson.Obj(
          "type" -> "message",
          "channel" -> channel,
          "ts" -> ts,
          "channel_type" -> "group"
        ),
        "event_ts" -> at
      )
    )

  /** Slack telling grit that `user` changed, as its docs give `user_change`: the user object,
    * here deactivated, with their team's id when `team` is given.
    */
  def userChange(user: String = Ana, team: Option[String] = Some(Team)): String = {
    val changed = ujson.Obj("id" -> user, "name" -> "ana", "deleted" -> true)
    team.foreach(t => changed("team_id") = t)
    callback(
      ujson.Obj(
        "type" -> "user_change",
        "user" -> changed,
        "cache_ts" -> 1515449522,
        "event_ts" -> "1515449522.000100"
      )
    )
  }

  /** `user` adding `emoji` to a file, as Slack's docs give a `reaction_added` on one. */
  def fileReaction(emoji: String, user: String = Ana): String =
    callback(
      ujson.Obj(
        "type" -> "reaction_added",
        "user" -> user,
        "reaction" -> emoji,
        "item" -> ujson.Obj("type" -> "file", "file" -> "F123ABC456"),
        "event_ts" -> "1515449600.000100"
      )
    )
}
