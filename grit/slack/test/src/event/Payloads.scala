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
}
