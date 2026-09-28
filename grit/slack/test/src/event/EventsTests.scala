package grit.slack.event

import utest.*

/** [[Events.read]] over payloads shaped as Slack's docs give them. */
object EventsTests extends TestSuite {
  import Payloads.*

  private val bot = UserId(Bot)

  private def said(ts: String, thread: String, text: String, mentions: Boolean): Event =
    Event.Said(
      TeamId(Team),
      ChannelId("C123ABC456"),
      Ts(ts),
      Ts(thread),
      UserId(Ana),
      text,
      mentions
    )

  val tests = Tests {
    test("a top-level mention is said by its author, in the thread it starts, mentioning grit") {
      Events.read(mention("1515449522.000016"), bot) ==> Right(
        said(
          "1515449522.000016",
          "1515449522.000016",
          s"<@$Bot> is it everything a river should be?",
          true
        )
      )
    }

    test("a mention in a thread is in that thread") {
      Events.read(mentionIn("1.0", "1.5"), bot) ==> Right(
        said("1.5", "1.0", s"<@$Bot> and this?", true)
      )
    }

    test("a message is said, mentioning grit only when its text does") {
      Events.read(message("2.0", "just chatting", Some("1.0")), bot) ==>
        Right(said("2.0", "1.0", "just chatting", false))
      Events.read(message("2.1", s"hey <@$Bot>"), bot) ==> Right(
        said("2.1", "2.1", s"hey <@$Bot>", true)
      )
    }

    test("a thread reply broadcast to the channel, or sharing a file, is said too") {
      Events.read(
        message("3.0", "also", Some("1.0"), extra = Seq("subtype" -> "thread_broadcast")),
        bot
      ) ==>
        Right(said("3.0", "1.0", "also", false))
      Events.read(message("3.1", "see file", extra = Seq("subtype" -> "file_share")), bot) ==>
        Right(said("3.1", "3.1", "see file", false))
    }

    test("grit's own messages, other bots', edits, and messages outside a channel are ignored") {
      Events.read(message("4.0", "reply", user = Bot), bot) ==> Right(
        Event.Ignored("a bot's message")
      )
      Events.read(message("4.1", "beep", extra = Seq("bot_id" -> "B1")), bot) ==>
        Right(Event.Ignored("a bot's message"))
      Events.read(message("4.2", "x", extra = Seq("subtype" -> "message_changed")), bot) ==>
        Right(Event.Ignored("a message's message_changed"))
      Events.read(message("4.3", "x", extra = Seq("channel_type" -> "group")), bot) ==>
        Right(Event.Ignored("a message in a group"))
    }

    test(
      "another event type is ignored; a payload that is not an event callback, or lacks a field, is not read"
    ) {
      Events.read(
        ujson
          .Obj(
            "type" -> "event_callback",
            "team_id" -> Team,
            "event" -> ujson.Obj("type" -> "reaction_added")
          )
          .render(),
        bot
      ) ==> Right(Event.Ignored("an event of type reaction_added"))
      Events.read(ujson.Obj("type" -> "url_verification").render(), bot) ==>
        Left("not an event callback: url_verification")
      Events.read(
        ujson
          .Obj(
            "type" -> "event_callback",
            "team_id" -> Team,
            "event" -> ujson.Obj("type" -> "app_mention")
          )
          .render(),
        bot
      ) ==> Left("an app_mention event without user")
    }
  }
}
