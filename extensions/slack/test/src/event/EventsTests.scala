package grit.slack.event

import java.time.Instant

import utest.*

/** [[Events.read]] over payloads shaped as Slack's docs give them. */
object EventsTests extends TestSuite {
  import Payloads.*

  private val bot = UserId(Bot)

  /** Said at `ts`'s whole seconds: every ts below but the first names no fraction. */
  private def said(ts: String, thread: String, text: String, mentions: Boolean): Event =
    Event.Said(
      TeamId(Team),
      ChannelId("C123ABC456"),
      Ts(ts),
      Ts(thread),
      UserId(Ana),
      text,
      mentions,
      Instant.ofEpochSecond(ts.takeWhile(_ != '.').toLong)
    )

  val tests = Tests {
    test("a top-level mention is said by its author, in the thread it starts, mentioning grit") {
      Events.read(mention("1515449522.000000"), bot) ==> Right(
        said(
          "1515449522.000000",
          "1515449522.000000",
          s"<@$Bot> is it everything a river should be?",
          true
        )
      )
    }

    test("a message is said at the time its ts names, to the microsecond") {
      Events.read(message("1515449522.000016", "hi"), bot).map {
        case m: Event.Said => Some(m.at)
        case Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) => None
      } ==> Right(Some(Instant.parse("2018-01-08T22:12:02.000016Z")))
    }

    test("a message whose ts names no time is not read") {
      Events.read(message("yesterday", "hi"), bot) ==>
        Left("a message event whose ts names no time: yesterday")
    }

    test("a mention in a thread is in that thread") {
      Events.read(mentionIn("1.0", "5.0"), bot) ==> Right(
        said("5.0", "1.0", s"<@$Bot> and this?", true)
      )
    }

    test("a message is said, mentioning grit only when its text does") {
      Events.read(message("2.0", "just chatting", Some("1.0")), bot) ==>
        Right(said("2.0", "1.0", "just chatting", false))
      Events.read(message("6.0", s"hey <@$Bot>"), bot) ==> Right(
        said("6.0", "6.0", s"hey <@$Bot>", true)
      )
    }

    test("a thread reply broadcast to the channel, or sharing a file, is said too") {
      Events.read(
        message("3.0", "also", Some("1.0"), extra = Seq("subtype" -> "thread_broadcast")),
        bot
      ) ==>
        Right(said("3.0", "1.0", "also", false))
      Events.read(message("7.0", "see file", extra = Seq("subtype" -> "file_share")), bot) ==>
        Right(said("7.0", "7.0", "see file", false))
    }

    test(
      "a post grit's bot made at a channel's top level is a bot's message, as a message and as a mention of grit"
    ) {
      val text = s"<@$Bot> the build is green"
      Events.read(botPost("8.0", text, mention = false), bot) ==>
        Right(Event.Ignored("a bot's message"))
      Events.read(botPost("8.0", text, mention = true), bot) ==>
        Right(Event.Ignored("a bot's message"))
    }

    test("a message in a private channel is said as one in a public channel is") {
      Events.read(
        message("2.0", "just chatting", Some("1.0"), extra = Seq("channel_type" -> "group")),
        bot
      ) ==>
        Right(said("2.0", "1.0", "just chatting", false))
    }

    test(
      "grit's own messages, other bots', edits, and messages outside a channel (a direct message, a group one, an app's home) are ignored"
    ) {
      Events.read(message("4.0", "reply", user = Bot), bot) ==> Right(
        Event.Ignored("a bot's message")
      )
      Events.read(message("4.1", "beep", extra = Seq("bot_id" -> "B1")), bot) ==>
        Right(Event.Ignored("a bot's message"))
      Events.read(message("4.2", "x", extra = Seq("subtype" -> "message_changed")), bot) ==>
        Right(Event.Ignored("a message's message_changed"))
      Events.read(message("4.3", "x", extra = Seq("channel_type" -> "im")), bot) ==>
        Right(Event.Ignored("a message outside a channel (im)"))
      Events.read(message("4.4", "x", extra = Seq("channel_type" -> "mpim")), bot) ==>
        Right(Event.Ignored("a message outside a channel (mpim)"))
      Events.read(message("4.5", "x", extra = Seq("channel_type" -> "app_home")), bot) ==>
        Right(Event.Ignored("a message outside a channel (app_home)"))
    }

    test(
      "a listed message is read as a live one: bots', grit's and joins ignored, a broadcast and a mention said"
    ) {
      def listed(
          ts: String,
          text: String = "hi",
          thread: Option[String] = None,
          user: Option[String] = Some(Ana),
          fromBot: Boolean = false,
          subtype: Option[String] = None
      ) = Events.listed(
        Listed(Ts(ts), thread.map(Ts(_)), user.map(UserId(_)), fromBot, subtype, text),
        TeamId(Team),
        ChannelId("C123ABC456"),
        bot
      )
      listed("4.0", fromBot = true) ==> Right(Event.Ignored("a bot's message"))
      listed("4.1", user = Some(Bot)) ==> Right(Event.Ignored("a bot's message"))
      listed("4.2", subtype = Some("channel_join")) ==>
        Right(Event.Ignored("a message's channel_join"))
      listed("4.3", user = None) ==> Left("a message event without user")
      listed("3.0", "also", Some("1.0"), subtype = Some("thread_broadcast")) ==>
        Right(said("3.0", "1.0", "also", false))
      listed("6.0", s"hey <@$Bot>") ==> Right(said("6.0", "6.0", s"hey <@$Bot>", true))
    }

    test(
      "a reaction added to a message, or removed, is reacted by its user, at the time its event_ts names"
    ) {
      val at = Instant.ofEpochSecond(1515449600L, 100000L)
      def reacted(emoji: String, added: Boolean, user: String) =
        Event.Reacted(
          TeamId(Team),
          ChannelId("C0REVIEW1"),
          Ts("1515449522.000016"),
          UserId(user),
          emoji,
          added,
          at
        )
      Vector(
        Events.read(reaction("1515449522.000016", "+1", channel = "C0REVIEW1"), bot),
        Events.read(
          reaction("1515449522.000016", "-1::skin-tone-2", added = false, channel = "C0REVIEW1"),
          bot
        ),
        Events.read(reaction("1515449522.000016", "+1", user = Bot, channel = "C0REVIEW1"), bot)
      ) ==> Vector(
        Right(reacted("+1", added = true, Ana)),
        Right(reacted("-1::skin-tone-2", added = false, Ana)),
        Right(reacted("+1", added = true, Bot))
      )
    }

    test(
      "a reaction to a file is ignored; one without its emoji, or whose event_ts names no time, is not read"
    ) {
      Events.read(fileReaction("+1"), bot) ==> Right(Event.Ignored("a reaction to a file"))
      Events.read(reaction("1.0", "+1", at = "soon"), bot) ==>
        Left("a reaction_added event whose event_ts names no time: soon")
      Events.read(
        ujson
          .Obj(
            "type" -> "event_callback",
            "team_id" -> Team,
            "event" -> ujson.Obj(
              "type" -> "reaction_removed",
              "user" -> Ana,
              "item" -> ujson.Obj("type" -> "message", "channel" -> "C1", "ts" -> "1.0"),
              "event_ts" -> "2.0"
            )
          )
          .render(),
        bot
      ) ==> Left("a reaction_removed event without reaction")
    }

    test(
      "another event type is ignored; a payload that is not an event callback, or lacks a field, is not read"
    ) {
      Events.read(
        ujson
          .Obj(
            "type" -> "event_callback",
            "team_id" -> Team,
            "event" -> ujson.Obj("type" -> "emoji_changed")
          )
          .render(),
        bot
      ) ==> Right(Event.Ignored("an event of type emoji_changed"))
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
