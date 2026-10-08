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
        case Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) | Event.UserChanged(_, _) |
            Event.Told(_, _, _, _, _, _, _, _) | Event.Joined(_, _, _, _) | Event.Left(_, _, _) =>
          None
      } ==> Right(Some(Instant.parse("2018-01-08T22:12:02.000016Z")))
    }

    test(
      "a message's author is of their own team: its user_team, else its team, else the workspace's"
    ) {
      def author(extra: (String, ujson.Value)*): Either[String, Option[TeamId]] =
        Events.read(message("2.0", "hi", extra = extra), bot).map {
          case m: Event.Said => Some(m.author)
          case Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) | Event.UserChanged(_, _) |
              Event.Told(_, _, _, _, _, _, _, _) | Event.Joined(_, _, _, _) | Event.Left(_, _, _) =>
            None
        }
      (
        author("user_team" -> "T0THEIRS", "team" -> "T0OTHER"),
        author("team" -> "T0OTHER"),
        author()
      ) ==> (
        Right(Some(TeamId("T0THEIRS"))),
        Right(Some(TeamId("T0OTHER"))),
        Right(Some(TeamId(Team)))
      )
    }

    test(
      "a user's change names the user, of the team its user object names, else the workspace's"
    ) {
      (
        Events.read(userChange(team = Some("T0THEIRS")), bot),
        Events.read(userChange(team = None), bot)
      ) ==> (
        Right(Event.UserChanged(TeamId("T0THEIRS"), UserId(Ana))),
        Right(Event.UserChanged(TeamId(Team), UserId(Ana)))
      )
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
      "a person's direct message is told, in the thread it is in, by its author of the team user_team names, else the callback's"
    ) {
      (
        Events.read(direct("9.0", "hi grit"), bot),
        Events.read(direct("9.2", "more", Some("9.0"), userTeam = Some("T0OTHER01")), bot)
      ) ==> (
        Right(
          Event.Told(
            TeamId(Team),
            TeamId(Team),
            ChannelId(AnasDm),
            Ts("9.0"),
            Ts("9.0"),
            UserId(Ana),
            "hi grit",
            Instant.ofEpochSecond(9)
          )
        ),
        Right(
          Event.Told(
            TeamId(Team),
            TeamId("T0OTHER01"),
            ChannelId(AnasDm),
            Ts("9.2"),
            Ts("9.0"),
            UserId(Ana),
            "more",
            Instant.ofEpochSecond(9, 200000000)
          )
        )
      )
    }

    test("grit's own direct message, and a bot's, are ignored, as in a channel") {
      (
        Events.read(direct("9.3", "reply", user = Bot), bot),
        Events.read(direct("9.4", "beep", extra = Seq("bot_id" -> "B1")), bot)
      ) ==> (Right(Event.Ignored("a bot's message")), Right(Event.Ignored("a bot's message")))
    }

    test(
      "grit's own messages, other bots', edits, a group direct message, and messages outside a channel (an app's home) are ignored"
    ) {
      Events.read(message("4.0", "reply", user = Bot), bot) ==> Right(
        Event.Ignored("a bot's message")
      )
      Events.read(message("4.1", "beep", extra = Seq("bot_id" -> "B1")), bot) ==>
        Right(Event.Ignored("a bot's message"))
      Events.read(message("4.2", "x", extra = Seq("subtype" -> "message_changed")), bot) ==>
        Right(Event.Ignored("a message's message_changed"))
      Events.read(message("4.4", "x", extra = Seq("channel_type" -> "mpim")), bot) ==>
        Right(Event.Ignored("a group direct message is not heard"))
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
      Events
        .listed(
          Listed(Ts("7.0"), None, Some(UserId(Ana)), false, None, "hi", Some(TeamId("T0THEIRS"))),
          TeamId(Team),
          ChannelId("C123ABC456"),
          bot
        )
        .map {
          case m: Event.Said => Some((m.team, m.author))
          case Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) | Event.UserChanged(_, _) |
              Event.Told(_, _, _, _, _, _, _, _) | Event.Joined(_, _, _, _) | Event.Left(_, _, _) =>
            None
        } ==> Right(Some((TeamId(Team), TeamId("T0THEIRS"))))
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
      "grit's bot joining a channel is a join, with its inviter, at its event_ts, else the callback's event_time"
    ) {
      val c = ChannelId("C123ABC456")
      (
        Events.read(joined(), bot),
        Events.read(joined(inviter = "", eventTs = None), bot)
      ) ==> (
        Right(
          Event.Joined(
            TeamId(Team),
            c,
            Some(UserId(Ana)),
            Instant.parse("2018-01-08T22:13:20.000100Z")
          )
        ),
        Right(Event.Joined(TeamId(Team), c, None, Instant.ofEpochSecond(1515449522)))
      )
    }

    test(
      "grit's bot leaving a channel, as member_left_channel, channel_left or group_left, is a leave"
    ) {
      val c = ChannelId("C123ABC456")
      val g = ChannelId("G02ELGNBH")
      (
        Events.read(memberLeft(), bot),
        Events.read(botLeft(), bot),
        Events.read(botLeft("group_left", "G02ELGNBH"), bot)
      ) ==> (
        Right(Event.Left(TeamId(Team), c, Instant.parse("2018-01-08T22:15:00.000200Z"))),
        Right(Event.Left(TeamId(Team), c, Instant.ofEpochSecond(1515449522))),
        Right(Event.Left(TeamId(Team), g, Instant.ofEpochSecond(1515449522)))
      )
    }

    test("another member joining or leaving a channel is ignored") {
      (Events.read(joined(user = Ana), bot), Events.read(memberLeft(user = Ana), bot)) ==> (
        Right(Event.Ignored("another member joined a channel")),
        Right(Event.Ignored("another member left a channel"))
      )
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
