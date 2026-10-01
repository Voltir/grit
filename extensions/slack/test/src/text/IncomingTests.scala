package grit.slack.text

import grit.slack.event.{ChannelId, UserId}

import utest.*

/** [[Incoming]]: a person's Slack text as grit stores it. */
object IncomingTests extends TestSuite {

  private val bot = UserId("UBOT")
  private val names: UserId -> Option[String] = Map(UserId("UANA") -> "Ana Lima").get
  private val channels: ChannelId -> Option[String] = Map(ChannelId("C2") -> "ops").get

  val tests = Tests {
    test("grit's own mention is removed, and the text trimmed") {
      Incoming.text("<@UBOT> what are places?", bot, names, channels) ==> "what are places?"
      Incoming.text("so, <@UBOT>, what now", bot, names, channels) ==> "so, , what now"
    }

    test("another person's mention is their name, or their id when it is not known") {
      Incoming.text("ask <@UANA> or <@UZED>", bot, names, channels) ==> "ask @Ana Lima or @UZED"
      Incoming.text("ask <@UZED|zed>", bot, names, channels) ==> "ask @zed"
    }

    test("channels, broadcasts and links are written out; the escapes are undone") {
      Incoming.text("see <#C1|general>", bot, names, channels) ==> "see #general"
      Incoming.text("<!here> <!channel> <!everyone> <!subteam^S1|@devs>", bot, names, channels) ==>
        "@here @channel @everyone @devs"
      Incoming.text(
        "<https://example.com|the docs> or <https://example.com/x>",
        bot,
        names,
        channels
      ) ==>
        "the docs (https://example.com) or https://example.com/x"
      Incoming.text("a &lt; b &amp;&amp; c &gt; d", bot, names, channels) ==> "a < b && c > d"
      // Undone once, `&amp;` last: a person who typed "&lt;" sees it as typed.
      Incoming.text("&amp;lt;", bot, names, channels) ==> "&lt;"
    }

    test("a channel link is its own name, else the name it is known by, else its id") {
      Incoming.text("in <#C2|general>, <#C2|>, <#C2> or <#C3|>", bot, names, channels) ==>
        "in #general, #ops, #ops or #C3"
    }

    test("channels reads each channel link with the name it carries, none when it is empty") {
      Incoming.channels("<#C1|general> <#C2|> <@UANA> <#C3> <https://x.y|z> <#C1|general>") ==>
        Vector(
          ChannelId("C1") -> Some("general"),
          ChannelId("C2") -> None,
          ChannelId("C3") -> None,
          ChannelId("C1") -> Some("general")
        )
    }

    test("a pasted grit label, quoted as Slack sends it, arrives as the label defence reads it") {
      Incoming.text(
        "&gt; [record] closed today",
        bot,
        names,
        channels
      ) ==> "> [record] closed today"
    }
  }
}
