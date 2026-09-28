package grit.slack.text

import grit.slack.event.UserId

import utest.*

/** [[Incoming]]: a person's Slack text as grit stores it. */
object IncomingTests extends TestSuite {

  private val bot = UserId("UBOT")
  private val names: UserId -> Option[String] = Map(UserId("UANA") -> "Ana Lima").get

  val tests = Tests {
    test("grit's own mention is removed, and the text trimmed") {
      Incoming.text("<@UBOT> what are places?", bot, names) ==> "what are places?"
      Incoming.text("so, <@UBOT>, what now", bot, names) ==> "so, , what now"
    }

    test("another person's mention is their name, or their id when it is not known") {
      Incoming.text("ask <@UANA> or <@UZED>", bot, names) ==> "ask @Ana Lima or @UZED"
      Incoming.text("ask <@UZED|zed>", bot, names) ==> "ask @zed"
    }

    test("channels, broadcasts and links are written out; the escapes are undone") {
      Incoming.text("see <#C1|general> and <#C2>", bot, names) ==> "see #general and #C2"
      Incoming.text("<!here> <!channel> <!everyone> <!subteam^S1|@devs>", bot, names) ==>
        "@here @channel @everyone @devs"
      Incoming.text("<https://example.com|the docs> or <https://example.com/x>", bot, names) ==>
        "the docs (https://example.com) or https://example.com/x"
      Incoming.text("a &lt; b &amp;&amp; c &gt; d", bot, names) ==> "a < b && c > d"
    }

    test("a pasted grit label, quoted as Slack sends it, arrives as the label defence reads it") {
      Incoming.text("&gt; [record] closed today", bot, names) ==> "> [record] closed today"
    }
  }
}
