package grit.slack.client

import scala.jdk.CollectionConverters.*

import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.{ChannelId, Ts}
import grit.slack.text.RichText

import com.slack.api.model.Message
import utest.*

/** What [[SocketSlack]] builds and reads without Slack: the post's request, the tag it carries,
  * and the tokens. The transport itself is covered by the real-use run.
  */
object SocketSlackTests extends TestSuite {

  private val post = RichText.render(Doc(Vector(Block.Paragraph(Text.plain("hi"))))).head

  private def message(eventType: String, turn: String, part: String): Message = {
    val m = new Message()
    m.setTs("9.9")
    m.setMetadata(
      Message.Metadata
        .builder()
        .eventType(eventType)
        .eventPayload(Map[String, AnyRef]("turn" -> turn, "part" -> part).asJava)
        .build()
    )
    m
  }

  val tests = Tests {
    test("a reply's request carries its thread, blocks, fallback and tag, with link previews off") {
      val r = SocketSlack.request(ChannelId("C1"), Ts("1.0"), post, Tag("c:3", 1))
      (
        r.getChannel,
        r.getThreadTs,
        r.getBlocksAsString,
        r.getText,
        r.isUnfurlLinks,
        r.isUnfurlMedia
      ) ==>
        ("C1", "1.0", post.blocks.render(), "hi", false, false)
      (r.getMetadata.getEventType, r.getMetadata.getEventPayload.asScala.toMap) ==>
        ("grit_reply", Map("turn" -> "c:3", "part" -> "1"))
    }

    test("a message carries a tag only under grit's event type, with the same turn and part") {
      val tag = Tag("c:3", 1)
      SocketSlack.carries(message("grit_reply", "c:3", "1"), tag) ==> true
      SocketSlack.carries(message("grit_reply", "c:3", "0"), tag) ==> false
      SocketSlack.carries(message("grit_reply", "c:4", "1"), tag) ==> false
      SocketSlack.carries(message("other_app", "c:3", "1"), tag) ==> false
      SocketSlack.carries(new Message(), tag) ==> false
    }

    test("a token without its prefix is refused, and a token never shows itself") {
      BotToken.of("xoxp-123") ==> Left("a bot token starts xoxb-")
      AppToken.of("xoxb-123") ==> Left("an app-level token starts xapp-")
      BotToken.of(" xoxb-secret ").map(_.toString) ==> Right("BotToken(****)")
      AppToken.of("xapp-secret").map(_.toString) ==> Right("AppToken(****)")
    }
  }
}
