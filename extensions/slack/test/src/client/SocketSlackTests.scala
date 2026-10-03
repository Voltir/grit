package grit.slack.client

import scala.jdk.CollectionConverters.*

import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.{ChannelId, Listed, Ts, UserId}
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
    test(
      "a listed message keeps its ts, thread, user, subtype and text, and whether a bot sent it"
    ) {
      val reply = new Message()
      reply.setTs("2.0")
      reply.setThreadTs("1.0")
      reply.setUser("U1")
      reply.setSubtype("thread_broadcast")
      reply.setText("also")
      val bots = new Message()
      bots.setTs("3.0")
      bots.setBotId("B1")
      (SocketSlack.listed(reply), SocketSlack.listed(bots)) ==> (
        Listed(
          Ts("2.0"),
          Some(Ts("1.0")),
          Some(UserId("U1")),
          false,
          Some("thread_broadcast"),
          "also"
        ),
        Listed(Ts("3.0"), None, None, true, None, "")
      )
    }

    test("history starts at the ts naming its instant, to the microsecond") {
      SocketSlack.oldest(java.time.Instant.parse("2018-01-08T22:12:02.000016Z")) ==>
        "1515449522.000016"
    }

    test("a reply's request carries its thread, blocks, fallback and tag, with link previews off") {
      val r = SocketSlack.request(ChannelId("C1"), Ts("1.0"), post, Tag.Reply("c:3", 1))
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

    test(
      "a refusal carries the message it answers under grit's refusal event, and no reply's tag"
    ) {
      val refused = Tag.Refused(Ts("5.0"))
      val r = SocketSlack.request(ChannelId("C1"), Ts("1.0"), post, refused)
      (r.getMetadata.getEventType, r.getMetadata.getEventPayload.asScala.toMap) ==>
        ("grit_refusal", Map("message" -> "5.0"))
      val m = new Message()
      m.setMetadata(r.getMetadata)
      (
        SocketSlack.carries(m, refused),
        SocketSlack.carries(m, Tag.Refused(Ts("6.0"))),
        SocketSlack.carries(message("grit_reply", "c:3", "1"), refused),
        SocketSlack.carries(m, Tag.Reply("c:3", 1))
      ) ==> (true, false, false, false)
    }

    test(
      "a top-level post's request has no thread and carries its request under grit's post event, read back as its tag"
    ) {
      // A stored form: SlackStub keys no metadata, so this is the one pin of a post's payload.
      val r = SocketSlack.topLevel(ChannelId("C1"), post, Tag.Sent("k"))
      (
        Option(r.getThreadTs),
        r.getMetadata.getEventType,
        r.getMetadata.getEventPayload.asScala.toMap
      ) ==>
        (None, "grit_post", Map("request" -> "k"))
      val m = new Message()
      m.setMetadata(r.getMetadata)
      SocketSlack.tagOf(m) ==> Some(Tag.Sent("k"))
    }

    test(
      "a review prompt carries its heard entry under grit's review event, read back as its tag"
    ) {
      // A stored form, as a post's is: the one pin of a prompt's payload.
      val tag = Tag.Prompt("in:c1:1790782260.791279")
      val r = SocketSlack.topLevel(ChannelId("C1"), post, tag)
      (r.getMetadata.getEventType, r.getMetadata.getEventPayload.asScala.toMap) ==>
        ("grit_review", Map("entry" -> "in:c1:1790782260.791279"))
      val m = new Message()
      m.setMetadata(r.getMetadata)
      SocketSlack.tagOf(m) ==> Some(tag)
    }

    test("a message carries a tag only under grit's event type, with the same turn and part") {
      val tag = Tag.Reply("c:3", 1)
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
