package grit.slack.client

import scala.jdk.CollectionConverters.*

import grit.core.identity.{Email, Standing}
import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.{ChannelId, Listed, TeamId, Ts, UserId}
import grit.slack.text.RichText

import com.slack.api.model.{Message, User}
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

  private val Ours = TeamId("T1")

  /** A full member of [[Ours]] with a confirmed address at an unclaimed domain, as `users.info`
    * gives one, changed by `change`.
    */
  private def user(change: User -> Unit = _ => ()): User = {
    val u = new User()
    u.setId("U1")
    u.setTeamId("T1")
    u.setRealName("Ana Lima")
    val p = new User.Profile()
    p.setDisplayName("Ana")
    p.setEmail("Ana@Elsewhere.example")
    u.setProfile(p)
    u.setEmailConfirmed(true)
    change(u)
    u
  }

  val tests = Tests {
    test("a slash command is acknowledged before its handler is started, then handled") {
      // A Java list: the three callbacks share it, as one thread runs each in turn here.
      val seen = new java.util.ArrayList[String]()
      SocketSlack.commanded(
        "E1",
        "{}",
        id => { val _ = seen.add(s"acknowledged $id") },
        run => {
          val _ = seen.add("started")
          run()
        },
        payload => { val _ = seen.add(s"handled $payload") }
      )
      seen.asScala.toVector ==> Vector("acknowledged E1", "started", "handled {}")
    }

    test(
      "a response url's refusal is Slack's word for it, from a JSON body or a plain one, else the status"
    ) {
      Vector(
        SocketSlack.answered(200, "ok"),
        SocketSlack.answered(404, """{"ok":false,"error":"used_url"}"""),
        SocketSlack.answered(410, "expired_url\n"),
        SocketSlack.answered(500, "")
      ) ==> Vector(
        Right(()),
        Left(SlackError.Refused("used_url")),
        Left(SlackError.Refused("expired_url")),
        Left(SlackError.Refused("http 500"))
      )
    }

    test(
      "a user is a full member with their confirmed address, whatever its domain, only of their own team and when none of guest, stranger, bot, app, invited or deactivated"
    ) {
      def standing(change: User -> Unit): Standing = SocketSlack.member(Ours, user(change)).standing
      val outside = Vector[(String, User -> Unit)](
        "a guest" -> (_.setRestricted(true)),
        "a single-channel guest" -> (_.setUltraRestricted(true)),
        "a stranger" -> (_.setStranger(true)),
        "another team's user" -> (_.setTeamId("T2")),
        "a user of no team" -> (_.setTeamId(null)),
        "a bot" -> (_.setBot(true)),
        "an app's user" -> (_.setAppUser(true)),
        "an invited user" -> (_.setInvitedUser(true)),
        "a deactivated user" -> (_.setDeleted(true))
      ).map((who, change) => who -> standing(change))
      outside.filter(_._2 != Standing.Outside) ==> Vector.empty
      (
        standing(_ => ()),
        standing(_.setEmailConfirmed(false)),
        standing(_.getProfile.setEmail(null)),
        standing(_.getProfile.setEmail("not an address")),
        standing(_.setProfile(null))
      ) ==> (
        Standing.Full(Email.of("ana@elsewhere.example").toOption),
        Standing.Full(None),
        Standing.Full(None),
        Standing.Full(None),
        Standing.Full(None)
      )
    }

    test("members whose later page fails is that failure, never the pages before it") {
      val bot =
        BotToken.of("xoxb-contract").fold(e => throw new java.lang.AssertionError(e), identity)
      val app =
        AppToken.of("xapp-contract").fold(e => throw new java.lang.AssertionError(e), identity)
      val stub = new SlackStub(0, failing = Set("users.list cursor=dXNlcjpVMDAwMDAwMDAz"))
      val slack = new SocketSlack(bot, app, stub.api)
      try slack.members(SlackContract.Team) ==> Left(SlackError.Refused("fatal_error"))
      finally {
        slack.close()
        stub.close()
      }
    }

    test("a user's name is their display name, else their real name, else none") {
      (
        SocketSlack.member(Ours, user()).name,
        SocketSlack.member(Ours, user(_.getProfile.setDisplayName(" "))).name,
        SocketSlack.member(Ours, user(u => { u.setProfile(null); u.setRealName("") })).name
      ) ==> (Some("Ana"), Some("Ana Lima"), None)
    }

    test(
      "a listed message keeps its ts, thread, user, subtype, text and its author's team, and whether a bot sent it"
    ) {
      val reply = new Message()
      reply.setTs("2.0")
      reply.setThreadTs("1.0")
      reply.setUser("U1")
      reply.setSubtype("thread_broadcast")
      reply.setText("also")
      reply.setTeam("T0THEIRS")
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
          "also",
          Some(TeamId("T0THEIRS"))
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
