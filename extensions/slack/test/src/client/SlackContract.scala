package grit.slack.client

import java.time.Instant

import scala.concurrent.duration.Duration

import grit.core.identity.{Email, Standing}
import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.{ChannelId, Listed, ResponseUrl, TeamId, Ts, UserId}
import grit.slack.text.{Post, RichText}

import utest.*

/** What every [[Slack]] keeps of its documented behaviour, over one small workspace
  * ([[SlackContract.Workspace]]), run against [[FakeSlack]] and against [[SocketSlack]]
  * answered by Slack's own recorded JSON. `listen` is not covered.
  */
abstract class SlackContract extends TestSuite {
  import SlackContract.*

  /** Runs `body` over a Slack holding [[SlackContract.Workspace]]. The next `limited` Web API
    * requests from the start are answered rate-limited, 0 s to wait: they are counted across
    * calls, so one `history` spends them all on its first request. When `down`, Slack cannot
    * be reached at all.
    */
  protected def withSlack[A](limited: Int = 0, down: Boolean = false)(body: Slack => A): A

  /** Runs `body` over a Slack as [[withSlack]] does, given [[Hooks]]: the response urls of two
    * slash commands and what each was answered. When `down`, Slack cannot be reached there
    * either.
    */
  protected def withCommand[A](down: Boolean = false)(body: (Slack, Hooks^) => A): A

  private val w = Workspace

  /** A listing as the names of its messages, a ts the workspace does not know as itself. */
  private def named(r: Either[SlackError, Vector[Listed]]): Either[SlackError, Vector[String]] =
    r.map(_.map(l => w.nameOf(l.ts)))

  private val beforeAll = w.at("m1").minusSeconds(1)

  private val post: Post = RichText.render(Doc(Vector(Block.Paragraph(Text.plain("hi"))))) match {
    case p +: _ => p
    case _ => throw new java.lang.AssertionError("a paragraph renders as no post")
  }

  val tests = Tests {
    test("self is the workspace's team and grit's bot user") {
      withSlack() { slack =>
        slack.self() ==> Right(Self(Team, Bot))
      }
    }

    test(
      "history from before every message is every root and every reply, oldest first, each once"
    ) {
      withSlack() { slack =>
        named(slack.history(Public, beforeAll)) ==>
          Right(Vector("m1", "m2", "r1", "r2", "b2", "m3", "t1"))
      }
    }

    test(
      "history from a message's own ts includes it, and leaves out a later reply to an earlier root"
    ) {
      withSlack() { slack =>
        named(slack.history(Public, w.at("m2"))) ==> Right(Vector("m2", "r2", "b2", "m3", "t1"))
      }
    }

    test(
      "history from a microsecond after a root leaves it and its replies out, but keeps one also sent to the channel"
    ) {
      withSlack() { slack =>
        named(slack.history(Public, w.at("m2").plusNanos(1000))) ==>
          Right(Vector("b2", "m3", "t1"))
      }
    }

    test("history where grit's bot is not a member is Refused(not_in_channel)") {
      withSlack() { slack =>
        slack.history(Outside, beforeAll) ==> Left(SlackError.Refused("not_in_channel"))
      }
    }

    test("history waits out 5 rate limits in a row, and is Limited by a 6th") {
      withSlack(limited = 5) { slack =>
        named(slack.history(Public, w.at("m3"))) ==> Right(Vector("m3", "t1"))
      }
      withSlack(limited = 6) { slack =>
        slack.history(Public, w.at("m3")) ==> Left(SlackError.Limited(Duration.Zero))
      }
    }

    test("tagged finds exactly the post carrying the tag, and nothing for another tag") {
      withSlack() { slack =>
        val thread = w.ts("m3")
        val posted = slack.post(Public, thread, post, Tag.Reply("contract", 0))
        slack.tagged(Public, thread, Tag.Reply("contract", 0)) ==> posted.map(Vector(_))
        slack.tagged(Public, thread, Tag.Reply("contract", 1)) ==> Right(Vector.empty)
        slack.tagged(Public, thread, Tag.Refused(thread)) ==> Right(Vector.empty)
      }
    }

    test("postTopLevel starts a thread of its own, the post found there by its tag") {
      withSlack() { slack =>
        val found = for {
          ts <- slack.postTopLevel(Public, post, Tag.Sent("contract"))
          tagged <- slack.tagged(Public, ts, Tag.Sent("contract"))
        } yield tagged == Vector(ts)
        found ==> Right(true)
      }
    }

    test("root of a thread grit posted is grit's bot's message, with its tag and text") {
      withSlack() { slack =>
        slack.postTopLevel(Public, post, Tag.Sent("contract")).flatMap(slack.root(Public, _)) ==>
          Right(Some(Root(Some(Bot), Some(Tag.Sent("contract")), "hi")))
      }
    }

    test("postTopLevel where grit's bot is not a member is Refused(not_in_channel)") {
      withSlack() { slack =>
        slack.postTopLevel(Outside, post, Tag.Sent("contract")) ==>
          Left(SlackError.Refused("not_in_channel"))
      }
    }

    test("post where grit's bot is not a member is Refused(not_in_channel)") {
      withSlack() { slack =>
        slack.post(Outside, w.ts("m1"), post, Tag.Reply("contract", 0)) ==>
          Left(SlackError.Refused("not_in_channel"))
      }
    }

    test("a thread no message begins: tagged is Refused(thread_not_found), and root is None") {
      withSlack() { slack =>
        (
          slack.tagged(Public, NoMessage, Tag.Reply("contract", 0)),
          slack.root(Public, NoMessage)
        ) ==>
          (Left(SlackError.Refused("thread_not_found")), Right(None))
      }
    }

    test("a reaction to no message is Refused(message_not_found)") {
      withSlack() { slack =>
        slack.react(Public, NoMessage, "eyes") ==> Left(SlackError.Refused("message_not_found"))
      }
    }

    test("a reaction added twice, or removed twice, is never an error") {
      withSlack() { slack =>
        val m1 = w.ts("m1")
        (slack.react(Public, m1, "eyes"), slack.react(Public, m1, "eyes")) ==> (
          Right(()),
          Right(())
        )
        (slack.unreact(Public, m1, "eyes"), slack.unreact(Public, m1, "eyes")) ==>
          (Right(()), Right(()))
      }
    }

    test(
      "member: a full member with a confirmed address has it, grit's bot is outside, and no such user is outside with no name"
    ) {
      withSlack() { slack =>
        (slack.member(Team, Ana), slack.member(Team, Bot), slack.member(Team, Nobody)) ==> (
          Right(Member(Some("Ana"), AnaStanding)),
          Right(Member(Some("grit"), Standing.Outside)),
          Right(Member(None, Standing.Outside))
        )
      }
    }

    test("member rate-limited is that failure, never outside") {
      withSlack(limited = 1) { slack =>
        slack.member(Team, Ana) ==> Left(SlackError.Limited(Duration.Zero))
      }
    }

    test("member of another team than the user's is outside, under the same name") {
      withSlack() { slack =>
        slack.member(OtherTeam, Ana) ==> Right(Member(Some("Ana"), Standing.Outside))
      }
    }

    test("members is every page of the workspace's users, each as member reads them") {
      withSlack() { slack =>
        slack.members(Team) ==> Right(
          Map(
            Ana -> Member(Some("Ana"), AnaStanding),
            Bot -> Member(Some("grit"), Standing.Outside),
            Gia -> Member(Some("Gia"), Standing.Outside)
          )
        )
      }
    }

    test("members waits out 5 rate limits in a row, and is Limited by a 6th") {
      withSlack(limited = 5) { slack =>
        slack.members(Team).map(_.keySet) ==> Right(Set(Ana, Bot, Gia))
      }
      withSlack(limited = 6) { slack =>
        slack.members(Team) ==> Left(SlackError.Limited(Duration.Zero))
      }
    }

    test(
      "a message's permalink is its channel's archive, then p and its ts without the point; none for no message"
    ) {
      withSlack() { slack =>
        (slack.permalink(Public, w.ts("m1")), slack.permalink(Public, NoMessage)) ==> (
          Right(s"https://$Domain.slack.com/archives/C0000000001/p1790782260791279"),
          Left(SlackError.Refused("message_not_found"))
        )
      }
    }

    test(
      "kind and channelName: a public channel, a private one, one grit's bot is not in, and one that does not exist"
    ) {
      withSlack() { slack =>
        Vector(Public, Private, Outside, Missing).map(c =>
          (slack.kind(c), slack.channelName(c))
        ) ==>
          Vector(
            (Right(ChannelKind.Public), Right(Some("grit-contract"))),
            (Right(ChannelKind.Private), Right(Some("grit-private"))),
            (Right(ChannelKind.Public), Right(Some("grit-outside"))),
            (Right(ChannelKind.Unseen), Right(None))
          )
      }
    }

    test("respond answers its asker alone, at the command's url, in the words given") {
      withCommand() { (slack, hooks) =>
        val words = "label <level> & <!channel> &lt;: *not bold*"
        (slack.respond(hooks.live, words), hooks.answered()) ==>
          (Right(()), Vector(Answered(hooks.live, ephemeral = true, words)))
      }
    }

    test("respond at a command's url past its 30 minutes is Refused(expired_url)") {
      withCommand() { (slack, hooks) =>
        (slack.respond(hooks.expired, "too late"), hooks.answered()) ==>
          (Left(SlackError.Refused("expired_url")), Vector.empty)
      }
    }

    test("respond where Slack cannot be reached is Unreachable") {
      withCommand(down = true) { (slack, hooks) =>
        slack.respond(hooks.live, "hi") match {
          case Left(SlackError.Unreachable(_)) => ()
          case other => throw new java.lang.AssertionError(s"not Unreachable: $other")
        }
      }
    }

    test("a Slack that cannot be reached is Unreachable from every call but listen") {
      withSlack(down = true) { slack =>
        val m1 = w.ts("m1")
        def kind(r: Either[SlackError, ?]): String = r match {
          case Left(SlackError.Unreachable(_)) => "Unreachable"
          case other => other.toString
        }
        Vector(
          "self" -> kind(slack.self()),
          "post" -> kind(slack.post(Public, m1, post, Tag.Reply("contract", 0))),
          "postTopLevel" -> kind(slack.postTopLevel(Public, post, Tag.Sent("contract"))),
          "tagged" -> kind(slack.tagged(Public, m1, Tag.Reply("contract", 0))),
          "root" -> kind(slack.root(Public, m1)),
          "permalink" -> kind(slack.permalink(Public, m1)),
          "react" -> kind(slack.react(Public, m1, "eyes")),
          "unreact" -> kind(slack.unreact(Public, m1, "eyes")),
          "member" -> kind(slack.member(Team, Ana)),
          "members" -> kind(slack.members(Team)),
          "kind" -> kind(slack.kind(Public)),
          "history" -> kind(slack.history(Public, beforeAll)),
          "channelName" -> kind(slack.channelName(Public))
        ).filter(_._2 != "Unreachable") ==> Vector.empty
      }
    }
  }
}

object SlackContract {

  /** Two slash commands' response urls, and what each has been answered. */
  trait Hooks {

    /** The url of a command Slack still takes answers at. */
    def live: ResponseUrl

    /** The url of a command asked more than 30 minutes ago. */
    def expired: ResponseUrl

    /** What Slack took at either url, oldest first. */
    def answered(): Vector[Answered]
  }

  /** An answer Slack took at `url`: whether only its asker sees it, and its words as they
    * read in Slack.
    */
  final case class Answered(url: ResponseUrl, ephemeral: Boolean, shown: String)

  /** The ids the recordings use: the capture maps every real id to these. */
  val Team = TeamId("T0000000001")

  /** The workspace's Slack domain, as the recordings name it. */
  val Domain = "grit-contract"
  val Ana = UserId("U0000000001")
  val Bot = UserId("U0000000002")
  val Nobody = UserId("U0000000000")

  /** A guest of the workspace, listed but not a full member. */
  val Gia = UserId("U0000000003")

  /** A team none of the workspace's users is of. */
  val OtherTeam = TeamId("T0000000009")

  /** What Slack says of [[Ana]] in [[Team]]: a full member, her address confirmed. */
  val AnaStanding: Standing =
    Standing.Full(Email.of("ana@grit-contract.example").toOption)

  /** A public channel grit's bot is in: every seeded message is posted here. */
  val Public = ChannelId("C0000000001")

  /** A private channel grit's bot is in. */
  val Private = ChannelId("C0000000002")

  /** A public channel grit's bot is not in. */
  val Outside = ChannelId("C0000000003")

  /** No channel at all. */
  val Missing = ChannelId("C0000000000")

  /** A ts no message in [[Public]] has. */
  val NoMessage = Ts("1790782200.000001")

  /** One message the capture posted in [[Public]], all as grit's bot. `root` names the message
    * whose thread it replies in; `broadcast`, a reply also sent to the channel.
    */
  final case class Seeded(
      name: String,
      ts: Ts,
      root: Option[String],
      broadcast: Boolean,
      text: String
  )

  /** The workspace the contract runs over: the messages `slack-api/workspace.json` lists, with
    * the ts Slack gave them, in the order they were posted.
    */
  object Workspace {
    val messages: Vector[Seeded] = {
      val stream = Option(getClass.getResourceAsStream("/slack-api/workspace.json"))
        .getOrElse(throw new java.lang.AssertionError("slack-api/workspace.json is missing"))
      val json =
        try ujson.read(stream)
        finally stream.close()
      json("messages").arr.toVector.map { m =>
        Seeded(
          m("name").str,
          Ts(m("ts").str),
          m("root").strOpt,
          m("broadcast").bool,
          m("text").str
        )
      }
    }

    private def find(name: String): Seeded =
      messages
        .find(_.name == name)
        .getOrElse(throw new java.lang.AssertionError(s"workspace.json has no $name"))

    def ts(name: String): Ts = find(name).ts

    /** The instant message `name`'s ts names, to the microsecond. */
    def at(name: String): Instant = {
      val (seconds, micros) = Ts.value(ts(name)).span(_ != '.')
      Instant.ofEpochSecond(seconds.toLong, micros.drop(1).toLong * 1000)
    }

    /** The name of the message at `ts`, or `ts` itself when none was seeded there. */
    def nameOf(ts: Ts): String = messages.find(_.ts == ts).fold(Ts.value(ts))(_.name)

    /** What Slack lists in [[Public]]: its history, newest first, then each thread's replies,
      * root first, as a client reading both would meet them.
      */
    val listed: Vector[Listed] = {
      def listing(m: Seeded): Listed = {
        val thread = m.root.map(r => ts(r)).orElse {
          Option.when(messages.exists(_.root.contains(m.name)))(m.ts)
        }
        val subtype = Option.when(m.broadcast)("thread_broadcast")
        Listed(m.ts, thread, Some(Bot), true, subtype, m.text)
      }
      val history = messages.filter(m => m.root.isEmpty || m.broadcast).reverse
      val threads = messages.filter(m => m.root.isEmpty).flatMap { root =>
        messages.filter(m => m.name == root.name || m.root.contains(root.name))
      }
      (history ++ threads).map(listing)
    }
  }
}
