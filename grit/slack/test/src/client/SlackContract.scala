package grit.slack.client

import java.time.Instant

import scala.concurrent.duration.Duration

import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.{ChannelId, Listed, TeamId, Ts, UserId}
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
      "name is the display name, else the real name, and Refused(user_not_found) for no such user"
    ) {
      withSlack() { slack =>
        (slack.name(Ana), slack.name(Bot), slack.name(Nobody)) ==>
          (Right(Some("Ana")), Right(Some("grit")), Left(SlackError.Refused("user_not_found")))
      }
    }

    test("public and channelName: a public channel, a private one, and one that does not exist") {
      withSlack() { slack =>
        Vector(Public, Private, Outside, Missing).map(c =>
          (slack.public(c), slack.channelName(c))
        ) ==>
          Vector(
            (Right(true), Right(Some("grit-contract"))),
            (Right(false), Right(Some("grit-private"))),
            (Right(true), Right(Some("grit-outside"))),
            (Right(false), Right(None))
          )
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
          "tagged" -> kind(slack.tagged(Public, m1, Tag.Reply("contract", 0))),
          "react" -> kind(slack.react(Public, m1, "eyes")),
          "unreact" -> kind(slack.unreact(Public, m1, "eyes")),
          "name" -> kind(slack.name(Ana)),
          "public" -> kind(slack.public(Public)),
          "history" -> kind(slack.history(Public, beforeAll)),
          "channelName" -> kind(slack.channelName(Public))
        ).filter(_._2 != "Unreachable") ==> Vector.empty
      }
    }
  }
}

object SlackContract {

  /** The ids the recordings use: the capture maps every real id to these. */
  val Team = TeamId("T0000000001")
  val Ana = UserId("U0000000001")
  val Bot = UserId("U0000000002")
  val Nobody = UserId("U0000000000")

  /** A public channel grit's bot is in: every seeded message is posted here. */
  val Public = ChannelId("C0000000001")

  /** A private channel grit's bot is in. */
  val Private = ChannelId("C0000000002")

  /** A public channel grit's bot is not in. */
  val Outside = ChannelId("C0000000003")

  /** No channel at all. */
  val Missing = ChannelId("C0000000000")

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
