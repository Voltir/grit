package grit.slack.edge

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.{Edges, Permit, Registration, Route, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.speech.Rate
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.slack.client.{FakeSlack, Tag}
import grit.slack.event.{ChannelId, Ts}

import utest.*

/** [[Posting]], `slack_post`, run against a fake Slack at a time the test sets. */
object PostingTests extends TestSuite {

  private val Skynet = ChannelId("C0C5U2FPAL8")
  private val General = ChannelId("C0GENERAL01")

  /** A clock at `at`, moved by the test. */
  private final class SetClock(
      // Set by the test's thread alone, between calls.
      @caps.unsafe.untrackedCaptures var at: Instant
  ) extends Clock {
    def now(): Instant = at
    def millis(): Long = at.toEpochMilli
    def sleep(duration: FiniteDuration): Unit = at = at.plusNanos(duration.toNanos)
    def nonce(): String = "n"
  }

  /** 2 an hour: small enough to reach. */
  private val TwoAnHour: Rate =
    Rate.of(2, 1.hour).getOrElse(throw new java.lang.AssertionError("a rate"))

  private final class World {
    val slack: FakeSlack^ = new FakeSlack
    slack.channelNames = Map(Skynet -> "probably-not-skynet", General -> "general")
    val clock: SetClock^ = new SetClock(Instant.parse("2026-10-01T12:00:00Z"))
    val posting: Posting^{slack, clock} =
      Posting
        .of(slack, clock, TwoAnHour, Vector(("probably-not-skynet", Skynet)))
        .getOrElse(throw new java.lang.AssertionError("one channel is a posting"))

    // Counted by the test's thread alone.
    @caps.unsafe.untrackedCaptures
    private var calls = 0

    /** `slack_post` called with `arguments`, as a request routed to `service:slack`. */
    def call(arguments: ujson.Obj): Outcome = {
      val q = request(arguments, calls)
      calls += 1
      posting.run(route(q), q)
    }

    /** What Slack was posted: channel, thread, fallback text, tag. */
    def posted: Vector[(ChannelId, Ts, String, Tag)] =
      slack.posts.map(p => (p.channel, p.thread, p.post.fallback, p.tag))
  }

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)

  def slot(index: Int): CallSlot =
    CallSlot.of(turn, 0, index).getOrElse(throw new java.lang.AssertionError("a slot"))

  /** `slack_post` called with `arguments` as call `index` of a turn's first round, sent to
    * `service:slack` as the turn sends it; also the request the other edge suites send.
    */
  def request(arguments: ujson.Obj, index: Int): ToolRequest =
    ToolRequest(
      slot(index),
      ToolRequest.Protocol,
      turn.conversationId,
      SlackEdge.PostsAt.place,
      PrincipalId.Grit,
      Posting.Name,
      Permit.Free,
      Retry.Interrupt,
      arguments,
      Set.empty,
      None
    )

  /** Where an edge hosting `q`'s place routes it. */
  def route(q: ToolRequest): Route =
    Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  private def post(text: String, channel: String = "probably-not-skynet"): ujson.Obj =
    ujson.Obj("channel" -> channel, "text" -> text)

  private def reply(text: String, thread: String): ujson.Obj =
    ujson.Obj("channel" -> "probably-not-skynet", "text" -> text, "thread" -> thread)

  private val RootLink = "https://acme.slack.com/archives/C0C5U2FPAL8/p1790782262102319"

  val tests = Tests {
    test("it posts at a declared channel's top level, tagged with its request, and says where") {
      val w = new World
      w.call(post("The build is **green**.")) ==> Outcome.Done("Posted in #probably-not-skynet.")
      w.posted.map((c, _, text, tag) => (c, text, tag)) ==>
        Vector((Skynet, "The build is green.", Tag.Sent(slot(0).key)))
      w.slack.posts.map(p => p.thread == p.ts) ==> Vector(true)
    }

    test("a channel named with its leading # posts there, as named without it") {
      val w = new World
      w.call(post("one", channel = "#probably-not-skynet")) ==>
        Outcome.Done("Posted in #probably-not-skynet.")
      w.posted.map((c, _, text, _) => (c, text)) ==> Vector((Skynet, "one"))
    }

    test("it posts in the thread a link to a message in that channel names") {
      val w = new World
      val replyLink = s"$RootLink?thread_ts=1790782260.000001&cid=C0C5U2FPAL8"
      w.call(reply("one", RootLink)) ==>
        Outcome.Done("Posted in the thread in #probably-not-skynet.")
      w.call(reply("two", replyLink)) ==>
        Outcome.Done("Posted in the thread in #probably-not-skynet.")
      w.posted.map((c, t, text, _) => (c, t, text)) ==> Vector(
        (Skynet, Ts("1790782262.102319"), "one"),
        (Skynet, Ts("1790782260.000001"), "two")
      )
    }

    test(
      "it refuses a channel not offered, a link into another channel, and text that is no link, posting nothing"
    ) {
      val w = new World
      Vector(
        w.call(post("hi", channel = "general")),
        w.call(reply("hi", "https://acme.slack.com/archives/C0GENERAL01/p1790782262102319")),
        w.call(reply("hi", "the thread from this morning"))
      ) ==> Vector(
        Outcome.Failed(
          "The call to `slack_post` was not run: `channel` takes one of `probably-not-skynet`, " +
            "`#probably-not-skynet`, not \"general\". You sent: " +
            "{\"channel\":\"general\",\"text\":\"hi\"}"
        ),
        Outcome.Failed(
          "That link is to a message outside #probably-not-skynet. Nothing was posted."
        ),
        Outcome.Failed(
          "`the thread from this morning` is not a link to a Slack message. Nothing was posted."
        )
      )
      w.posted ==> Vector.empty
    }

    test(
      "it refuses a post past its rate, naming the rate, and posts again once one has left the span"
    ) {
      val w = new World
      w.call(post("one"))
      w.clock.at = w.clock.at.plusSeconds(1800)
      w.call(post("two"))
      w.clock.at = w.clock.at.plusSeconds(1799)
      w.call(post("three")) ==> Outcome.Failed(
        "grit has made 2 posts in the last 1 hour, as many as it may. Nothing was posted."
      )
      w.clock.at = w.clock.at.plusSeconds(1)
      w.call(post("four")) ==> Outcome.Done("Posted in #probably-not-skynet.")
      w.posted.map(_._3) ==> Vector("one", "two", "four")
    }

    test("a post Slack refuses is told as Slack's refusal, and does not count against the rate") {
      val w = new World
      w.slack.notIn = Set(Skynet)
      w.call(post("one")) ==> Outcome.Failed(
        "grit's bot is not in #probably-not-skynet, so it cannot post there. Nothing was posted."
      )
      w.slack.notIn = Set.empty
      w.call(post("two"))
      w.call(post("three")) ==> Outcome.Done("Posted in #probably-not-skynet.")
    }

    test("text too long for one Slack message, or empty, is refused, posting nothing") {
      val w = new World
      w.call(post("word " * 700)) ==> Outcome.Failed(
        "The text is too long for one Slack message (at most 3000 characters and 50 blocks). " +
          "Nothing was posted."
      )
      w.call(post("   ")) ==> Outcome.Failed("The text is empty. Nothing was posted.")
      w.posted ==> Vector.empty
    }

    test("what it posts is literal: its plain text escapes a broadcast and a mention") {
      val w = new World
      w.call(post("<!channel> ask <@U0BEN0001> & co"))
      w.posted.map(_._3) ==> Vector("&lt;!channel&gt; ask &lt;@U0BEN0001&gt; &amp; co")
    }

    test(
      "its advert: slack_post alone, never asking first, never run again, the channels an enum of each name bare and with its #"
    ) {
      val w = new World
      w.posting.offered.tools.map(e => (e.name, e.asks, e.retry)) ==>
        Vector((ToolName("slack_post"), false, Retry.Interrupt))
      w.posting.offered.tools.map(_.parameters("properties")("channel")("enum")) ==>
        Vector(ujson.Arr("probably-not-skynet", "#probably-not-skynet"))
      w.posting.offered.tools.map(_.does) ==> Vector(
        "Post a message in a Slack channel, as grit: at the channel's top level, or as a " +
          "reply in a thread when `thread` is a link to a message in that channel. It posts " +
          "only in #probably-not-skynet, and at most 2 posts per 1 hour across them. Use it " +
          "only when the person asks for something to be posted there; your reply to them is " +
          "posted where they wrote, without it. The text is posted as written: nothing in it " +
          "becomes a mention. A post cut short may already be in Slack."
      )
    }
  }
}
