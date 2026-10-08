package grit.slack.edge

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.{Edges, Permit, Registration, Route, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Namespace, Place}
import grit.core.speech.Rate
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.slack.client.{FakeSlack, Tag}
import grit.slack.event.{ChannelId, Payloads, TeamId, Ts}

import utest.*

/** [[Posting]], `slack_post`, run against a fake Slack at a time the test sets. */
object PostingTests extends TestSuite {
  import Payloads.Team

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
    val posting: Posting^{slack, clock} = new Posting(slack, clock, TwoAnHour, TeamId(Team))
    posting
      .offer(Vector(("probably-not-skynet", Skynet), ("general", General)))
      .fold(why => throw new java.lang.AssertionError(why), _ => ())

    // Counted by the test's thread alone.
    @caps.unsafe.untrackedCaptures
    private var calls = 0

    /** `slack_post` called with `arguments`, as a request routed to `service:slack` writing to
      * `destination`.
      */
    def call(arguments: ujson.Obj, destination: Option[Place] = Some(SkynetAt)): Outcome = {
      val q = request(arguments, calls, destination)
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

  /** Where [[Skynet]] is written to: `slack:{team}/{id}`. */
  val SkynetAt: Place = Place.under(Namespace.Slack, Vector(Team, ChannelId.value(Skynet)))

  /** Where [[General]] is written to. */
  private val GeneralAt: Place =
    Place.under(Namespace.Slack, Vector(Team, ChannelId.value(General)))

  /** `slack_post` called with `arguments` as call `index` of a turn's first round, sent to
    * `service:slack` writing to `destination` as the turn sends it; also the request the other
    * edge suites send.
    */
  def request(
      arguments: ujson.Obj,
      index: Int,
      destination: Option[Place] = Some(SkynetAt)
  ): ToolRequest =
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
      destination
    )

  /** Where an edge hosting `q`'s place routes it. */
  def route(q: ToolRequest): Route =
    Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  private def post(text: String): ujson.Obj = ujson.Obj("text" -> text)

  private def reply(text: String, thread: String): ujson.Obj =
    ujson.Obj("text" -> text, "thread" -> thread)

  private val RootLink = "https://acme.slack.com/archives/C0C5U2FPAL8/p1790782262102319"

  val tests = Tests {
    test("it posts at an offered channel's top level, tagged with its request, and says where") {
      val w = new World
      w.call(post("The build is **green**.")) ==> Outcome.Done("Posted in #probably-not-skynet.")
      w.posted.map((c, _, text, tag) => (c, text, tag)) ==>
        Vector((Skynet, "The build is green.", Tag.Sent(slot(0).key)))
      w.slack.posts.map(p => p.thread == p.ts) ==> Vector(true)
    }

    test("it posts to the channel its request was checked to write to, whatever `to` held") {
      val w = new World
      Vector(
        w.call(ujson.Obj("to" -> "general", "text" -> "one")),
        w.call(post("two"), Some(GeneralAt))
      ) ==> Vector(
        Outcome.Done("Posted in #probably-not-skynet."),
        Outcome.Done("Posted in #general.")
      )
      w.posted.map((c, _, text, _) => (c, text)) ==> Vector((Skynet, "one"), (General, "two"))
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
      "it refuses a request told no channel or one not its own, a link into another channel, and text that is no link, posting nothing"
    ) {
      val w = new World
      val elsewhere = Place.under(Namespace.Slack, Vector(Team, "C0RANDOM01"))
      Vector(
        w.call(post("hi"), None),
        w.call(post("hi"), Some(elsewhere)),
        w.call(reply("hi", "https://acme.slack.com/archives/C0GENERAL01/p1790782262102319")),
        w.call(reply("hi", "the thread from this morning"))
      ) ==> Vector(
        Outcome.Failed("slack_post was told no place to write to; it did not run."),
        Outcome.Failed(
          s"slack_post does not write to slack:$Team/C0RANDOM01 here; it did not run."
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

    test(
      "its rate is counted across a change of the channels it offers: posts made before count after"
    ) {
      val w = new World
      w.call(post("one"))
      w.call(post("two"))
      w.posting.offer(Vector(("general", General))).map(_ => ()) ==> Right(())
      w.call(post("three"), Some(GeneralAt)) ==> Outcome.Failed(
        "grit has made 2 posts in the last 1 hour, as many as it may. Nothing was posted."
      )
      w.posted.map(_._3) ==> Vector("one", "two")
    }

    test(
      "offered other channels, it refuses a request to one no longer offered; offered none, it advertises nothing and refuses every request, posting nothing"
    ) {
      val w = new World
      w.posting
        .offer(Vector(("general", General)))
        .map(_.tools.map(e => (e.name, e.writes.map(_.to.toVector.map(_._1))))) ==>
        Right(Vector((ToolName("slack_post"), Some(Vector("general", "#general")))))
      val toSkynet = w.call(post("one"))
      w.posting.offer(Vector.empty) ==> Right(ToolSet.Empty)
      (toSkynet, w.posting.offered, w.call(post("two"), Some(GeneralAt)), w.posted) ==> (
        Outcome.Failed(
          s"slack_post does not write to slack:$Team/${ChannelId.value(Skynet)} here; it did not run."
        ),
        ToolSet.Empty,
        Outcome.Failed(
          "grit's bot is in no channel slack_post may post in now. Nothing was posted."
        ),
        Vector.empty
      )
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
      "its advert: slack_post alone, never asking first, never run again, writing to each channel by its name bare and with its #, at slack:{team}/{id}, its description naming none"
    ) {
      val w = new World
      val at = (id: ChannelId) => Place.under(Namespace.Slack, Vector(Team, ChannelId.value(id)))
      w.posting.offered.tools.map(e => (e.name, e.asks, e.retry)) ==>
        Vector((ToolName("slack_post"), false, Retry.Interrupt))
      w.posting.offered.tools.map(_.writes.map(_.to.toVector)) ==> Vector(
        Some(
          Vector(
            "probably-not-skynet" -> at(Skynet),
            "general" -> at(General),
            "#probably-not-skynet" -> at(Skynet),
            "#general" -> at(General)
          )
        )
      )
      w.posting.offered.tools.map(_.does) ==> Vector(
        "Post a message in a Slack channel, as grit: at the channel's top level, or as a " +
          "reply in a thread when `thread` is a link to a message in that channel. It makes " +
          "at most 2 posts per 1 hour across its channels. Use it only when the person asks " +
          "for something to be posted there; your reply to them is posted where they wrote, " +
          "without it. The text is posted as written: nothing in it becomes a mention. A post " +
          "cut short may already be in Slack."
      )
    }
  }
}
