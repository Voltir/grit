package grit.slack.edge

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.{
  EdgeRefusal,
  EdgeStores,
  InMemoryDeliveries,
  InMemoryEdges,
  ServedEdge,
  Unheard,
  Variable
}
import grit.core.id.{PrincipalId, SourceId}
import grit.core.inbox.InMemoryInbox
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.speech.Rate
import grit.core.spend.Budget
import grit.core.store.{Jot, Origin, StoreError, Tx}
import grit.core.tool.Outcome
import grit.dbos.sql.TestTx
import grit.slack.client.{AppToken, BotToken, FakeSlack, Slack}
import grit.slack.event.{ChannelId, Listed, Payloads, Ts, UserId}

import utest.*

/** [[SlackEdge.serving]] and [[SlackEdge.backfill]] as a deployment's kit opens them, over the
  * in-memory stores and a fake Slack standing in for Socket Mode.
  */
object ServedTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  private val C = ChannelId("C123ABC456")

  private val Env = Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "xapp-1")

  private val Skynet = ChannelId("C0C5U2FPAL8")

  /** A channel Slack gives no name. */
  private val Unnamed = ChannelId("C0UNNAMED1")

  private val TwoAnHour: Rate =
    Rate.of(2, 1.hour).getOrElse(throw new java.lang.AssertionError("a rate"))

  private final class World {
    val slack = new FakeSlack
    val inbox: InMemoryInbox = InMemoryInbox.fresh(Budget(ZoneOffset.UTC, None))
    val edges: InMemoryEdges = new InMemoryEdges
    val stores =
      EdgeStores(inbox, inbox.principals, new InMemoryDeliveries, FakeJot, edges)

    /** What opening logged, in order. */
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    /** The edge as `serving` makes it, posting as `posts` allows, opened; logged in [[logged]]. */
    def openPosting(posts: Posts): ServedEdge.Open^{this} =
      Served
        .serving(Set.empty, Some(posts), connect)
        .open(stores, Env, line => logged :+= line) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
    val connect: Served.Connect^{slack} = new Served.Connect {
      def apply(bot: BotToken, app: AppToken): Slack^ = slack
    }
  }

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  /** Why `opened` was refused; `None` when it opened. */
  private def refusal(opened: Either[EdgeRefusal, Any]): Option[EdgeRefusal] = opened match {
    case Left(r) => Some(r)
    case Right(_) => None
  }

  val tests = Tests {
    test("serving refuses a token unset or of the wrong kind by its variable, never quoting it") {
      val w = new World
      val edge = Served.serving(Set(C), None, w.connect)
      def refused(env: Map[String, String]) = refusal(edge.open(w.stores, env, _ => ()))
      refused(Map("SLACK_APP_TOKEN" -> "xapp-1")) ==>
        Some(EdgeRefusal.Missing(Variable("SLACK_BOT_TOKEN")))
      refused(Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "sekrit")) ==>
        Some(EdgeRefusal.Malformed(Variable("SLACK_APP_TOKEN"), "an app-level token starts xapp-"))
      edge.needs ==> Vector(Variable("SLACK_BOT_TOKEN"), Variable("SLACK_APP_TOKEN"))
    }

    test(
      "serving, opened, names the assistant, takes a mention as a turn, posts its reply, and closes Slack"
    ) {
      val w = new World
      w.slack.names = w.slack.names.updated(UserId(Bot), Some("Bort"))
      val open = Served.serving(Set(C), None, w.connect).open(w.stores, Env, _ => ()) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
      w.inbox.principals.name(PrincipalId(s"slack:$Team"))(using TestTx.fake) ==> Right(
        Some("Bort")
      )
      w.slack.deliver(mention("1.0")) ==> true
      val t = w.inbox
        .ingested(Origin.Slack(Team, "C123ABC456", "1.0"), SourceId("1.0"))
        .getOrElse(None)
        .getOrElse(throw new java.lang.AssertionError("no turn"))
      w.inbox.finish(t, Some(reply("done")), "replied")
      open.deliver() ==> Right(1)
      w.slack.posts.map(_.post.fallback) ==> Vector("done")
      w.slack.closed ==> false
      open.close()
      w.slack.closed ==> true
    }

    test(
      "serving with posts registers service:slack, advertises slack_post over the channels Slack names, and runs a request sent there"
    ) {
      val w = new World
      w.slack.channelNames = w.slack.channelNames.updated(Skynet, "probably-not-skynet")
      val open = w.openPosting(Posts(TwoAnHour, Skynet, Unnamed))
      try {
        val expected =
          Posting.of(
            w.slack,
            Clock.system(),
            TwoAnHour,
            Vector(("probably-not-skynet", Skynet))
          ) match {
            case Some(p) => Some(p.offered.id)
            case None => None
          }
        w.edges.adverts.toVector.map((at, advert) => (at._2, Some(advert.tools))) ==>
          Vector((SlackEdge.PostsAt.place, expected))
        w.logged.filter(_.contains("post")) ==> Vector(
          "slack: not posting to C0UNNAMED1: Slack gives grit no name for it",
          "slack: posts to #probably-not-skynet (C0C5U2FPAL8), at most 2 per 1 hour"
        )
        val q = PostingTests.request(
          ujson.Obj("channel" -> "probably-not-skynet", "text" -> "the build is green"),
          0
        )
        val _ = w.edges.dispatch(Vector(q))(using TestTx.fake)
        val deadline = System.nanoTime() + 10_000_000_000L
        while (w.edges.told.isEmpty && System.nanoTime() < deadline) Thread.sleep(20)
        (w.edges.told, w.slack.posts.map(p => (p.channel, p.post.fallback))) ==> (
          Vector((q.slot, Outcome.Done("Posted in #probably-not-skynet."))),
          Vector((Skynet, "the build is green"))
        )
      } finally open.close()
    }

    test(
      "serving with posts none of whose channels Slack names serves nothing there, and says so"
    ) {
      val w = new World
      val open = w.openPosting(Posts(TwoAnHour, Unnamed))
      try {
        (w.edges.adverts, w.logged.filter(_.contains("post"))) ==> (
          Map.empty,
          Vector(
            "slack: not posting to C0UNNAMED1: Slack gives grit no name for it",
            "slack: posts nowhere: no channel it may post to has a name grit can read"
          )
        )
      } finally open.close()
    }

    test("serving refuses when Slack will not say who grit is, and closes the connection") {
      val w = new World
      w.slack.down = true
      refusal(Served.serving(Set(C), None, w.connect).open(w.stores, Env, _ => ())) ==>
        Some(EdgeRefusal.Refused("Slack refused the bot token: Unreachable(down)"))
      w.slack.closed ==> true
    }

    test("serving cannot answer a tool call that asks first") {
      Served.serving(Set(C), None, new World().connect).answersAsks ==> false
    }

    test(
      "backfill reads each channel's unheard threads as their messages' lengths, and hears them on hear"
    ) {
      val w = new World
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("1.0"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "is the freeze on?"),
          Listed(Ts("1.1"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "it is"),
          Listed(Ts("2.0"), None, Some(UserId(Ana)), false, None, "lunch?")
        )
      )
      val now = Instant.ofEpochSecond(86_400 * 3)
      val open = Served.backfill(Set(C), 3, w.connect).open(w.stores, Env, now, _ => ()) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
      (open.since, open.unheard) ==>
        (Instant.EPOCH, Vector(Unheard("#standup (C123ABC456)", Vector(Vector(17, 5), Vector(6)))))
      open.hear() ==> Right(())
      Served.backfill(Set(C), 3, w.connect).open(w.stores, Env, now, _ => ()) match {
        case Right(again) =>
          again.unheard ==> Vector(Unheard("#standup (C123ABC456)", Vector.empty))
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
      open.close()
      w.slack.closed ==> true
    }

    test("backfill of no channel is refused before Slack is asked") {
      val w = new World
      w.slack.down = true
      refusal(
        Served.backfill(Set.empty, 2, w.connect).open(w.stores, Env, Instant.EPOCH, _ => ())
      ) ==>
        Some(EdgeRefusal.Refused("Slack listens in no channel: there is nothing to backfill"))
    }
  }
}
