package grit.slack.edge

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.admin.{Answer, InMemoryAdministration}
import grit.core.clock.{Clock, SetClock}
import grit.core.edge.{
  Acknowledgement,
  Acknowledgements,
  Attesting,
  EdgeRefusal,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  Membership,
  ServedEdge,
  Unheard,
  Variable
}
import grit.core.id.{SourceId, TurnRef}
import grit.core.identity.{Standing, TestAccounts, Vouched}
import grit.core.inbox.InMemoryInbox
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.{Namespace, Place}
import grit.core.review.Reason
import grit.core.speech.Rate
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.tool.Outcome
import grit.core.visibility.{RoomAccess, Subject, Visibility}
import grit.dbos.sql.TestTx
import grit.slack.client.{AppToken, BotToken, FakeSlack, Slack, SlackError}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}

import utest.*

/** [[SlackEdge.serving]] and [[SlackEdge.backfill]] as a deployment's kit opens them, over the
  * in-memory stores and a fake Slack standing in for Socket Mode.
  */
object ServedTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val C = ChannelId("C123ABC456")

  /** The slash command each edge here answers. */
  private val Grit: SlackCommand =
    SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)

  private val Env = Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "xapp-1")

  private val Skynet = ChannelId("C0C5U2FPAL8")

  /** A channel whose id sorts before [[C]]'s. */
  private val First = ChannelId("C0AAAAAAAA1")

  /** The room a thread of `channel` in grit's team is in. */
  private def roomOf(channel: ChannelId): Place =
    Origin.Slack(Team, ChannelId.value(channel), "1.0").room

  /** A channel grit's bot was in, and is no longer. */
  private val Old = ChannelId("C0OLD00001")

  /** Every room of grit's team. */
  private val Workspace: Place = Place.under(Namespace.Slack, Vector(Team))

  /** A channel Slack gives no name. */
  private val Unnamed = ChannelId("C0UNNAMED1")

  private val TwoAnHour: Rate =
    Rate.of(2, 1.hour).getOrElse(throw new java.lang.AssertionError("a rate"))

  /** Runs each join backfill at once, on the thread that wakes it, so a test reads what it did
    * as soon as the open or delivery that woke it returns.
    */
  private object Inline extends Backfilling.Start {
    def apply(body: () => Unit): Backfilling.Running = {
      body()
      new Backfilling.Running {
        def alive: Boolean = false
        def join(within: FiniteDuration): Unit = ()
      }
    }
  }

  /** The accounts of grit's team, which a world that attests answers for. */
  private val Ours =
    SlackAccounts.realm(TeamId(Team)).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The stores of an edge, over a voucher of grit's team's accounts when it `attests`. */
  private final class World(attests: Boolean = false) {
    val slack = new FakeSlack

    /** The clock every edge of this world is opened with: the epoch, until a test moves it. */
    val clock: SetClock = new SetClock(Instant.EPOCH)
    val voucher =
      new InMemoryVoucher(if (attests) Set(Ours) else Set.empty, Set.empty, Visibility.Shipped)
    val inbox: InMemoryInbox = InMemoryInbox.fresh(Budget(ZoneOffset.UTC, None))
    val edges: InMemoryEdges = new InMemoryEdges
    val picks = new PickedPrompts(inbox)
    val acknowledgements = new InMemoryAcknowledgements

    /** Where its edges record their bot's memberships. */
    val joins: InMemoryJoins = InMemoryJoins.none()
    val stores =
      EdgeStores(
        inbox,
        InMemoryAdministration.none(),
        joins,
        inbox.principals,
        new InMemoryDeliveries,
        acknowledgements,
        picks.reviews,
        FakeJot,
        edges,
        new Attesting(voucher, FakeJot, _ => ())
      )

    /** What opening logged, in order. */
    @caps.unsafe.untrackedCaptures
    var logged: Vector[String] = Vector.empty

    /** The edge as `serving` makes it, opened on `at`; logged in [[logged]]. */
    def openAt(at: Clock^): ServedEdge.Open^{this, at} =
      Served
        .serving(Grit, Backfill.Default, None, None, connect, Inline)
        .open(stores, Env, at, line => logged :+= line) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }

    /** The edge as `serving` makes it, posting as `posts` allows, opened; logged in [[logged]]. */
    def openPosting(posts: Posts): ServedEdge.Open^{this} =
      Served
        .serving(Grit, Backfill.Default, Some(posts), None, connect, Inline)
        .open(stores, Env, clock, line => logged :+= line) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
    val connect: Served.Connect^{slack} = new Served.Connect {
      def apply(bot: BotToken, app: AppToken): Slack^ = slack
    }
  }

  /** `under`, keeping the time of each mark put up. */
  private final class DatedAcknowledgements(under: InMemoryAcknowledgements)
      extends Acknowledgements {

    @caps.unsafe.untrackedCaptures
    var shownAt = Vector.empty[Instant]

    def want(turn: TurnRef, to: String, at: Instant)(using Tx^): Either[StoreError, Unit] =
      under.want(turn, to, at)
    def standing()(using Tx^): Either[StoreError, Vector[Acknowledgement]] = under.standing()
    def shown(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit] = {
      shownAt :+= at
      under.shown(turn, at)
    }
    def cleared(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit] =
      under.cleared(turn, at)
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

  /** The review at `slack:{team}/{channel}`, rated by U0NICK001. */
  private def reviewAt(team: String, channel: String): SlackReview =
    SlackReview
      .of(Place.under(Namespace.Slack, Vector(team, channel)), UserId("U0NICK001"))
      .fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test(
      "a review's place is a channel's or a DM's, slack:{team}/{id}; any other is refused, naming it"
    ) {
      def of(text: String) =
        SlackReview
          .of(Place.read(text).fold(sys.error, identity), UserId("U0NICK001"))
          .map(r => (r.team, r.channel))
      (
        of("slack:T1/C0REVIEW1"),
        of("slack:T1/D0NICK001"),
        of("service:slack"),
        of("slack:T1"),
        of("slack:T1/#review"),
        of("slack:T1/C0REVIEW1/1.0")
      ) ==> (
        Right((TeamId("T1"), ChannelId("C0REVIEW1"))),
        Right((TeamId("T1"), ChannelId("D0NICK001"))),
        Left("a review's place is slack:{team}/{channel id}, not service:slack"),
        Left("a review's place is slack:{team}/{channel id}, not slack:T1"),
        Left("a review's place is slack:{team}/{channel id}, not slack:T1/#review"),
        Left("a review's place is slack:{team}/{channel id}, not slack:T1/C0REVIEW1/1.0")
      )
    }

    test(
      "serving's open records grit's bot a member of each channel Slack lists it in and gone from each it no longer does, and says which"
    ) {
      val w = new World
      w.slack.channelNames = w.slack.channelNames ++ Map(First -> "general", Old -> "old")
      w.slack.privateChannels = Set(First)
      w.slack.notIn = Set(Old)
      w.joins.joined(roomOf(Old), RoomAccess.Open, None, Instant.EPOCH) ==>
        Right(Membership.Member(Instant.EPOCH, Some(Instant.EPOCH)))
      val now = Instant.ofEpochSecond(86_400)
      val open = w.openAt(new SetClock(now))
      open.close()
      (
        w.joins.members(Workspace),
        w.joins.access(roomOf(First)),
        w.logged.filter(_.contains("member"))
      ) ==> (
        Right(
          Vector(
            // Each join's backfill run at open, with nothing said before it.
            roomOf(First) -> Membership.Member(now, None),
            roomOf(C) -> Membership.Member(now, None)
          )
        ),
        Some(RoomAccess.Invited),
        Vector("slack: a member of #general (C0AAAAAAAA1), #standup (C123ABC456)")
      )
    }

    test(
      "serving's open forgets a room left over a day ago, so a late join of it counts, and keeps one left within the day"
    ) {
      val w = new World
      val now = Instant.ofEpochSecond(86_400 * 3)
      w.joins.left(roomOf(Old), now.minusSeconds(86_400 + 1)) ==> Right(Membership.Gone)
      w.joins.left(roomOf(First), now.minusSeconds(3_600)) ==> Right(Membership.Gone)
      w.openAt(new SetClock(now)).close()
      val before = Instant.EPOCH
      (
        w.joins.joined(roomOf(Old), RoomAccess.Open, None, before),
        w.joins.joined(roomOf(First), RoomAccess.Open, None, before),
        w.logged.filter(_.contains("forgot"))
      ) ==> (
        Right(Membership.Member(before, Some(before))),
        Right(Membership.Gone),
        Vector("slack: forgot 1 rooms left over a day ago")
      )
    }

    test("serving refuses to open when Slack will not list grit's channels, closing Slack") {
      val w = new World
      w.slack.unlisted = Some(SlackError.Unreachable("gone"))
      (
        refusal(
          Served
            .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
            .open(w.stores, Env, w.clock, _ => ())
        ),
        w.slack.closed
      ) ==> (
        Some(EdgeRefusal.Refused("Slack would not list grit's channels: Unreachable(gone)")),
        true
      )
    }

    test("serving's open never records the review's channel a member, and says it is never heard") {
      val w = new World
      val review = reviewAt(Team, "C123ABC456")
      Served
        .serving(Grit, Backfill.Default, None, Some(review), w.connect, Inline)
        .open(w.stores, Env, w.clock, w.logged :+= _) match {
        case Right(o) => o.close()
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
      (w.joins.members(Workspace), w.logged.filter(_.contains("review"))) ==> (
        Right(Vector.empty),
        Vector("slack: the review's channel, #standup (C123ABC456), is never heard")
      )
    }

    test("serving posts review prompts at its review's place, and nowhere without one") {
      val review = reviewAt(Team, "C0REVIEW1")
      (
        Served
          .serving(Grit, Backfill.Default, None, Some(review), new World().connect, Inline)
          .reviewsAt,
        Served
          .serving(
            Grit,
            Backfill.Default,
            Some(Posts(TwoAnHour, Skynet)),
            None,
            new World().connect,
            Inline
          )
          .reviewsAt
      ) ==> (Some(Place.under(Namespace.Slack, Vector(Team, "C0REVIEW1"))), None)
    }

    test("serving refuses to open when its review's team is not the bot's, closing Slack") {
      val w = new World
      refusal(
        Served
          .serving(
            Grit,
            Backfill.Default,
            None,
            Some(reviewAt("T0OTHER01", "C0REVIEW1")),
            w.connect,
            Inline
          )
          .open(w.stores, Env, w.clock, _ => ())
      ) ==> Some(
        EdgeRefusal.Refused(
          s"the review's place, slack:T0OTHER01/C0REVIEW1, is not in grit's team, $Team"
        )
      )
      w.slack.closed ==> true
    }

    test(
      "serving hears what was said before grit's bot joined each channel it is in at open, and one it joins after, as it delivers"
    ) {
      val w = new World
      val Ops = ChannelId("C0OPS00001")
      w.clock.at = Instant.ofEpochSecond(1000L)
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("900.000000"), None, Some(UserId(Ana)), false, None, "standup moved")
        ),
        Ops -> Vector(Listed(Ts("950.000000"), None, Some(UserId(Ana)), false, None, "deploy at 3"))
      )
      val open = w.openAt(w.clock)
      val atOpen = w.inbox.conversations.all.map(_.origin.room)
      w.slack.channelNames = w.slack.channelNames.updated(Ops, "ops")
      w.slack.deliver(
        joined(channel = ChannelId.value(Ops), inviter = "", eventTs = Some("1000.500000"))
      ) ==> true
      val beforeDelivery = w.inbox.conversations.all.size
      open.deliver() ==> Right(0)
      open.close()
      (
        atOpen,
        beforeDelivery,
        w.inbox.conversations.all.map(_.origin.room).toSet,
        w.joins.members(Workspace).map(_.map(_._2.backfill)),
        w.logged.filter(_.contains("before grit's bot joined"))
      ) ==> (
        Vector(roomOf(C)),
        1,
        Set(roomOf(C), roomOf(Ops)),
        Right(Vector(None, None)),
        Vector(
          "slack: heard 1 messages said in #standup (C123ABC456) before grit's bot joined",
          "slack: heard 1 messages said in #ops (C0OPS00001) before grit's bot joined"
        )
      )
    }

    test("serving refuses a token unset or of the wrong kind by its variable, never quoting it") {
      val w = new World
      val edge = Served.serving(Grit, Backfill.Default, None, None, w.connect, Inline)
      def refused(env: Map[String, String]) = refusal(edge.open(w.stores, env, w.clock, _ => ()))
      refused(Map("SLACK_APP_TOKEN" -> "xapp-1")) ==>
        Some(EdgeRefusal.Missing(Variable("SLACK_BOT_TOKEN")))
      refused(Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "sekrit")) ==>
        Some(EdgeRefusal.Malformed(Variable("SLACK_APP_TOKEN"), "an app-level token starts xapp-"))
      edge.needs ==> Vector(Variable("SLACK_BOT_TOKEN"), Variable("SLACK_APP_TOKEN"))
    }

    test("serving with a review posts its prompts, with their reactions, as it delivers") {
      val w = new World
      w.slack.histories =
        Map(C -> Vector(Listed(Ts("1.0"), None, Some(UserId(Ana)), false, None, "hm")))
      val entry = w.picks.pick(Origin.Slack(Team, "C123ABC456", "1.0"), "1.0", Reason.Both)
      val review = reviewAt(Team, "C0REVIEW1")
      val open =
        Served
          .serving(Grit, Backfill.Default, None, Some(review), w.connect, Inline)
          .open(w.stores, Env, w.clock, _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      open.deliver() ==> Right(0)
      (
        w.slack.posts.map(p => (p.channel, p.tag)),
        w.slack.reactions.map((c, _, emoji) => (c, emoji))
      ) ==> (
        Vector((review.channel, grit.slack.client.Tag.Prompt(grit.core.id.EntryId.value(entry)))),
        Set(
          (review.channel, "+1"),
          (review.channel, "-1"),
          (review.channel, "bust_in_silhouette")
        )
      )
    }

    test("serving answers its slash command, and only it, to its asker through Slack") {
      val w = new World
      val _ =
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores, Env, w.clock, _ => ())
      (w.slack.command(command("help")), w.slack.command(command("help", name = "/other"))) ==>
        (true, true)
      w.slack.responses ==> Vector((grit.slack.event.ResponseUrl(Hook), Answer.Help.text))
    }

    test("serving puts up a wanted acknowledgement's mark as it delivers") {
      val w = new World
      val open =
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores, Env, w.clock, _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      w.slack.deliver(message("2.0", "Pip, what did we decide?")) ==> true
      val heard = w.inbox.conversations.all
        .find(_.origin == Origin.Slack(Team, "C123ABC456", "2.0"))
        .map(c => grit.core.id.TurnRef(c.id, grit.core.id.TurnSeq.First))
        .getOrElse(throw new java.lang.AssertionError("not heard"))
      w.acknowledgements.want(heard, "C123ABC456/2.0/2.0", java.time.Instant.EPOCH)(using
        TestTx.fake
      ) ==> Right(())
      open.deliver() ==> Right(0)
      w.slack.reactions ==> Set((C, Ts("2.0"), "eyes"))
    }

    test("serving dates the marks it puts up by the clock it is opened with") {
      val w = new World
      val at = Instant.parse("2026-10-07T09:30:00Z")
      w.clock.at = at
      val dated = new DatedAcknowledgements(w.acknowledgements)
      val open =
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores.copy(acknowledgements = dated), Env, w.clock, _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      w.slack.deliver(message("2.0", "Pip, what did we decide?")) ==> true
      val heard = w.inbox.conversations.all
        .find(_.origin == Origin.Slack(Team, "C123ABC456", "2.0"))
        .map(c => grit.core.id.TurnRef(c.id, grit.core.id.TurnSeq.First))
        .getOrElse(throw new java.lang.AssertionError("not heard"))
      dated.want(heard, "C123ABC456/2.0/2.0", Instant.EPOCH)(using TestTx.fake) ==> Right(())
      open.deliver() ==> Right(0)
      open.close()
      dated.shownAt ==> Vector(at)
    }

    test(
      "serving, opened, logs its bot's Slack name, takes a mention as a turn, posts its reply, and closes Slack"
    ) {
      val w = new World
      w.slack.names = w.slack.names.updated(UserId(Bot), Some("Pip"))
      var logged: Vector[String] = Vector.empty
      val open =
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores, Env, w.clock, logged :+= _) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      logged.filter(_.contains("named")) ==> Vector("slack: grit's bot is named Pip in Slack")
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
            TeamId(Team),
            Vector(("probably-not-skynet", Skynet))
          ) match {
            case Right(p) => Some(p.offered.id)
            case Left(_) => None
          }
        w.edges.adverts.toVector.map((at, advert) => (at._2, Some(advert.tools))) ==>
          Vector((SlackEdge.PostsAt.place, expected))
        w.logged.filter(_.contains("post")) ==> Vector(
          "slack: not posting to C0UNNAMED1: Slack gives grit no name for it",
          "slack: posts to #probably-not-skynet (C0C5U2FPAL8), at most 2 per 1 hour"
        )
        val q = PostingTests.request(ujson.Obj("text" -> "the build is green"), 0)
        val _ = w.edges.dispatch(Vector(q))(using TestTx.fake)
        val deadline = System.nanoTime() + 10_000_000_000L
        while (w.edges.answers.isEmpty && System.nanoTime() < deadline) Thread.sleep(20)
        (w.edges.answers, w.slack.posts.map(p => (p.channel, p.post.fallback))) ==> (
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
      refusal(
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores, Env, w.clock, _ => ())
      ) ==>
        Some(EdgeRefusal.Refused("Slack refused the bot token: Unreachable(down)"))
      w.slack.closed ==> true
    }

    test("serving and backfill each say they are the Slack attester") {
      val w = new World
      (
        Served.serving(Grit, Backfill.Default, None, None, w.connect, Inline).attester,
        Served.backfill(1, w.connect).attester
      ) ==>
        (Some(SlackAccounts.Attester), Some(SlackAccounts.Attester))
    }

    test(
      "an edge served looks, when asked, through its own source: a due account's team listed once"
    ) {
      val w = new World(attests = true)
      w.voucher.saw(TestAccounts.account(s"slack:$Team/$Ana"))
      val open =
        Served
          .serving(Grit, Backfill.Default, None, None, w.connect, Inline)
          .open(w.stores, Env, w.clock, _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      (open.attest(), w.slack.listings) ==> (Right(1), 1)
      open.close()
    }

    test("a backfill hears each author once Slack has answered for them") {
      val w = new World(attests = true)
      w.slack.histories =
        Map(C -> Vector(Listed(Ts("1.0"), None, Some(UserId(Ana)), false, None, "hm")))
      val open =
        Served.backfill(3, w.connect).open(w.stores, Env, w.clock, _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      open.hear() ==> Right(())
      w.voucher.vouched ==> Vector(
        Vouched(TestAccounts.account(s"slack:$Team/$Ana"), Standing.Full(None))
      )
      open.close()
    }

    test(
      "the team the bot token is installed in is Slack's answer to who grit is; refused as opening is, Slack closed"
    ) {
      val w = new World
      val installed = Served.installedIn(Env, w.connect)
      val closed = w.slack.closed
      val down = new World
      down.slack.down = true
      (
        installed,
        closed,
        Served.installedIn(Map("SLACK_APP_TOKEN" -> "xapp-1"), w.connect),
        Served.installedIn(Env, down.connect)
      ) ==> (
        Right(TeamId(Team)),
        true,
        Left(EdgeRefusal.Missing(Variable("SLACK_BOT_TOKEN"))),
        Left(EdgeRefusal.Refused("Slack refused the bot token: Unreachable(down)"))
      )
    }

    test("serving cannot answer a tool call that asks first") {
      Served
        .serving(Grit, Backfill.Default, None, None, new World().connect, Inline)
        .answersAsks ==> false
    }

    test(
      "backfill reads each channel's unheard threads as their messages' lengths, in channel id order, each in its channel's room, and hears them on hear"
    ) {
      val w = new World
      w.slack.channelNames = w.slack.channelNames + (First -> "general")
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("1.0"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "is the freeze on?"),
          Listed(Ts("1.1"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "it is"),
          Listed(Ts("2.0"), None, Some(UserId(Ana)), false, None, "lunch?")
        ),
        First -> Vector(Listed(Ts("3.0"), None, Some(UserId(Ana)), false, None, "hello"))
      )
      val now = Instant.ofEpochSecond(86_400 * 3)
      val open =
        Served
          .backfill(3, w.connect)
          .open(w.stores, Env, new SetClock(now), _ => ()) match {
          case Right(o) => o
          case Left(r) => throw new java.lang.AssertionError(r.message)
        }
      (open.since, open.unheard) ==> (
        Instant.EPOCH,
        Vector(
          Unheard("#general (C0AAAAAAAA1)", roomOf(First), Vector(Vector(5))),
          Unheard("#standup (C123ABC456)", roomOf(C), Vector(Vector(17, 5), Vector(6)))
        )
      )
      open.hear() ==> Right(())
      Served
        .backfill(3, w.connect)
        .open(w.stores, Env, new SetClock(now), _ => ()) match {
        case Right(again) =>
          again.unheard ==> Vector(
            Unheard("#general (C0AAAAAAAA1)", roomOf(First), Vector.empty),
            Unheard("#standup (C123ABC456)", roomOf(C), Vector.empty)
          )
        case Left(r) => throw new java.lang.AssertionError(r.message)
      }
      open.close()
      w.slack.closed ==> true
    }

    test("backfill of a channel Slack will not read is refused by it, and closes the connection") {
      val w = new World
      w.slack.histories =
        Map(C -> Vector(Listed(Ts("1.0"), None, Some(UserId(Ana)), false, None, "hi")))
      w.slack.channelNames = w.slack.channelNames + (First -> "general")
      w.slack.unreachable = Set(First)
      refusal(
        Served.backfill(3, w.connect).open(w.stores, Env, w.clock, _ => ())
      ) ==> Some(EdgeRefusal.Refused("C0AAAAAAAA1 not read: Unreachable(gone)"))
      w.slack.closed ==> true
    }

    test(
      "backfill is refused when grit's bot is a member of no channel, and closes the connection"
    ) {
      val w = new World
      w.slack.notIn = Set(C)
      refusal(
        Served.backfill(2, w.connect).open(w.stores, Env, w.clock, _ => ())
      ) ==> Some(
        EdgeRefusal.Refused("grit's bot is a member of no channel: there is nothing to backfill")
      )
      w.slack.closed ==> true
    }
  }
}
