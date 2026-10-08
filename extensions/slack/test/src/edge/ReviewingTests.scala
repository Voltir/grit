package grit.slack.edge

import java.time.Instant

import grit.core.admin.InMemoryAdministration
import grit.core.edge.InMemoryJoins
import grit.core.edge.{
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges
}
import grit.core.id.{EdgeName, EntryId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.InMemoryInbox
import grit.core.review.{Label, Prompt, Reason, Reviews, Verdict}
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.slack.client.{FakeSlack, Self, Tag}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText

import utest.*

/** [[SlackEdge]] answering a review: the prompts it posts as it delivers, and the reactions
  * to them it keeps, over the in-memory stores and a fake Slack.
  */
object ReviewingTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val C = ChannelId("C123ABC456")

  private val Grit: SlackCommand =
    SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)

  /** The review's place: a private channel grit's bot is in. */
  private val Place = ChannelId("C0REVIEW1")

  /** The review's rater. */
  private val Nick = "U0NICK001"

  private val Review =
    SlackReview
      .of(
        grit.core.place.Place.under(grit.core.place.Namespace.Slack, Vector(Team, "C0REVIEW1")),
        UserId(Nick)
      )
      .fold(why => throw new java.lang.AssertionError(why), identity)

  /** Reviews as `under` keeps them, but `posted` first notes what `reactions` says grit's
    * reactions on Slack are at that moment, and fails, as a crash before it would, while
    * `crash` is set.
    */
  private final class Watched(under: Reviews) extends Reviews {
    // Set once, by the world that made this fake, before any edge call reads it; it reads a
    // fake Slack that dies with that world.
    @caps.unsafe.untrackedCaptures
    var reactions: () -> Set[(ChannelId, Ts, String)] = () => Set.empty
    // Written by the edge's calls, which the test makes and waits on, on its own thread, and
    // read by that test once they return; no other code holds this fake.
    @caps.unsafe.untrackedCaptures
    var reactionsWhenKept = Vector.empty[Set[(ChannelId, Ts, String)]]
    // Set and cleared by the test around one edge call it makes on its own thread.
    @caps.unsafe.untrackedCaptures
    var crash = false

    /** Each place [[unposted]] was asked for, in order. */
    // Written as `reactionsWhenKept` is: by the edge's calls on the test's thread, read after.
    @caps.unsafe.untrackedCaptures
    var askedFor = Vector.empty[grit.core.place.Place]
    def unposted(to: grit.core.place.Place)(using Tx^): Either[StoreError, Vector[Prompt]] = {
      askedFor = askedFor :+ to
      under.unposted(to)
    }
    def posted(entry: EntryId, address: String, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = {
      reactionsWhenKept = reactionsWhenKept :+ reactions()
      if (crash) Left(StoreError.DatabaseError("crashed"))
      else under.posted(entry, address, at)
    }
    def reacted(address: String, rater: Account, verdict: Verdict, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = under.reacted(address, rater, verdict, at)
    def unreacted(address: String, rater: Account, verdict: Verdict)(using
        Tx^
    ): Either[StoreError, Boolean] = under.unreacted(address, rater, verdict)
  }

  private final class World {
    val slack = new FakeSlack
    slack.channelNames = Map(C -> "standup", Place -> "debug")
    slack.privateChannels = Set(Place)
    val inbox: InMemoryInbox = InMemoryInbox.fresh()
    val picks = new PickedPrompts(inbox)
    val watched = new Watched(picks.reviews)
    // The fake Slack outlives neither the world that made it nor this reader of it.
    watched.reactions = caps.unsafe.unsafeAssumePure(() => slack.reactions)

    // Appended to by the edge's log, called only inside the calls the test makes on its own
    // thread, and read by that test once they return.
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    val edge: SlackEdge^ = new SlackEdge(
      slack,
      Self(TeamId(Team), UserId(Bot)),
      EdgeStores(
        inbox,
        InMemoryAdministration.none(),
        InMemoryJoins.none(),
        inbox.principals,
        new InMemoryDeliveries,
        new InMemoryAcknowledgements,
        watched,
        FakeJot,
        new InMemoryEdges,
        new Attesting(InMemoryVoucher.none(), FakeJot, _ => ())
      ),
      Some(Review),
      grit.core.clock.Clock.system(),
      s => logged = logged :+ s
    )
    val _ = slack.listen(edge.receive, _ => ())

    /** Message `ts` said at the top of C, known to Slack, heard, and picked as `reason`. */
    def picked(ts: String, reason: Reason = Reason.ShadowOnly): EntryId = {
      known(C, ts)
      picks.pick(Origin.Slack(Team, ChannelId.value(C), ts), ts, reason)
    }

    def known(channel: ChannelId, ts: String): Unit =
      slack.histories = slack.histories.updated(
        channel,
        slack.histories.getOrElse(channel, Vector.empty) :+
          Listed(Ts(ts), None, Some(UserId(Ana)), false, None, "hm")
      )

    def unposted: Vector[EntryId] =
      picks.reviews
        .unposted(Review.place)(using TestTx.fake)
        .fold(e => sys.error(e.toString), _.map(_.entry))

    /** The label standing on `entry`'s prompt. */
    def label(entry: EntryId): Option[Label] =
      picks.reviews
        .reviewed(Instant.EPOCH)(using TestTx.fake)
        .fold(e => sys.error(e.toString), identity)
        .find(_.entry == entry)
        .flatMap(_.label)

    /** The ts the prompt of `entry` was posted at. */
    def promptOf(entry: EntryId): Ts =
      slack.posts
        .find(_.tag == Tag.Prompt(EntryId.value(entry)))
        .fold(throw new java.lang.AssertionError(s"no prompt for $entry"))(_.ts)
  }

  private val Said = "1515449522.000016"

  val tests = Tests {
    test("a pass asks the store only for the prompts its review's place may receive") {
      val w = new World
      val _ = w.picked(Said)
      val _ = w.edge.prompt()
      w.watched.askedFor ==> Vector(Review.place)
    }

    test(
      "a picked message heard in this workspace's Slack is prompted at the top of the place, its permalink linked, wearing the three reactions, then kept as posted"
    ) {
      val w = new World
      val entry = w.picked(Said)
      val first = w.edge.prompt()
      val shown = RichText.render(
        ReviewPrompt.doc(
          "#standup",
          s"https://fake.slack.com/archives/C123ABC456/p1515449522000016"
        )
      )
      val ts = w.promptOf(entry)
      (
        first,
        w.slack.posts.map(p => (p.channel, p.thread == p.ts, Vector(p.post), p.tag)),
        w.slack.reactions,
        w.unposted,
        w.edge.prompt(),
        w.slack.posts.size
      ) ==> (
        Right(1),
        Vector((Place, true, shown, Tag.Prompt(EntryId.value(entry)))),
        Set((Place, ts, "+1"), (Place, ts, "-1"), (Place, ts, "bust_in_silhouette")),
        Vector.empty,
        Right(0),
        1
      )
    }

    test("a prompt is kept as posted only once its three reactions are on it") {
      val w = new World
      val _ = w.picked(Said)
      val _ = w.edge.prompt()
      w.watched.reactionsWhenKept.map(_.map(_._3)) ==> Vector(Set("+1", "-1", "bust_in_silhouette"))
    }

    test(
      "a prompt kept as posted by no store, as after a crash, is posted again next round"
    ) {
      val w = new World
      val _ = w.picked(Said)
      w.watched.crash = true
      val crashed = w.edge.prompt()
      w.watched.crash = false
      (crashed, w.edge.prompt(), w.slack.posts.size, w.unposted) ==> (
        Right(0),
        Right(1),
        2,
        Vector.empty
      )
    }

    test(
      "a prompt whose message was heard in another workspace or no Slack is never posted; one Slack cannot link waits for a later round"
    ) {
      val w = new World
      val task = w.picks.pick(Origin.Task("daily", "1"), Said)
      val elsewhere = w.picks.pick(Origin.Slack("T999", ChannelId.value(C), "2.0"), "2.0")
      val unlinked = w.picks.pick(Origin.Slack(Team, ChannelId.value(C), "3.0"), "3.0")
      val before = (w.edge.prompt(), w.slack.posts.size)
      w.known(C, "3.0")
      (before, w.edge.prompt(), w.slack.posts.map(_.tag), w.unposted.toSet) ==> (
        (Right(0), 0),
        Right(1),
        Vector(Tag.Prompt(EntryId.value(unlinked))),
        Set(task, elsewhere)
      )
    }

    test(
      "the rater's reaction on a prompt is kept as its verdict; a later one replaces it; removing the one standing withdraws it, removing another changes nothing"
    ) {
      val w = new World
      val entry = w.picked(Said)
      val _ = w.edge.prompt()
      val ts = Ts.value(w.promptOf(entry))
      def react(emoji: String, added: Boolean, at: String): (Boolean, Option[Label]) = {
        val acked =
          w.slack.deliver(reaction(ts, emoji, added, user = Nick, channel = "C0REVIEW1", at = at))
        (acked, w.label(entry))
      }
      val rater = TestAccounts.account(s"slack:$Team/$Nick")
      Vector(
        react("+1", added = true, "1515449600.000000"),
        react("-1::skin-tone-2", added = true, "1515449601.000000"),
        react("+1", added = false, "1515449602.000000"),
        react("-1::skin-tone-2", added = false, "1515449603.000000")
      ) ==> Vector(
        (true, Some(Label(Verdict.Welcome, rater, Instant.ofEpochSecond(1515449600L)))),
        (true, Some(Label(Verdict.Interruption, rater, Instant.ofEpochSecond(1515449601L)))),
        (true, Some(Label(Verdict.Interruption, rater, Instant.ofEpochSecond(1515449601L)))),
        (true, None)
      )
    }

    test(
      "only the rater's three reactions on a prompt in the place count: another person's, grit's own, another emoji, and the rater's elsewhere are ignored"
    ) {
      val w = new World
      val entry = w.picked(Said)
      val _ = w.edge.prompt()
      val ts = Ts.value(w.promptOf(entry))
      w.known(Place, "9.0")
      val ignored = Vector(
        reaction(ts, "+1", user = Ana, channel = "C0REVIEW1"),
        reaction(ts, "+1", user = Bot, channel = "C0REVIEW1"),
        reaction(ts, "eyes", user = Nick, channel = "C0REVIEW1"),
        reaction(ts, "+1", user = Nick, channel = "C123ABC456"),
        reaction("9.0", "+1", user = Nick, channel = "C0REVIEW1")
      ).map(w.slack.deliver)
      val ignoredLabel = w.label(entry)
      val _ =
        w.slack.deliver(reaction(ts, "bust_in_silhouette", user = Nick, channel = "C0REVIEW1"))
      (ignored, ignoredLabel, w.label(entry).map(_.verdict)) ==> (
        Vector(true, true, true, true, true),
        None,
        Some(Verdict.CutIn)
      )
    }
  }
}
