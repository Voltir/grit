package grit.slack.edge

import java.time.ZoneOffset

import scala.concurrent.duration.*

import grit.core.edge.{
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges
}
import grit.core.id.SourceId
import grit.core.identity.{Account, Email, Realm, Standing, TestAccounts, Vouched}
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.sql.TestTx
import grit.slack.client.{FakeSlack, Self, SlackError}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}

import utest.*

/** The Slack edge as its realm's source: what it asks Slack, when, and what it does with the
  * message whose author it checks, over the in-memory stores, a fake Slack, and the in-memory
  * voucher of grit's own team, which keeps each answer it is given.
  */
object SlackAttestingTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val C = ChannelId("C123ABC456")

  private val Ours: Realm =
    SlackAccounts.realm(TeamId(Team)).fold(e => throw new java.lang.AssertionError(e), identity)

  private def of(user: String, team: String = Team): Account =
    TestAccounts.account(s"slack:$team/$user")

  private val ana = of(Ana)

  /** Two more people of grit's team. */
  private val Ben = "U0BEN0001"
  private val Cy = "U0CY00001"

  private final class World {
    val slack = new FakeSlack
    slack.names = Map(
      UserId(Ana) -> Some("Ana Lima"),
      UserId(Ben) -> Some("Ben"),
      UserId(Cy) -> Some("Cy")
    )
    val inbox: InMemoryInbox = InMemoryInbox.fresh(Budget(ZoneOffset.UTC, None))
    val voucher = new InMemoryVoucher(Set(Ours), Set.empty, Visibility.Shipped)

    /** What the checks reported, in order. */
    @caps.unsafe.untrackedCaptures
    var reported = Vector.empty[Attesting.Report]

    /** What the edge said, in order. */
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    val edge: SlackEdge^ =
      new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot)),
        EdgeStores(
          inbox,
          inbox.principals,
          new InMemoryDeliveries,
          new InMemoryAcknowledgements,
          InMemoryReviews.over(inbox),
          FakeJot,
          new InMemoryEdges,
          new Attesting(voucher, FakeJot, r => reported = reported :+ r)
        ),
        Set(C),
        None,
        grit.core.clock.Clock.system(),
        s => logged = logged :+ s
      )
    val _ = slack.listen(edge.receive)

    /** Whether message `ts`, at the top of C, was recorded or heard. */
    def kept(ts: String): Boolean =
      inbox.recorded(Origin.Slack(Team, "C123ABC456", ts), Set(SourceId(ts))) ==
        Right(Set(SourceId(ts)))
  }

  val tests = Tests {
    test(
      "a new author's message is recorded once Slack has been asked about them, by one users.info, its answer kept"
    ) {
      val w = new World
      w.slack.deliver(message("1.0", "is the freeze on?")) ==> true
      (w.voucher.vouched, w.slack.asked, w.kept("1.0")) ==>
        (Vector(Vouched(ana, Standing.Full(None))), Vector(UserId(Ana)), true)
    }

    test(
      "a direct message's author is asked about before it is recorded; one Slack cannot answer for, never answered, is not recorded and left for Slack to send again"
    ) {
      val dm = Origin.Direct(TestAccounts.sourced(s"slack:$Team/$Ana"), "9.0")
      def recorded(w: World^): Boolean =
        w.inbox.recorded(dm, Set(SourceId("9.0"))) == Right(Set(SourceId("9.0")))
      val asked = new World
      val down = new World
      down.slack.unasked = Some(SlackError.Refused("missing_scope"))
      (
        asked.slack.deliver(direct("9.0", "hi")),
        asked.voucher.vouched,
        recorded(asked),
        down.slack.deliver(direct("9.0", "hi")),
        recorded(down)
      ) ==> (true, Vector(Vouched(ana, Standing.Full(None))), true, false, false)
    }

    test(
      "an author answered for less than a minute ago is not asked again, and one answered longer ago is"
    ) {
      val w = new World
      w.slack.deliver(message("1.0", "is the freeze on?")) ==> true
      w.slack.deliver(message("2.0", "and another")) ==> true
      val withinFresh = w.slack.asked
      w.voucher.aged(ana, Attesting.Fresh + 1.second)
      w.slack.deliver(message("3.0", "later")) ==> true
      (withinFresh, w.slack.asked) ==> (Vector(UserId(Ana)), Vector(UserId(Ana), UserId(Ana)))
    }

    test(
      "an author Slack cannot be asked about, never answered for, is not recorded, and the message is left for Slack to send again"
    ) {
      val refusals = Vector(SlackError.Limited(1.second), SlackError.Refused("missing_scope"))
      refusals.map { refusal =>
        val w = new World
        w.slack.unasked = Some(refusal)
        (w.slack.deliver(mention("1.0")), w.kept("1.0"), w.voucher.vouched)
      } ==> Vector.fill(2)((false, false, Vector.empty))
    }

    test(
      "an author Slack cannot be asked about, answered for before, is recorded under that answer, and that is warned"
    ) {
      val w = new World
      val _ = w.voucher.vouch(Vouched(ana, Standing.Full(None)))(using TestTx.fake)
      w.voucher.aged(ana, 5.minutes)
      w.slack.unasked = Some(SlackError.Unreachable("down"))
      (w.slack.deliver(mention("1.0")), w.kept("1.0"), w.reported) ==> (
        true,
        true,
        Vector(Attesting.Report.Unreached(Ours, 1, "Unreachable(down)"))
      )
    }

    test("a user Slack knows not at all is outside, and their message is still recorded") {
      val w = new World
      w.slack.deliver(mention("1.0", user = "U0NOBODY1")) ==> true
      (w.voucher.vouched, w.kept("1.0")) ==>
        (Vector(Vouched(of("U0NOBODY1"), Standing.Outside)), true)
    }

    test(
      "a message from a workspace sharing the channel is recorded, Slack asked only its author's name, never who they are"
    ) {
      val w = new World
      w.slack.deliver(
        message("1.0", "hi from next door", extra = Seq("user_team" -> "T0THEIRS"))
      ) ==>
        true
      (w.kept("1.0"), w.slack.asked, w.voucher.vouched) ==> (
        true,
        Vector(UserId(Ana)),
        Vector.empty
      )
    }

    test(
      "a user's change asks Slack again however recently it answered, and a deactivated user is recorded outside"
    ) {
      val w = new World
      w.slack.deliver(message("1.0", "is the freeze on?")) ==> true
      w.slack.standings = Map(UserId(Ana) -> Standing.Outside)
      w.slack.deliver(userChange()) ==> true
      (w.slack.asked, w.voucher.vouched) ==> (
        Vector(UserId(Ana), UserId(Ana)),
        Vector(Vouched(ana, Standing.Full(None)), Vouched(ana, Standing.Outside))
      )
    }

    test("a change to another workspace's user asks nothing") {
      val w = new World
      w.slack.deliver(userChange(team = Some("T0THEIRS"))) ==> true
      (w.slack.asked, w.slack.listings, w.voucher.vouched) ==> (Vector.empty, 0, Vector.empty)
    }

    test(
      "a look lists grit's team once and records only the accounts grit has seen, never enrolling the rest"
    ) {
      val w = new World
      w.voucher.saw(of(Ben))
      w.edge.attest() ==> Right(1)
      (w.slack.listings, w.slack.asked, w.voucher.vouched) ==>
        (1, Vector.empty, Vector(Vouched(of(Ben), Standing.Full(None))))
    }

    test(
      "a full member's confirmed address is passed on whatever its domain: the edge claims none"
    ) {
      val w = new World
      val address = Email.of("ana@elsewhere.example").toOption
      w.slack.standings = Map(UserId(Ana) -> Standing.Full(address))
      w.slack.deliver(mention("1.0")) ==> true
      w.voucher.vouched ==> Vector(Vouched(ana, Standing.Full(address)))
    }

    test(
      "a backfill hears each author once Slack has answered for them, and stops at one it cannot answer for, naming the message"
    ) {
      val w = new World
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("1.0"), None, Some(UserId(Ana)), false, None, "earlier"),
          Listed(Ts("2.0"), None, Some(UserId(Ben)), false, None, "later")
        )
      )
      val unheard = w.edge.unheard(C, java.time.Instant.EPOCH).fold(sys.error, identity)
      w.edge.backfill(unheard.take(1)) ==> Right(())
      w.slack.unasked = Some(SlackError.Unreachable("down"))
      val stopped = w.edge.backfill(unheard.drop(1))
      (w.voucher.vouched, w.kept("1.0"), w.kept("2.0"), stopped) ==> (
        Vector(Vouched(ana, Standing.Full(None))),
        true,
        false,
        Left(
          s"message 2.0 not heard: slack:$Team/$Ben was never attested and Slack could not be asked: Unreachable(down)"
        )
      )
    }
  }
}
