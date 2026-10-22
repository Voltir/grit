package grit.slack.edge

import java.time.{Instant, ZoneId}

import grit.core.admin.InMemoryAdministration
import grit.core.document.InMemoryDocuments
import grit.core.edge.{
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins
}
import grit.core.id.{EntryId, SourceId, ToolCallId, TurnRef}
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.store.{Entry, InMemoryVoucher, Jot, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.sql.TestTx
import grit.slack.client.{Card, FakeSlack, Self}
import grit.slack.event.{ChannelId, Payloads, TeamId, Ts, UserId}

import utest.*

/** The Slack edge showing a turn's open asks to its asker as cards and sending their clicks to
  * the inbox, over a fake Slack and the in-memory stores, whose asks are read from the inbox's
  * own entries.
  */
object SlackAskingTests extends TestSuite {
  import Payloads.*

  private val C = ChannelId("C123ABC456")

  /** When the edge works: the deliveries list the asks open then. */
  private val Now = Instant.parse("2026-10-21T15:00:00Z")

  /** When the asks it makes expire. */
  private val Until = Instant.parse("2026-10-21T15:03:00Z")

  private object Stopped extends grit.core.clock.Clock {
    def now(): Instant = Now
    def millis(): Long = 0L
    def sleep(duration: scala.concurrent.duration.FiniteDuration): Unit = ()
  }

  /** The edge over a fake Slack knowing Ana, and the in-memory stores. */
  private final class World {
    val slack = new FakeSlack
    private val voucher = InMemoryVoucher.none()
    val inbox: InMemoryInbox = InMemoryInbox.fresh(records = voucher.records)
    val deliveries = new InMemoryDeliveries(inbox.entries, inbox.principals)
    deliveries.now = Now

    /** The subject of each transaction the edge opened, in order. */
    // Replaced whole by the jot's own calls, which the test makes on its one thread.
    @caps.unsafe.untrackedCaptures
    var subjects = Vector.empty[Subject]

    private object Recording extends Jot {
      def write[A](
          subject: Subject
      )(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
        subjects = subjects :+ subject
        body(using TestTx.fake)
      }
    }

    /** What the edge said, in order. */
    // Replaced whole by the fake's own calls, which the test makes on its one thread.
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    val edge: SlackEdge^{this} =
      new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot), grit.core.id.Href.of(Workspace)),
        EdgeStores(
          inbox,
          new InMemoryAdministration(Visibility.Shipped, voucher),
          new InMemoryJoins(voucher),
          inbox.principals,
          deliveries,
          inbox.ledger,
          new InMemoryAcknowledgements,
          InMemoryReviews.over(inbox),
          new InMemoryOrigins(inbox.conversations),
          (new InMemoryDocuments).reviews.of,
          Recording,
          new InMemoryEdges,
          new Attesting(voucher, Recording, _ => ())
        ),
        None,
        ReplyNote.Off,
        Stopped,
        s => logged = logged :+ s
      )
    val _ = slack.listen(edge.receive, _ => ())
    edge.reconcile() ==> Right(())

    /** Ana's mention at the top of C, at `ts`: its turn. */
    def mentioned(ts: String): TurnRef = {
      slack.deliver(mention(ts)) ==> true
      inbox
        .ingested(Origin.Slack(Team, ChannelId.value(C), ts), SourceId(ts))
        .toOption
        .flatten
        .getOrElse(throw new java.lang.AssertionError(s"no turn for $ts"))
    }

    /** Records `turn`'s gated call `call` asking first, `shown`, open until `until`. */
    def asks(turn: TurnRef, call: ToolCallId, shown: String, until: Instant = Until): Unit = {
      given Tx = TestTx.fake
      val next =
        inbox.entries.lockNext(turn.conversationId).fold(e => sys.error(e.toString), identity)
      val level = inbox.entries.believed(turn).fold(e => sys.error(e.toString), identity)
      val _ = inbox.entries.insert(
        Entry(
          EntryId(s"ask:${ToolCallId.value(call)}"),
          turn.conversationId,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Ask(call, shown, Some(until)),
          Now,
          level
        )
      )
    }
  }

  private val C1 = ToolCallId("c1")

  val tests = Tests {
    test("an open ask is posted once to its asker as an ephemeral card in its thread") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Set this room's label to confidential")
      val before = w.subjects.size
      w.edge.deliver() ==> Right(0)
      w.edge.deliver() ==> Right(0)
      (w.slack.cards, w.subjects.drop(before).contains(Subject.Turn(t))) ==> (
        Vector(
          w.slack.Asked(
            C,
            Ts("1.0"),
            UserId(Ana),
            Card(
              t.workflowId,
              C1,
              "Set this room's label to confidential",
              "Wed 2026-10-21 15:03 UTC"
            )
          )
        ),
        true
      )
    }

    test("a card's expiry is in its asker's zone when Slack places them in one") {
      val w = World()
      w.slack.zones = Map(UserId(Ana) -> ZoneId.of("America/Los_Angeles"))
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.edge.deliver() ==> Right(0)
      w.slack.cards.map(_.card.until) ==> Vector("Wed 2026-10-21 08:03 PDT")
    }

    test("a card Slack will not show is said, and shown at the next pass") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.slack.down = true
      w.edge.deliver()
      w.slack.down = false
      val said = w.logged.exists(_.contains("not shown"))
      w.edge.deliver() ==> Right(0)
      (said, w.slack.cards.map(_.card.call)) ==> (true, Vector(C1))
    }
  }
}
