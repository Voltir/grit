package grit.slack.edge

import java.time.{Instant, ZoneId}

import grit.core.admin.InMemoryAdministration
import grit.core.approval.Approval
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
import grit.core.files.NoIntake
import grit.core.id.WorkflowId
import grit.core.id.{EntryId, SourceId, ToolCallId, TurnRef}
import grit.core.inbox.InMemoryInbox
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.review.InMemoryReviews
import grit.core.store.{Entry, InMemoryVoucher, Jot, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.sql.TestTx
import grit.slack.client.{Card, FakeSlack, Self, Tag}
import grit.slack.event.{ChannelId, Clicked, Payloads, ResponseUrl, TeamId, Ts, UserId}

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

    private val stores = EdgeStores(
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
      new Attesting(voucher, Recording, _ => ()),
      new NoIntake
    )

    val edge: SlackEdge^{this} = opened()

    /** A new edge over the same fake Slack and stores, as after a restart, listening and
      * reconciled.
      */
    def opened(): SlackEdge^{this} = {
      val edge = new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot), grit.core.id.Href.of(Workspace)),
        stores,
        None,
        ReplyNote.Off,
        Stopped,
        s => logged = logged :+ s
      )
      val _ = slack.listen(edge.receive, _ => (), _ => ())
      edge.reconcile() ==> Right(())
      edge
    }

    /** Ana's mention at the top of C, at `ts`: its turn. */
    def mentioned(ts: String): TurnRef = turnOf(ts, mention(ts))

    /** The turn of `payload`, a message at the top of C at `ts` that mentions grit. */
    def turnOf(ts: String, payload: String): TurnRef = {
      slack.deliver(payload) ==> true
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

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  private val C1 = ToolCallId("c1")
  private val C2 = ToolCallId("c2")

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

    test(
      "a turn whose ask was shown as a card offers slack_post again once its reply is delivered; one shown none does not"
    ) {
      val w = World()
      val asked = w.mentioned("1.0")
      val other = w.mentioned("2.0")
      w.asks(asked, C1, "Make this room quiet: grit posts nothing here unasked.")
      // Counted by the offers below, each run on this thread.
      @caps.unsafe.untrackedCaptures
      var offers = 0
      val offer: Vector[(String, ChannelId)] -> Either[String, Unit] = _ => {
        offers += 1
        Right(())
      }
      def done(turn: TurnRef): Unit = w.inbox.finish(turn, Some(reply("ok")), "replied")
      def pass(): Int = {
        val _ = w.edge.deliver()
        w.edge.reoffer(offer)
        offers
      }
      w.edge.reoffer(offer)
      val first = offers
      val carded = pass()
      done(other)
      val unasked = pass()
      done(asked)
      val answered = pass()
      (first, carded, unasked, answered, w.slack.cards.size) ==> (1, 1, 1, 2, 1)
    }

    test(
      "an open ask is told in its thread, naming its asker, before its card; once, though a restarted edge shows the card again"
    ) {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Set this room's label to confidential")
      w.edge.deliver() ==> Right(0)
      w.edge.deliver() ==> Right(0)
      w.opened().deliver() ==> Right(0)
      val note = Tag.Asking(WorkflowId.value(t.workflowId), ToolCallId.value(C1))
      w.slack.made.map {
        case p: w.slack.Posted => (p.channel, p.thread, p.tag == note, p.post.fallback)
        case a: w.slack.Asked =>
          (a.channel, a.thread, false, s"card ${ToolCallId.value(a.card.call)}")
      } ==> Vector(
        (
          C,
          Ts("1.0"),
          true,
          s"Waiting for <@$Ana> to approve or cancel this; the buttons are shown only to them, in this thread."
        ),
        (C, Ts("1.0"), false, s"card ${ToolCallId.value(C1)}"),
        (C, Ts("1.0"), false, s"card ${ToolCallId.value(C1)}")
      )
    }

    test("an ask whose note Slack will not post is said, and its card shown all the same") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.slack.limited = 1
      w.edge.deliver()
      (
        w.logged.count(_.contains("the note for")),
        w.slack.posts.size,
        w.slack.cards.map(_.card.call)
      ) ==> (1, 0, Vector(C1))
    }

    test("an ask to an account that is no user of grit's team posts no note and no card") {
      val w = World()
      val t = w.turnOf("1.0", message("1.0", s"<@$Bot> hi", extra = Seq("user_team" -> "T0OTHER1")))
      w.asks(t, C1, "Edit a.txt")
      w.edge.deliver() ==> Right(0)
      (w.slack.made, w.logged.count(_.contains("not shown"))) ==> (Vector.empty, 1)
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

    test("the asker's Apply answers Approved and the card says applying; Cancel answers Declined") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.asks(t, C2, "Edit b.txt")
      w.edge.deliver() ==> Right(0)
      w.edge.click(click("grit_apply", Clicked.value(t.workflowId, C1)))
      w.edge.click(click("grit_cancel", Clicked.value(t.workflowId, C2)))
      (w.inbox.answers, w.slack.replaced, w.slack.responses) ==> (
        Vector(
          (t.workflowId, C1, Approval.Approved),
          (t.workflowId, C2, Approval.Declined(None))
        ),
        Vector(
          (ResponseUrl(ActionHook), SlackEdge.Applying),
          (ResponseUrl(ActionHook), SlackEdge.Cancelled)
        ),
        Vector.empty
      )
    }

    test(
      "another person's click sends nothing the core accepts and tells them only the asker can answer"
    ) {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.edge.click(click("grit_apply", Clicked.value(t.workflowId, C1), user = "U0BEN0001"))
      (w.inbox.answers, w.slack.replaced, w.slack.responses) ==> (
        Vector.empty,
        Vector.empty,
        Vector((ResponseUrl(ActionHook), "Only Ana Lima can answer this"))
      )
    }

    test("a click on a closed ask rewrites the card Expired") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      w.inbox.ledger.now = Until.plusSeconds(1)
      w.edge.click(click("grit_apply", Clicked.value(t.workflowId, C1)))
      (w.inbox.answers, w.slack.replaced) ==> (
        Vector.empty,
        Vector((ResponseUrl(ActionHook), SlackEdge.Expired))
      )
    }

    test("a click grit cannot read, or one the inbox cannot take, is said and the card left") {
      val w = World()
      val t = w.mentioned("1.0")
      w.asks(t, C1, "Edit a.txt")
      val before = w.logged.size
      w.edge.click(click("someone_elses", Clicked.value(t.workflowId, C1)))
      w.inbox.down = true
      w.edge.click(click("grit_apply", Clicked.value(t.workflowId, C1)))
      (w.logged.drop(before), w.slack.answers, w.inbox.answers) ==>
        (
          Vector(
            "slack: a click grit cannot read: an action that is not a card's button",
            "slack: a click on c1 of c1:0 not answered: Unavailable(the database is down)"
          ),
          Vector.empty,
          Vector.empty
        )
    }
  }
}
