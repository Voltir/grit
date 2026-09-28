package grit.slack.edge

import java.time.ZoneOffset

import grit.core.edge.{InMemoryDeliveries, Part}
import grit.core.id.{PrincipalId, SourceId, TurnRef}
import grit.core.inbox.InMemoryInbox
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Jot, Origin, Payload, StoreError, Tx}
import grit.dbos.sql.TestTx
import grit.slack.client.{FakeSlack, Self, Tag}
import grit.slack.event.{ChannelId, Payloads, TeamId, Ts, UserId}

import utest.*

/** [[SlackEdge]] over the in-memory stores and a fake Slack that hands it payloads shaped as
  * Slack's docs give them.
  */
object SlackEdgeTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  private val C = ChannelId("C123ABC456")

  private final class World(budget: Budget = Budget(ZoneOffset.UTC, None)) {
    val slack = new FakeSlack
    val inbox: InMemoryInbox = InMemoryInbox.fresh(budget)

    /** Records a call that cost `usd` today. */
    def spend(usd: String): Unit = {
      val turn = TurnRef(grit.core.id.ConversationId("elsewhere"), grit.core.id.TurnSeq.First)
      val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(usd)))
      val _ = inbox.ledger.record(
        grit.core.id.EntryId(s"spent:${inbox.ledger.rows.size}"),
        turn,
        turn.workflowId,
        "m",
        usage,
        Tokens(1)
      )(using TestTx.fake)
    }
    val deliveries = new InMemoryDeliveries
    def edge(): SlackEdge^ =
      new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot)),
        EdgeStores(inbox, inbox.principals, deliveries, FakeJot),
        _ => ()
      )
    val first: SlackEdge^ = edge()
    val _ = slack.listen(first.receive)

    def origin(thread: String): Origin = Origin.Slack(Team, "C123ABC456", thread)

    /** The turn message `ts` of the thread rooted at `thread` was recorded as, if any. */
    def turnOf(thread: String, ts: String): Option[TurnRef] =
      inbox.ingested(origin(thread), SourceId(ts)).getOrElse(None)

    def turn(thread: String, ts: String): TurnRef =
      turnOf(thread, ts).getOrElse(throw new java.lang.AssertionError(s"no turn for $ts"))

    /** The stored text of message `ts` in the thread rooted at `thread`, and its speaker. */
    def stored(thread: String, ts: String): Option[(String, Option[String])] =
      for {
        t <- turnOf(thread, ts)
        entry <- inbox.entries
          .list(t.conversationId)(using TestTx.fake)
          .toOption
          .flatMap(_.find(_.turnSeq == t.turnSeq))
        text <- entry.payload match {
          case Payload.Message(Message.User(text)) => Some(text)
          case _ => None
        }
      } yield (
        text,
        inbox.principals
          .speakers(Vector(entry.id))(using TestTx.fake)
          .toOption
          .flatMap(_.of(entry.id))
      )

    def pending: Vector[grit.core.edge.Pending] =
      deliveries.pending()(using TestTx.fake).getOrElse(Vector.empty)
  }

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  private def tag(turn: TurnRef, part: Int): Tag =
    Tag.Reply(grit.core.id.WorkflowId.value(turn.workflowId), part)

  val tests = Tests {
    test("introduce names the workspace's assistant as Slack names grit's bot, and says why not") {
      val w = new World
      w.slack.names = w.slack.names.updated(UserId(Bot), "Bort")
      w.first.introduce() ==> Right(())
      w.inbox.principals.name(PrincipalId(s"slack:$Team"))(using TestTx.fake) ==> Right(
        Some("Bort")
      )
      w.slack.names = w.slack.names.removed(UserId(Bot))
      w.edge().introduce() ==> Left(s"grit's bot $Bot has no name in Slack")
    }

    test(
      "a mention is a turn of its thread's conversation, in the person's words under their Slack name, started, awaited and marked :eyes:"
    ) {
      val w = new World
      w.slack.deliver(mention("1.0")) ==> true
      val t = w.turn("1.0", "1.0")
      w.stored("1.0", "1.0") ==> Some(("is it everything a river should be?", Some("Ana Lima")))
      w.inbox.started ==> Vector(t)
      w.pending.map(_.turn) ==> Vector(t)
      w.slack.reactions ==> Set((C, Ts("1.0"), "eyes"))
    }

    test("the same message delivered twice (as a mention and as a message) is one turn") {
      val w = new World
      w.slack.deliver(mention("1.0")) ==> true
      w.slack.deliver(message("1.0", s"<@$Bot> is it everything a river should be?")) ==> true
      w.inbox.started ==> Vector(w.turn("1.0", "1.0"))
      w.pending.size ==> 1
    }

    test("another person's mention reaches the store as their name") {
      val w = new World
      w.slack.deliver(mention("1.0", s"<@$Bot> ask <@$Ana>, or <@U999>")) ==> true
      w.stored("1.0", "1.0").map(_._1) ==> Some("ask @Ana Lima, or @U999")
    }

    test(
      "in a thread grit started every message is a turn; elsewhere only a mention, so a thread grit is pulled into has no history"
    ) {
      val w = new World
      w.slack.deliver(mention("1.0")) ==> true
      w.slack.deliver(message("1.1", "and the delta?", Some("1.0"))) ==> true
      w.slack.deliver(message("2.1", "chat", Some("2.0"))) ==> true
      w.slack.deliver(mentionIn("3.0", "3.1")) ==> true
      w.slack.deliver(message("3.2", "more", Some("3.0"))) ==> true
      w.slack.deliver(message("4.0", "hello")) ==> true
      Vector(("1.0", "1.1"), ("2.0", "2.1"), ("3.0", "3.1"), ("3.0", "3.2"), ("4.0", "4.0")).map(
        w.turnOf(_, _).nonEmpty
      ) ==>
        Vector(true, false, true, false, false)
    }

    test("a message in a channel that is not public, or a bot's, is ignored and acknowledged") {
      val w = new World
      w.slack.privateChannels = Set(C)
      w.slack.deliver(mention("1.0")) ==> true
      w.slack.privateChannels = Set.empty
      w.slack.deliver(message("5.0", s"<@$Bot> hi", user = Bot)) ==> true
      (w.turnOf("1.0", "1.0"), w.turnOf("5.0", "5.0"), w.inbox.started) ==> (
        None,
        None,
        Vector.empty
      )
    }

    test(
      "over the day's cap a new message is not recorded, is told the refusal once in its thread, unmarked, and acknowledged"
    ) {
      val cap = DailyCap.of("1").fold(e => throw new java.lang.AssertionError(e), identity)
      val w = new World(Budget(ZoneOffset.UTC, Some(cap)))
      w.spend("1.00")
      w.slack.deliver(mention("1.0")) ==> true
      // Delivered again, as after a crash between the post and the acknowledgement.
      w.slack.deliver(mention("1.0")) ==> true
      w.turnOf("1.0", "1.0") ==> None
      w.slack.posts.map(p => (p.thread, p.post.fallback, p.tag)) ==>
        Vector((Ts("1.0"), Budget.Refusal, Tag.Refused(Ts("1.0"))))
      (w.slack.reactions, w.pending, w.inbox.started) ==> (Set.empty, Vector.empty, Vector.empty)
    }

    test("a message the database cannot record is not acknowledged, so Slack sends it again") {
      val w = new World
      w.inbox.down = true
      w.slack.deliver(mention("1.0")) ==> false
      w.inbox.down = false
      w.slack.deliver(mention("1.0")) ==> true
      w.inbox.started.size ==> 1
    }

    test("a finished turn's reply is posted once in its thread, with its tag, and :eyes: removed") {
      val w = new World
      w.slack.deliver(mention("1.0"))
      val t = w.turn("1.0", "1.0")
      w.first.deliver() ==> Right(0)
      w.inbox.finish(t, Some(reply("**Places** are where")), "replied")
      w.first.deliver() ==> Right(1)
      w.first.deliver() ==> Right(0)
      w.slack.posts.map(p => (p.channel, p.thread, p.tag, p.post.fallback)) ==>
        Vector((C, Ts("1.0"), tag(t, 0), "Places are where"))
      (w.slack.reactions, w.pending) ==> (Set.empty, Vector.empty)
    }

    test(
      "a part left posting by a crash is looked for by its tag, and posted again only if Slack lacks it"
    ) {
      val w = new World
      w.slack.deliver(mention("1.0"))
      w.slack.deliver(mention("2.0"))
      val (one, two) = (w.turn("1.0", "1.0"), w.turn("2.0", "2.0"))
      w.inbox.finish(one, Some(reply("one")), "replied")
      w.inbox.finish(two, Some(reply("two")), "replied")
      val _ = w.deliveries.posting(one, 0)(using TestTx.fake)
      val _ = w.deliveries.posting(two, 0)(using TestTx.fake)
      // The crash came after Slack took the first post, and before it took the second.
      val _ = w.slack.post(
        C,
        Ts("1.0"),
        grit.slack.text.RichText.render(grit.prose.markdown.Markdown.parse("one")).head,
        tag(one, 0)
      )
      w.first.deliver() ==> Right(2)
      w.slack.posts.map(p => (p.thread, p.post.fallback)) ==> Vector(
        (Ts("1.0"), "one"),
        (Ts("2.0"), "two")
      )
    }

    test("a post Slack refuses leaves its part posting, and the next pass finishes it") {
      val w = new World
      w.slack.deliver(mention("1.0"))
      val t = w.turn("1.0", "1.0")
      w.inbox.finish(t, Some(reply("hi")), "replied")
      w.slack.down = true
      w.first.deliver() ==> Right(0)
      w.pending.map(_.parts) ==> Vector(Map(0 -> Part.Posting))
      w.slack.down = false
      w.first.deliver() ==> Right(1)
      w.slack.posts.map(_.post.fallback) ==> Vector("hi")
    }

    test("a turn that ends with no reply posts one line saying grit could not answer, and why") {
      val w = new World
      w.slack.deliver(mention("1.0"))
      w.inbox.finish(w.turn("1.0", "1.0"), None, "failed: the model is down")
      w.first.deliver() ==> Right(1)
      w.slack.posts.map(_.post.fallback) ==> Vector(
        "grit could not answer: failed: the model is down"
      )
    }

    test(
      "an edge starting up starts every unfinished turn again on its first pass, and only then"
    ) {
      val w = new World
      w.slack.deliver(mention("1.0"))
      val t = w.turn("1.0", "1.0")
      w.inbox.started =
        Vector.empty // the start was lost, as a crash between ingest and start loses it
      val restarted = w.edge()
      restarted.deliver() ==> Right(0)
      w.inbox.started ==> Vector(t)
      w.inbox.started = Vector.empty
      restarted.deliver() ==> Right(0)
      w.inbox.started ==> Vector.empty
    }
  }
}
