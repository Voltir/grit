package grit.slack.edge

import java.time.ZoneOffset

import grit.core.admin.InMemoryAdministration
import grit.core.edge.{
  Acknowledgement,
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  Membership,
  Part
}
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  JobName,
  ScheduleId,
  ScheduleKey,
  SourceId,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Account, Realm, Standing, TestAccounts}
import grit.core.inbox.{InMemoryInbox, InboxError}
import grit.core.job.Slot
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.review.InMemoryReviews
import grit.core.speech.Reach
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{InMemoryVoucher, Jot, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  RoomAccess,
  RoomLabels,
  Subject,
  TestLabels,
  Visibility
}
import grit.dbos.sql.TestTx
import grit.prose.markdown.Markdown
import grit.slack.client.{FakeSlack, Self, Tag}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText

import utest.*

/** [[SlackEdge]] over the in-memory stores and a fake Slack that hands it payloads shaped as
  * Slack's docs give them.
  */
object SlackEdgeTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val C = ChannelId("C123ABC456")

  /** Another person, whom a message may name. */
  private val Ben = "U0BEN0001"

  /** grit's team's accounts. */
  private val Ours: Realm =
    SlackAccounts.realm(TeamId(Team)).fold(e => throw new java.lang.AssertionError(e), identity)

  /** What the team's full members are cleared for under [[Staff]]. */
  private val StaffGrant = grit.core.visibility.Label.at(grit.core.visibility.Level.Internal)

  /** Every full member of grit's team is staff, cleared [[StaffGrant]]; every room public. */
  private val Staff: Visibility =
    (for {
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(Group(TestLabels.group("staff"), Set.empty, Set(Ours))),
          Vector(Grant(TestLabels.group("staff"), StaffGrant))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The edge over the in-memory stores and a fake Slack: when `staff`, trusted for grit's team,
    * whose members [[Staff]] clears, its inbox resolving people as linking does; otherwise
    * attesting no one, under the shipped visibility.
    */
  private final class World(
      budget: Budget = Budget(ZoneOffset.UTC, None),
      staff: Boolean = false,
      review: Option[SlackReview] = None,
      reconciled: Boolean = true
  ) {
    val slack = new FakeSlack
    private val voucher =
      if (staff) new InMemoryVoucher(Set(Ours), Set.empty, Staff) else InMemoryVoucher.none()
    val inbox: InMemoryInbox =
      if (staff) InMemoryInbox.fresh(budget, Staff, voucher.principal)
      else InMemoryInbox.fresh(budget)

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

    /** The joins the edge records its bot's memberships through, vouching as the inbox does. */
    val joins = new InMemoryJoins(voucher)
    val acknowledgements = new InMemoryAcknowledgements

    /** What the edges said, in order. */
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    def edge(): SlackEdge^{this} =
      new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot)),
        EdgeStores(
          inbox,
          InMemoryAdministration.none(),
          joins,
          inbox.principals,
          deliveries,
          acknowledgements,
          InMemoryReviews.over(inbox),
          FakeJot,
          new InMemoryEdges,
          new Attesting(voucher, FakeJot, _ => ())
        ),
        review,
        Stopped,
        s => logged = logged :+ s
      )

    /** The entries of the thread rooted at `thread`, in order. */
    def written(thread: String): Vector[Payload] =
      inbox.conversations.all.find(_.origin == origin(thread)).toVector.flatMap { c =>
        inbox.entries.list(c.id)(using TestTx.fake).getOrElse(Vector.empty).map(_.payload)
      }

    /** The call the thread rooted at `thread` was begun by, if a post began it. */
    def postedBy(thread: String): Option[CallSlot] =
      inbox.conversations.all
        .find(_.origin == origin(thread))
        .flatMap(c => inbox.conversations.posts.get(c.id))

    /** A post grit made with `slack_post` at the top of C, for the call `slot`; its ts. */
    def post(slot: CallSlot, text: String = "The engine's open issues."): String =
      RichText.render(Markdown.parse(text)) match {
        case one +: _ =>
          slack
            .postTopLevel(C, one, Tag.Sent(slot.key))
            .fold(e => throw new java.lang.AssertionError(e.toString), Ts.value)
        case _ => throw new java.lang.AssertionError("nothing to post")
      }
    val first: SlackEdge^{this} = edge()
    val _ = slack.listen(first.receive, _ => ())
    // As serving opens: which channels grit's bot is in read from Slack first, unless a test
    // starts from none recorded.
    if (reconciled) {
      first.reconcile() ==> Right(())
      logged = Vector.empty
    }

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

    /** The heard messages of the thread rooted at `thread`, in order, each with its speaker. */
    def heard(thread: String): Vector[(String, Option[String])] =
      inbox.conversations.all.find(_.origin == origin(thread)).toVector.flatMap { c =>
        val all = inbox.entries.list(c.id)(using TestTx.fake).getOrElse(Vector.empty)
        val names = inbox.principals
          .speakers(all.map(_.id))(using TestTx.fake)
          .getOrElse(grit.core.store.Speakers.none)
        all.collect { case e @ grit.core.store.Entry(_, _, _, _, _, Payload.Heard(text), _) =>
          (text, names.of(e.id))
        }
      }

    /** The account each entry of the thread rooted at `thread` was written through, in order. */
    def authors(thread: String): Vector[Option[Account]] =
      inbox.conversations.all.find(_.origin == origin(thread)).toVector.flatMap { c =>
        inbox.entries
          .list(c.id)(using TestTx.fake)
          .getOrElse(Vector.empty)
          .map(e => inbox.principals.author(e.id))
      }

    /** When each entry of the thread rooted at `thread` is dated, in order. */
    def dated(thread: String): Vector[java.time.Instant] =
      inbox.conversations.all.find(_.origin == origin(thread)).toVector.flatMap { c =>
        inbox.entries.list(c.id)(using TestTx.fake).getOrElse(Vector.empty).map(_.createdAt)
      }

    /** The reach kept for each entry of the thread rooted at `thread`, in order. */
    def reached(thread: String): Vector[Option[Reach]] =
      inbox.conversations.all.find(_.origin == origin(thread)).toVector.flatMap { c =>
        inbox.entries
          .list(c.id)(using TestTx.fake)
          .getOrElse(Vector.empty)
          .map(e => inbox.speech.reach(TurnRef(c.id, e.turnSeq))(using TestTx.fake).getOrElse(None))
      }

    def pending: Vector[grit.core.edge.Pending] =
      deliveries.pending()(using TestTx.fake).getOrElse(Vector.empty)

    /** The top-level message `thread`, heard in C; its turn, wanting its acknowledgement where
      * a reply to it would go, as triage wants it of a draft put to grit.
      */
    def named(thread: String): TurnRef = {
      slack.deliver(message(thread, "Pip, what did we decide about the refi page?")) ==> true
      val turn = inbox.conversations.all
        .find(_.origin == origin(thread))
        .map(c => TurnRef(c.id, grit.core.id.TurnSeq.First))
        .getOrElse(throw new java.lang.AssertionError("not heard"))
      acknowledgements.want(turn, to(thread), Start)(using TestTx.fake) ==> Right(())
      turn
    }

    /** Where a reply to the heard message `thread` would go, as its reach keeps it. */
    def to(thread: String): String =
      reached(thread).flatten.flatMap(_.replyTo).headOption.getOrElse("none")

    def standing: Vector[grit.core.edge.Acknowledgement] =
      acknowledgements.standing()(using TestTx.fake).getOrElse(Vector.empty)
  }

  /** A clock stopped at [[Start]]: what the in-memory stores are told, which they ignore. */
  private val Start = java.time.Instant.parse("2026-10-04T12:00:00Z")

  private object Stopped extends grit.core.clock.Clock {
    def now(): java.time.Instant = Start
    def millis(): Long = 0L
    def sleep(duration: scala.concurrent.duration.FiniteDuration): Unit = ()
  }

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  /** The call a post below was made by: one of another conversation's turns. */
  private val Asking: CallSlot =
    CallSlot
      .of(TurnRef(ConversationId("asker"), TurnSeq.First), 0, 1)
      .getOrElse(throw new java.lang.AssertionError("a slot at 0, 1 reads"))

  private def tag(turn: TurnRef, part: Int): Tag =
    Tag.Reply(grit.core.id.WorkflowId.value(turn.workflowId), part)

  /** [[C]]'s room, and the team's rooms it is within. */
  private val Room: grit.core.place.Place = Origin.channel(Team, "C123ABC456")
  private val Workspace: grit.core.place.Place =
    grit.core.place.Place.under(grit.core.place.Namespace.Slack, Vector(Payloads.Team))

  /** When [[Payloads.joined]] says grit's bot joined, and when message 2.0 was said. */
  private val JoinedAt = java.time.Instant.parse("2018-01-08T22:13:20.000100Z")
  private val Said2 = java.time.Instant.ofEpochSecond(2)

  /** A review in a channel of its own, [[C]]. */
  private val Review: SlackReview =
    SlackReview
      .of(Room, UserId("U0NICK001"))
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** A workspace that shares channel [[C]] with grit's. */
  private val Theirs = "T0THEIRS"

  val tests = Tests {
    test(
      "a message in a shared channel is written through its author's account in their own team, addressed or heard, live or listed"
    ) {
      val w = new World
      w.slack.deliver(
        message("3.0", s"<@$Bot> hi from next door", extra = Seq("user_team" -> Theirs))
      ) ==> true
      w.slack.deliver(message("4.0", "just us", extra = Seq("user_team" -> Theirs))) ==> true
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("5.0"), None, Some(UserId(Ana)), false, None, "earlier", Some(TeamId(Theirs)))
        )
      )
      w.first.unheard(C, java.time.Instant.EPOCH).flatMap(w.first.backfill) ==> Right(())
      val theirs = Some(TestAccounts.account(s"slack:$Theirs/$Ana"))
      (w.authors("3.0"), w.authors("4.0"), w.authors("5.0")) ==>
        (Vector(theirs), Vector(theirs), Vector(theirs))
    }

    test(
      "unheard lists what a listened channel said since, as live messages are read, leaving out what is recorded"
    ) {
      val w = new World
      w.slack.histories = Map(
        C -> Vector(
          Listed(Ts("1.0"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "is the freeze on?"),
          Listed(Ts("1.1"), Some(Ts("1.0")), Some(UserId(Ana)), false, None, "it is"),
          Listed(Ts("1.2"), Some(Ts("1.0")), None, true, None, "beep"),
          Listed(Ts("1.3"), None, Some(UserId(Ana)), false, Some("channel_join"), "joined"),
          Listed(Ts("2.0"), None, Some(UserId(Ana)), false, None, s"<@$Bot> lunch?")
        )
      )
      w.slack.deliver(message("1.1", "it is", Some("1.0"))) ==> true
      w.first.unheard(C, java.time.Instant.EPOCH).map(_.map(m => Ts.value(m.ts))) ==>
        Right(Vector("1.0", "2.0"))
    }

    test(
      "backfill hears a past mention at the time it was said, and never takes it as a turn or answers it"
    ) {
      val w = new World
      w.slack.histories = Map(
        C -> Vector(Listed(Ts("2.0"), None, Some(UserId(Ana)), false, None, s"<@$Bot> lunch?"))
      )
      val unheard = w.first.unheard(C, java.time.Instant.EPOCH)
      unheard.flatMap(w.first.backfill) ==> Right(())
      (w.turnOf("2.0", "2.0"), w.inbox.started, w.slack.posts, w.slack.reactions) ==>
        (None, Vector.empty, Vector.empty, Set.empty)
      (w.heard("2.0"), w.dated("2.0")) ==>
        (Vector(("lunch?", Some("Ana Lima"))), Vector(java.time.Instant.ofEpochSecond(2)))
      // A past message has no reply address: it is never answered.
      w.reached("2.0") ==> Vector(Some(Reach(None, Set.empty)))
      w.first.unheard(C, java.time.Instant.EPOCH) ==> Right(Vector.empty)
    }

    test(
      "a message heard live keeps its thread as where a reply would go, and whom it names besides grit"
    ) {
      val w = new World
      w.slack.deliver(message("2.0", s"<@$Ben> is the deploy done?")) ==> true
      w.slack.deliver(message("2.1", "yes", Some("2.0"))) ==> true
      w.reached("2.0") ==> Vector(
        Some(Reach(Some("C123ABC456/2.0/2.0"), Set(TestAccounts.account(s"slack:$Team/$Ben")))),
        Some(Reach(Some("C123ABC456/2.0/2.1"), Set.empty))
      )
    }

    test("an unprompted reply to a top-level message is posted in a thread under it") {
      // Kept beside the mention's reply below: here the address is the heard reach's, awaited
      // as record-speech awaits a posted draft, not one the edge awaited itself.
      val w = new World
      w.slack.deliver(message("2.0", "what did we decide about the refi page?")) ==> true
      val heard = w.inbox.conversations.all
        .find(_.origin == w.origin("2.0"))
        .map(c => TurnRef(c.id, grit.core.id.TurnSeq.First))
        .getOrElse(throw new java.lang.AssertionError("not heard"))
      // As record-speech does for a posted draft: awaited at the reach it was heard with.
      val to = w.reached("2.0").flatten.flatMap(_.replyTo).headOption.getOrElse("none")
      w.deliveries.await(heard, to)(using TestTx.fake) ==> Right(())
      w.inbox.finish(heard, Some(reply("We moved it to Thursday.")), "posted")
      w.first.deliver() ==> Right(1)
      w.slack.posts.map(p => (p.channel, p.thread, p.post.fallback)) ==>
        Vector((C, Ts("2.0"), "We moved it to Thursday."))
    }

    test("a heard message a turn put to grit answers wears :eyes: while it runs, put up once") {
      val w = new World
      val t = w.named("2.0")
      (w.first.acknowledge(), w.first.acknowledge()) ==> (Right(1), Right(0))
      (w.slack.reactions, w.standing) ==> (
        Set((C, Ts("2.0"), "eyes")),
        Vector(Acknowledgement(t, "C123ABC456/2.0/2.0", shown = true))
      )
    }

    test("a turn that ends with nothing to post has its mark taken down, and cleared") {
      val w = new World
      val t = w.named("2.0")
      w.first.acknowledge() ==> Right(1)
      w.inbox.finish(t, None, "passed")
      w.first.acknowledge() ==> Right(1)
      (w.slack.reactions, w.standing) ==> (Set.empty, Vector.empty)
    }

    test("a mark stays until its reply is posted, and the delivery takes it down and clears it") {
      val w = new World
      val t = w.named("2.0")
      w.first.acknowledge() ==> Right(1)
      w.deliveries.await(t, w.to("2.0"))(using TestTx.fake) ==> Right(())
      w.inbox.finish(t, Some(reply("We moved it to Thursday.")), "posted")
      w.first.acknowledge() ==> Right(0)
      w.slack.reactions ==> Set((C, Ts("2.0"), "eyes"))
      w.first.deliver() ==> Right(1)
      (w.slack.reactions, w.standing, w.first.acknowledge()) ==> (Set.empty, Vector.empty, Right(0))
    }

    test("a turn that ended before its mark was put up is cleared, and never marks") {
      val w = new World
      val t = w.named("2.0")
      w.inbox.finish(t, None, "passed")
      w.first.acknowledge() ==> Right(1)
      (w.slack.reactions, w.standing) ==> (Set.empty, Vector.empty)
    }

    test("a mark on a message Slack no longer has is cleared") {
      val w = new World
      val gone = TurnRef(ConversationId("elsewhere"), grit.core.id.TurnSeq.First)
      w.acknowledgements.want(gone, "C123ABC456/9.0/9.0", Start)(using TestTx.fake) ==> Right(())
      w.first.acknowledge() ==> Right(1)
      (w.slack.reactions, w.standing) ==> (Set.empty, Vector.empty)
    }

    test("a mark Slack will not put up is tried again on the next pass") {
      val w = new World
      val t = w.named("2.0")
      w.slack.down = true
      w.first.acknowledge() ==> Right(0)
      w.standing ==> Vector(Acknowledgement(t, "C123ABC456/2.0/2.0", shown = false))
      w.slack.down = false
      w.first.acknowledge() ==> Right(1)
      w.slack.reactions ==> Set((C, Ts("2.0"), "eyes"))
    }

    test("each pass starts again every turn whose mark is not yet up, and no other") {
      val w = new World
      val t = w.named("2.0")
      w.slack.down = true
      w.first.acknowledge() ==> Right(0)
      w.slack.down = false
      w.inbox.started = Vector.empty
      w.first.acknowledge() ==> Right(1)
      w.inbox.started ==> Vector(t)
      w.inbox.started = Vector.empty
      w.first.acknowledge() ==> Right(0)
      w.inbox.started ==> Vector.empty
    }

    test("the bot's Slack name is read for the log, and says why not") {
      val w = new World
      w.slack.names = w.slack.names.updated(UserId(Bot), Some("Pip"))
      w.first.displayName() ==> Right("Pip")
      w.slack.names = w.slack.names.updated(UserId(Bot), None)
      w.edge().displayName() ==> Left(s"grit's bot $Bot has no name in Slack")
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

    test(
      "a mention under grit's post records the post first, as its thread's opening, made by the call its tag names, then the turn"
    ) {
      val w = new World
      val root = w.post(Asking)
      w.slack.deliver(mentionIn(root, "9.1", s"<@$Bot> why this?")) ==> true
      w.written(root) ==> Vector(
        Payload.Posted("The engine's open issues."),
        Payload.Message(Message.User("why this?"))
      )
      w.postedBy(root) ==> Some(Asking)
      w.inbox.started ==> Vector(w.turn(root, "9.1"))
    }

    test("heard replies under grit's post record the post once, before the first") {
      val w = new World
      val root = w.post(Asking)
      w.slack.deliver(message("9.1", "why this?", Some(root))) ==> true
      w.slack.deliver(message("9.2", "no idea", Some(root))) ==> true
      w.written(root) ==> Vector(
        Payload.Posted("The engine's open issues."),
        Payload.Heard("why this?"),
        Payload.Heard("no idea")
      )
    }

    test("a reply under a person's message, or under grit's reply, records nothing before it") {
      val w = new World
      w.slack.histories = Map(
        C -> Vector(Listed(Ts("5.0"), None, Some(UserId(Ana)), false, None, "a person's"))
      )
      w.slack.deliver(mentionIn("5.0", "5.1", s"<@$Bot> and this?")) ==> true
      w.written("5.0") ==> Vector(Payload.Message(Message.User("and this?")))
      val reply = w.slack
        .postTopLevel(
          C,
          RichText.render(Markdown.parse("hi")).headOption.getOrElse(sys.error("no post")),
          Tag.Reply("c:0", 0)
        )
        .fold(e => sys.error(e.toString), Ts.value)
      w.slack.deliver(message("9.9", "ok", Some(reply))) ==> true
      w.written(reply) ==> Vector(Payload.Heard("ok"))
    }

    test("a reply whose thread's root Slack will not give is recorded without it, and said") {
      val w = new World
      val root = w.post(Asking)
      w.slack.rootless = Set(Ts(root))
      w.slack.deliver(mentionIn(root, "9.1", "why this?")) ==> true
      w.written(root) ==> Vector(Payload.Message(Message.User("why this?")))
      w.logged ==> Vector(
        s"slack: the root of thread $root not read, so a post there is not recorded: Unreachable(gone)"
      )
    }

    test("backfill hears a past reply under grit's post after recording the post") {
      val w = new World
      val root = w.post(Asking)
      w.slack.histories = Map(
        C -> Vector(Listed(Ts("9.1"), Some(Ts(root)), Some(UserId(Ana)), false, None, "why?"))
      )
      w.first.unheard(C, java.time.Instant.EPOCH).flatMap(w.first.backfill) ==> Right(())
      w.written(root) ==> Vector(Payload.Posted("The engine's open issues."), Payload.Heard("why?"))
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
      "a channel link reaches the store as the channel's name, asked of Slack once, else its id"
    ) {
      val w = new World
      w.slack.channelNames = w.slack.channelNames.updated(ChannelId("C222"), "ops")
      w.slack.unreachable = Set(ChannelId("C333"))
      w.slack.deliver(
        mention("1.0", s"<@$Bot> in <#C222|>, <#C444|named>, <#C555|> or <#C333|>?")
      ) ==> true
      w.stored("1.0", "1.0").map(_._1) ==> Some("in #ops, #named, #C555 or #C333?")
      w.slack.channelNames = w.slack.channelNames.updated(ChannelId("C222"), "renamed")
      w.slack.deliver(mention("2.0", s"<@$Bot> and <#C222>?")) ==> true
      w.stored("2.0", "2.0").map(_._1) ==> Some("and #ops?")
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

    test(
      "in a channel grit's bot is a member of, a message not addressed to it is heard under the person's name, with no turn, mark or delivery; in one it is not, it is ignored"
    ) {
      val w = new World(reconciled = false)
      w.slack.deliver(message("2.0", "standup moves to 10:00")) ==> true
      w.slack.deliver(message("2.1", "fine by me", Some("2.0"))) ==> true
      w.heard("2.0") ==> Vector(
        ("standup moves to 10:00", Some("Ana Lima")),
        ("fine by me", Some("Ana Lima"))
      )
      (w.turnOf("2.0", "2.0"), w.inbox.started, w.pending, w.slack.reactions) ==>
        (None, Vector.empty, Vector.empty, Set.empty)
      val deaf = new World(reconciled = false)
      deaf.slack.notIn = Set(C)
      deaf.slack.deliver(message("2.0", "standup moves to 10:00")) ==> true
      deaf.slack.deliver(mention("3.0")) ==> true
      (deaf.heard("2.0"), deaf.inbox.conversations.all, deaf.joins.members(Workspace)) ==>
        (Vector.empty, Vector.empty, Right(Vector.empty))
    }

    test(
      "a message racing its channel's join records the join first, as of the message, then is heard; with Slack unreachable it is left for Slack to send again"
    ) {
      val w = new World(reconciled = false)
      w.slack.deliver(message("2.0", "standup moves to 10:00")) ==> true
      (w.joins.members(Workspace), w.joins.access(Room), w.heard("2.0").size) ==> (
        Right(Vector(Room -> Membership.Member(Said2, Some(Said2)))),
        Some(RoomAccess.Open),
        1
      )
      val cut = new World(reconciled = false)
      cut.slack.unreachable = Set(C)
      (cut.slack.deliver(message("2.0", "standup moves to 10:00")), cut.heard("2.0")) ==>
        (false, Vector.empty)
    }

    test(
      "grit's bot joining a channel records it a member, its access as Slack reports it, and its messages are heard without asking Slack again"
    ) {
      val w = new World(reconciled = false)
      w.slack.privateChannels = Set(C)
      w.slack.deliver(joined(inviter = "")) ==> true
      // Slack can no longer be asked: a member's message needs no asking.
      w.slack.unreachable = Set(C)
      w.slack.deliver(message("2.0", "standup moves to 10:00")) ==> true
      (w.joins.members(Workspace), w.joins.access(Room), w.heard("2.0").size) ==> (
        Right(Vector(Room -> Membership.Member(JoinedAt, Some(JoinedAt)))),
        Some(RoomAccess.Invited),
        1
      )
    }

    test(
      "a join invited by someone no trusted realm vouches a full member skips its backfill, and says why; by a full member it does not"
    ) {
      val w = new World(reconciled = false, staff = true)
      w.slack.standings = Map(UserId(Ana) -> Standing.Outside)
      w.slack.deliver(joined()) ==> true
      val v = new World(reconciled = false, staff = true)
      v.slack.deliver(joined()) ==> true
      (
        w.joins.members(Workspace),
        w.logged.filter(_.contains("joined")),
        v.joins.members(Workspace)
      ) ==> (
        Right(Vector(Room -> Membership.Member(JoinedAt, None))),
        Vector(
          s"slack: grit's bot joined #standup (C123ABC456); what was said before is not heard: slack:$Team/$Ana invited it, whom no trusted realm vouches a full member"
        ),
        Right(Vector(Room -> Membership.Member(JoinedAt, Some(JoinedAt))))
      )
    }

    test("a leave stops the next message in its channel from being heard") {
      val w = new World(reconciled = false)
      w.slack.deliver(joined()) ==> true
      w.slack.deliver(message("2.0", "before")) ==> true
      w.slack.deliver(memberLeft()) ==> true
      // Slack would deliver no more; were it to, it no longer says the bot is in the channel.
      w.slack.notIn = Set(C)
      w.slack.deliver(message("3.0", "after")) ==> true
      (w.heard("2.0").size, w.heard("3.0"), w.joins.members(Workspace)) ==>
        (1, Vector.empty, Right(Vector.empty))
    }

    test(
      "a join reported after a leave it was made before changes nothing: the channel stays unheard"
    ) {
      val w = new World(reconciled = false)
      w.slack.deliver(botLeft()) ==> true
      w.slack.deliver(joined(eventTs = Some("1515449500.000000"))) ==> true
      w.slack.notIn = Set(C)
      w.slack.deliver(message("2.0", "hello")) ==> true
      (w.joins.members(Workspace), w.heard("2.0")) ==> (Right(Vector.empty), Vector.empty)
    }

    test(
      "the review's channel is never heard nor recorded a member, its mentions not taken either"
    ) {
      val w = new World(reconciled = false, review = Some(Review))
      w.slack.deliver(joined()) ==> true
      w.slack.deliver(message("2.0", "standup moves to 10:00")) ==> true
      w.slack.deliver(mention("3.0")) ==> true
      (w.heard("2.0"), w.inbox.conversations.all, w.joins.members(Workspace)) ==>
        (Vector.empty, Vector.empty, Right(Vector.empty))
    }

    test(
      "a message heard in a quiet room keeps no reply address, and a mention there is answered"
    ) {
      val w = new World
      w.inbox.quiet(Room)
      w.slack.deliver(message("2.0", s"ask <@$Ben>")) ==> true
      w.slack.deliver(mention("3.0")) ==> true
      (w.reached("2.0"), w.turnOf("3.0", "3.0").toVector == w.inbox.started) ==> (
        Vector(Some(Reach(None, Set(TestAccounts.account(s"slack:$Team/$Ben"))))),
        true
      )
    }

    test(
      "in a private channel grit's bot is a member of, a message is heard and a mention is a turn, as in a public one"
    ) {
      val w = new World
      w.slack.privateChannels = Set(C)
      val inPrivate = Seq("channel_type" -> ujson.Str("group"))
      w.slack.deliver(message("2.0", "standup moves to 10:00", extra = inPrivate)) ==> true
      w.slack.deliver(mention("3.0")) ==> true
      (w.heard("2.0"), w.turnOf("3.0", "3.0").toVector == w.inbox.started, w.inbox.started.size) ==>
        (Vector(("standup moves to 10:00", Some("Ana Lima"))), true, 1)
    }

    test(
      "a mention in a heard thread is one turn, and the replies after it without a mention are still heard"
    ) {
      val w = new World
      w.slack.deliver(message("3.0", "is the freeze on Thursday?")) ==> true
      w.slack.deliver(mentionIn("3.0", "3.1")) ==> true
      w.slack.deliver(message("3.2", "it is", Some("3.0"))) ==> true
      w.inbox.started ==> Vector(w.turn("3.0", "3.1"))
      w.heard("3.0").map(_._1) ==> Vector("is the freeze on Thursday?", "it is")
    }

    test(
      "a message in a conversation that is not a channel, or a bot's, is ignored and acknowledged"
    ) {
      val w = new World(reconciled = false)
      w.slack.directs = Set(C)
      w.slack.deliver(mention("1.0")) ==> true
      w.slack.deliver(message("6.0", "overheard in private")) ==> true
      (w.heard("6.0"), w.turnOf("1.0", "1.0"), w.inbox.started) ==> (
        Vector.empty,
        None,
        Vector.empty
      )
      // A public channel's bot, in a world of its own: a cached answer that C is not a channel
      // would drop the bot's messages for the wrong reason.
      val b = new World
      b.slack.deliver(message("5.0", s"<@$Bot> hi", user = Bot)) ==> true
      b.slack.deliver(message("5.1", "a bot's aside", user = Bot)) ==> true
      // As Slack sends grit's own post that names grit: an app_mention from its bot.
      b.slack.deliver(botPost("5.2", s"<@$Bot> the build is green", mention = true)) ==> true
      (b.heard("5.1"), b.heard("5.2"), b.turnOf("5.0", "5.0"), b.turnOf("5.2", "5.2")) ==>
        (Vector.empty, Vector.empty, None, None)
      (b.inbox.conversations.all, b.inbox.started) ==> (Vector.empty, Vector.empty)
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

    test(
      "a direct message is a turn of its thread in its author's direct room, written through their account, started, awaited in its thread and marked, with no channel listened in"
    ) {
      val w = new World
      w.slack.deliver(direct("9.0", "what am I cleared for?")) ==> true
      val dm = Origin.Direct(TestAccounts.sourced(s"slack:$Team/$Ana"), "9.0")
      val t = w.inbox
        .ingested(dm, SourceId("9.0"))
        .toOption
        .flatten
        .getOrElse(throw new java.lang.AssertionError("not recorded"))
      val entries =
        w.inbox.entries.list(t.conversationId)(using TestTx.fake).getOrElse(Vector.empty)
      (
        entries.map(_.payload),
        entries.map(e => w.inbox.principals.author(e.id)),
        w.inbox.started,
        w.pending.map(p => (p.turn, p.to)),
        w.slack.reactions
      ) ==> (
        Vector(Payload.Message(Message.User("what am I cleared for?"))),
        Vector(Some(TestAccounts.account(s"slack:$Team/$Ana"))),
        Vector(t),
        Vector((t, s"$AnasDm/9.0/9.0")),
        Set((ChannelId(AnasDm), Ts("9.0"), "eyes"))
      )
    }

    test(
      "a direct message in a sealed thread is told so once in its thread, recorded nowhere, and a redelivery posts nothing more"
    ) {
      val w = new World
      val dm: Origin.Direct = Origin.Direct(TestAccounts.sourced(s"slack:$Team/$Ana"), "9.0")
      w.inbox.refusing = Some(InboxError.Sealed(dm))
      w.slack.deliver(direct("9.1", "and another", Some("9.0"))) ==> true
      w.slack.deliver(direct("9.1", "and another", Some("9.0"))) ==> true
      (
        w.inbox.recorded(dm, Set(SourceId("9.1"))),
        w.slack.posts.map(p => (p.channel, p.thread, p.post.fallback, p.tag)),
        w.slack.reactions,
        w.inbox.started
      ) ==> (
        Right(Set()),
        Vector(
          (ChannelId(AnasDm), Ts("9.0"), InboxError.SealedReply, Tag.Refused(Ts("9.1")))
        ),
        Set(),
        Vector()
      )
    }

    test(
      "a direct message is a turn at its author's clearance as Slack attests them: a full member of the team at its members' grant, a guest at public"
    ) {
      val w = new World(staff = true)
      w.slack.standings = Map(UserId(Ben) -> grit.core.identity.Standing.Outside)
      w.slack.deliver(direct("9.0", "hello")) ==> true
      w.slack.deliver(direct("9.0", "hello", user = Ben)) ==> true
      val dms =
        Vector(Ana, Ben).map(u => Origin.Direct(TestAccounts.sourced(s"slack:$Team/$u"), "9.0"))
      (
        dms.map(dm => w.inbox.ingested(dm, SourceId("9.0")).map(_.nonEmpty)),
        dms.map(dm => w.inbox.conversations.all.filter(_.origin == dm).map(_.label))
      ) ==> (
        Vector(Right(true), Right(true)),
        Vector(Vector(StaffGrant), Vector(grit.core.visibility.Label.Public))
      )
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

    test(
      "an edge starting up never starts a scheduled run, whose start is the clock's, and posts its reply once the run ends"
    ) {
      val w = new World
      val slot = Slot(
        ScheduleId
          .declared(Declarer.Deployment, ScheduleKey.of("standup").fold(sys.error, identity)),
        java.time.Instant.parse("2026-10-07T09:00:00Z")
      )
      val run = w.inbox
        .ingest(
          slot.origin(JobName.of("remind").fold(sys.error, identity)),
          Slot.source(1),
          Message.User("due"),
          Account.Grit
        )
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      FakeJot.write(Subject.Turn(run))(w.deliveries.await(run, "C123ABC456/5.0/5.0")) ==> Right(())
      val restarted = w.edge()
      restarted.deliver() ==> Right(0)
      w.inbox.finish(run, Some(reply("standup in ten")), "replied")
      restarted.deliver() ==> Right(1)
      (w.inbox.started, w.logged, w.slack.posts.map(_.post.fallback)) ==>
        (Vector.empty, Vector.empty, Vector("standup in ten"))
    }
  }
}
