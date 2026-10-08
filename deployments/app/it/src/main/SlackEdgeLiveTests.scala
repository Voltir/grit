package grit.app.main

import java.time.Instant

import scala.util.Using

import grit.core.approval.Approval
import grit.core.edge.{Attesting, EdgeStores}
import grit.core.id.{CallSlot, ScheduleId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.identity.{Account, Realm, Standing, TestAccounts, Vouched}
import grit.core.inbox.{Inbox, InboxError, Progress, Slotted}
import grit.core.message.Message
import grit.core.speech.Reach
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Subject,
  TestLabels,
  Visibility
}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.prose.markdown.Markdown
import grit.slack.client.{FakeSlack, Self, Tag}
import grit.slack.edge.{SlackAccounts, SlackEdge}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText
import grit.turn.Turn

import utest.*

/** Against Postgres and DBOS, with a fake Slack: the Slack edge over a real engine, across a
  * restart.
  */
object SlackEdgeLiveTests extends TestSuite {
  import LiveTurn.*
  import Payloads.*

  private val self = Self(TeamId(Team), UserId(Bot))

  private def edge(engine: Engine^, slack: FakeSlack): SlackEdge^ =
    new SlackEdge(
      slack,
      self,
      EdgeStores(
        engine.inbox,
        engine.principals,
        engine.deliveries,
        engine.acknowledgements,
        engine.reviews,
        engine.jot,
        engine,
        new Attesting(engine.voucher(Set.empty, Set.empty), engine.jot, _ => ())
      ),
      Set.empty,
      None,
      grit.core.clock.Clock.system(),
      _ => ()
    )

  /** grit's team's accounts, which the edges below attest. */
  private val Ours: Realm =
    SlackAccounts.realm(TeamId(Team)).fold(e => throw new java.lang.AssertionError(e), identity)

  private val ana: Account = TestAccounts.account(s"slack:$Team/$Ana")

  private val C = ChannelId("C123ABC456")

  /** Another person of grit's team. */
  private val Ben = "U0BEN0001"

  /** `user`'s direct message thread rooted at `thread`. */
  private def dm(user: String, thread: String): Origin.Direct =
    Origin.Direct(TestAccounts.sourced(s"slack:$Team/$user"), thread)

  /** The label `origin`'s conversation was created at, as `engine` reads it; none when it has
    * none.
    */
  private def labelled(engine: Engine^, origin: Origin): Option[Label] =
    engine.db
      .read(Subject.Public)(engine.conversations.find(origin))
      .fold(e => throw new java.lang.AssertionError(s"reading: $e"), _.map(_.label))

  /** Every full member of grit's team is staff, cleared Internal; every room Confidential, so
    * what a turn reads outside its room is what its asker is cleared for.
    */
  private val Staff: Visibility =
    (for {
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector.empty, Labelled.Mapped(Label.at(Level.Confidential)))
        .left
        .map(_.written)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(Group(TestLabels.group("staff"), Set.empty, Set(Ours))),
          Vector(Grant(TestLabels.group("staff"), Label.at(Level.Internal)))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The edge as a deployment trusting it for [[Ours]] serves it, listening in C, over
    * `inbox`.
    */
  private def attesting(engine: Engine^, slack: FakeSlack, inbox: Inbox): SlackEdge^ =
    new SlackEdge(
      slack,
      self,
      EdgeStores(
        inbox,
        engine.principals,
        engine.deliveries,
        engine.acknowledgements,
        engine.reviews,
        engine.jot,
        engine,
        new Attesting(engine.voucher(Set(Ours), Set.empty), engine.jot, _ => ())
      ),
      Set(C),
      None,
      grit.core.clock.Clock.system(),
      _ => ()
    )

  /** `inner`, but each turn started and each message ingested or heard is first shown to
    * `seen`: the turn, or the account the message was written through.
    */
  private final class Watched(inner: Inbox, seen: Either[Account, TurnRef] -> Unit) extends Inbox {
    def ingest(
        origin: Origin,
        source: SourceId,
        message: Message.User,
        by: Account
    ): Either[InboxError, TurnRef] = {
      seen(Left(by))
      inner.ingest(origin, source, message, by)
    }
    def hear(
        origin: Origin,
        source: SourceId,
        text: String,
        by: Account,
        at: Instant,
        reach: Reach
    ): Either[InboxError, Unit] = {
      seen(Left(by))
      inner.hear(origin, source, text, by, at, reach)
    }
    def posted(
        origin: Origin,
        source: SourceId,
        text: String,
        at: Instant,
        request: CallSlot,
        by: Account
    ): Either[InboxError, Boolean] = inner.posted(origin, source, text, at, request, by)
    def begun(origin: Origin): Either[InboxError, Boolean] = inner.begun(origin)
    def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]] =
      inner.ingested(origin, source)
    def recorded(origin: Origin, sources: Set[SourceId]): Either[InboxError, Set[SourceId]] =
      inner.recorded(origin, sources)
    def progress(turn: TurnRef): Either[InboxError, Progress] = inner.progress(turn)
    def startTurn(turn: TurnRef): Either[InboxError, Unit] = {
      seen(Right(turn))
      inner.startTurn(turn)
    }
    def startSlot(
        schedule: ScheduleId,
        version: Option[Int],
        now: Instant
    ): Either[InboxError, Slotted] = inner.startSlot(schedule, version, now)
    def answer(
        workflow: WorkflowId,
        call: ToolCallId,
        approval: Approval
    ): Either[InboxError, Unit] = inner.answer(workflow, call, approval)
  }

  /** Each row `sql` reads with `params`, as its columns' text. */
  private def rows(config: DbConfig, sql: String, params: String*): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val width = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to width).map(rs.getString).toVector
          out.result()
        }
      }
    }

  /** Whether `account`'s stored attestation makes it a full member, as text; none when unseen. */
  private def member(config: DbConfig, account: Account): Vector[String] =
    rows(
      config,
      "SELECT member::text FROM grit.attestations WHERE account = ?",
      Account.written(account)
    ).flatten

  /** [[ana]] attested a full member an hour ago, by the engine's voucher, as a run before a
    * restart left her.
    */
  private def memberAnHourAgo(engine: Engine^, config: DbConfig): Unit = {
    val _ = engine.jot
      .write(Subject.Public)(
        engine.voucher(Set(Ours), Set.empty).vouch(Vouched(ana, Standing.Full(None)))
      )
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "UPDATE grit.attestations SET seen_at = now() - interval '1 hour' WHERE account = ?"
        )
      ) { ps =>
        ps.setString(1, Account.written(ana))
        val _ = ps.executeUpdate()
      }
    }
  }

  val tests = Tests {
    test(
      "a new user's first-ever message is recorded once Slack has answered for them, which attests them a full member"
    ) {
      val config = TestPostgres.freshDatabase("slack_attest_new")
      val slack = new FakeSlack
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = Staff)
      val (received, turn) =
        try {
          launch(engine, engine.entries, new CountingProvider)
          val _ = slack.listen(attesting(engine, slack, engine.inbox).receive)
          (
            slack.deliver(mention("1.0")),
            engine.inbox.ingested(Origin.Slack(Team, "C123ABC456", "1.0"), SourceId("1.0"))
          )
        } finally engine.close()
      (received, turn.map(_.isDefined), member(config, ana)) ==>
        (true, Right(true), Vector("true"))
    }

    test(
      "a stored member Slack now calls a guest opens their first turn after a restart at public, checked before it is recorded"
    ) {
      val config = TestPostgres.freshDatabase("slack_attest_guest")
      val slack = new FakeSlack
      slack.standings = Map(UserId(Ana) -> Standing.Outside)
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = Staff)
      val (received, opened, standing) =
        try {
          launch(engine, engine.entries, new CountingProvider)
          memberAnHourAgo(engine, config)
          // As the turn would open once started: its asker cleared as the store says then.
          var opened = Vector.empty[Either[String, Label]]
          // Whether its author is a full member as the store says when the message is recorded.
          var standing = Vector.empty[Vector[String]]
          // Assumed pure: called only by the edge's receive, on the test's thread, inside the
          // deliver the test waits on; neither the closure nor what it writes outlives the test.
          val inbox = new Watched(
            engine.inbox,
            caps.unsafe.unsafeAssumePure {
              case Right(turn) =>
                opened = opened :+ engine.jot
                  .write(Subject.Turn(turn))((tx: Tx^) ?=> Right(Tx.cleared(tx)))
                  .left
                  .map(_.toString)
              case Left(by) => standing = standing :+ member(config, by)
            }
          )
          val _ = slack.listen(attesting(engine, slack, inbox).receive)
          (slack.deliver(mention("1.0")), opened, standing)
        } finally engine.close()
      (received, opened, standing) ==>
        (true, Vector(Right(Label.Public)), Vector(Vector("false")))
    }

    test(
      "a stored member Slack now calls a guest is heard by a backfill only once recorded outside"
    ) {
      val config = TestPostgres.freshDatabase("slack_attest_backfill")
      val slack = new FakeSlack
      slack.standings = Map(UserId(Ana) -> Standing.Outside)
      slack.histories =
        Map(C -> Vector(Listed(Ts("1.0"), None, Some(UserId(Ana)), false, None, "hm")))
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = Staff)
      val (heard, standing) =
        try {
          memberAnHourAgo(engine, config)
          var standing = Vector.empty[Vector[String]]
          // Assumed pure: called only by the backfill, on the test's thread, which the test waits
          // on; neither the closure nor what it writes outlives the test.
          val inbox = new Watched(
            engine.inbox,
            caps.unsafe.unsafeAssumePure {
              case Left(by) => standing = standing :+ member(config, by)
              case Right(_) => ()
            }
          )
          val e = attesting(engine, slack, inbox)
          (e.unheard(C, Instant.EPOCH).flatMap(e.backfill), standing)
        } finally engine.close()
      (heard, standing) ==> (Right(()), Vector(Vector("false")))
    }

    test(
      "a direct message opens at its author's clearance as Slack attests them before it is recorded: a full member of grit's team at staff's grant, a guest at public"
    ) {
      val config = TestPostgres.freshDatabase("slack_direct_cleared")
      val slack = new FakeSlack
      slack.standings = Map(UserId(Ben) -> Standing.Outside)
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = Staff)
      try {
        launch(engine, engine.entries, new CountingProvider)
        val _ = slack.listen(attesting(engine, slack, engine.inbox).receive)
        (
          slack.deliver(direct("9.0", "hello")),
          slack.deliver(direct("9.0", "hello", user = Ben)),
          labelled(engine, dm(Ana, "9.0")),
          labelled(engine, dm(Ben, "9.0"))
        ) ==> (true, true, Some(Label.at(Level.Internal)), Some(Label.Public))
      } finally engine.close()
    }

    test(
      "a direct message in a thread begun when its author was cleared for more is told so once in its thread, and recorded nowhere"
    ) {
      val config = TestPostgres.freshDatabase("slack_direct_sealed")
      val slack = new FakeSlack
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = Staff)
      try {
        launch(engine, engine.entries, new CountingProvider)
        val _ = slack.listen(attesting(engine, slack, engine.inbox).receive)
        slack.deliver(direct("9.0", "hello")) ==> true
        slack.standings = Map(UserId(Ana) -> Standing.Outside)
        slack.deliver(userChange()) ==> true
        slack.deliver(direct("9.1", "and another", Some("9.0"))) ==> true
        slack.deliver(direct("9.1", "and another", Some("9.0"))) ==> true
        (
          labelled(engine, dm(Ana, "9.0")),
          engine.inbox.recorded(dm(Ana, "9.0"), Set(SourceId("9.1"))),
          slack.posts.map(p => (p.channel, p.thread, p.post.fallback, p.tag))
        ) ==> (
          Some(Label.at(Level.Internal)),
          Right(Set()),
          Vector((ChannelId(AnasDm), Ts("9.0"), InboxError.SealedReply, Tag.Refused(Ts("9.1"))))
        )
      } finally engine.close()
    }

    test(
      "a mention recorded by one engine is answered once, under its author's name, by the next, in its thread"
    ) {
      val config = TestPostgres.freshDatabase("slack_edge")
      val slack = new FakeSlack
      val first = LiveEngine.open(config, Turn.Epoch)
      try {
        launch(first, first.entries, new CountingProvider)
        val _ = slack.listen(edge(first, slack).receive)
        slack.deliver(mention("1.0")) ==> true
      } finally first.close()
      slack.posts ==> Vector.empty

      val second = LiveEngine.open(config, Turn.Epoch)
      val (passes, last) =
        try {
          launch(second, second.entries, new CountingProvider)
          val restarted = edge(second, slack)
          val deadline = System.nanoTime() + 60_000_000_000L
          var passes = 0
          while (slack.posts.isEmpty && System.nanoTime() < deadline) {
            val _ = restarted.deliver()
            passes += 1
            Thread.sleep(200)
          }
          (passes, restarted.deliver())
        } finally second.close()
      assert(passes >= 1)
      last ==> Right(0)
      val expected = RichText.render(
        Markdown.parse("stub reply to: Ana Lima wrote:\nis it everything a river should be?")
      )
      slack.posts.map(p => (p.thread, p.post.fallback)) ==> Vector(
        (Ts("1.0"), expected.head.fallback)
      )
      slack.reactions ==> Set.empty
    }
  }
}
