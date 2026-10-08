package grit.core.inbox

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, SourceId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Period, TestClosings}
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Origin, Payload, StoreError}
import grit.core.visibility.Visibility
import grit.dbos.sql.TestTx

import utest.*

/** The inbox contract, kept by the in-memory fake. */
object InMemoryInboxTests extends InboxContract {

  protected def withInbox[A](budget: Budget, visibility: Visibility)(
      body: (Inbox, InboxContract.Store^) => A
  ): A = {
    val (inbox, store) = opened(budget, visibility)
    body(inbox, store)
  }

  protected def reopening[A, B](budget: Budget, was: Visibility, now: Visibility)(
      before: (Inbox, InboxContract.Store^) => A
  )(after: (A, Inbox, InboxContract.Store^) => B): B = {
    val (inbox, store) = opened(budget, was)
    val a = before(inbox, store)
    inbox.reopen(now)
    after(a, inbox, store)
  }

  /** A fresh inbox under `visibility`, its people resolved as linking does by a voucher of the
    * contract's realms, and the store under it.
    */
  private def opened(
      budget: Budget,
      visibility: Visibility
  ): (InMemoryInbox, InboxContract.Store^) = {
    val voucher = new InMemoryVoucher(
      Set(InboxContract.T1, InboxContract.T2),
      InboxContract.Claimed,
      visibility
    )
    val inbox = InMemoryInbox.fresh(budget, visibility, voucher.principal)
    def spend(usd: BigDecimal): Unit = {
      val entry = EntryId(s"spent:${inbox.ledger.rows.size}")
      val turn = TurnRef(ConversationId("elsewhere"), TurnSeq.First)
      val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(usd))
      val _ = inbox.ledger.record(entry, turn, turn.workflowId, "m", usage, Tokens(1))(using
        TestTx.fake
      )
    }
    def written(origin: Origin): Vector[(Payload, Option[String])] =
      inbox.conversations.all.find(_.origin == origin).toVector.flatMap { c =>
        val tx = TestTx.fake
        val all = inbox.entries.list(c.id)(using tx).fold(e => sys.error(e.toString), identity)
        val names = inbox.principals
          .speakers(all.map(_.id))(using tx)
          .fold(e => sys.error(e.toString), identity)
        all.map(e => (e.payload, names.of(e.id)))
      }
    def dated(origin: Origin): Vector[java.time.Instant] =
      inbox.conversations.all
        .find(_.origin == origin)
        .toVector
        .flatMap { c =>
          inbox.entries.list(c.id)(using TestTx.fake).fold(e => sys.error(e.toString), identity)
        }
        .map(_.createdAt)
    def periods(origin: Origin): Vector[Period] =
      inbox.conversations.all.find(_.origin == origin).toVector.flatMap { c =>
        inbox.periods.all(c.id)(using TestTx.fake).fold(e => sys.error(e.toString), identity)
      }
    def close(turn: TurnRef): Unit = {
      val tx = TestTx.fake
      val _ = inbox.periods
        .of(turn)(using tx)
        .flatMap(
          _.toRight(StoreError.Invalid(s"no period holds $turn")).flatMap(p =>
            inbox.periods.seal(
              CloseRef(p.ref, turn.turnSeq, Instant.EPOCH),
              CloseReason.Lapsed,
              TestClosings.prose("closed"),
              Instant.EPOCH
            )(using tx)
          )
        )
        .fold(e => sys.error(e.toString), identity)
    }
    def reached(origin: Origin): Vector[Option[Reach]] =
      inbox.conversations.all.find(_.origin == origin).toVector.flatMap { c =>
        val tx = TestTx.fake
        inbox.entries
          .list(c.id)(using tx)
          .fold(e => sys.error(e.toString), identity)
          .map(e =>
            inbox.speech
              .reach(TurnRef(c.id, e.turnSeq))(using tx)
              .fold(e => sys.error(e.toString), identity)
          )
      }
    def name(account: Account, name: String): Unit =
      inbox.principals
        .name(account, name)(using TestTx.fake)
        .fold(e => sys.error(e.toString), identity)
    (
      inbox,
      InboxContract.Store(
        spend,
        o => inbox.conversations.all.exists(_.origin == o),
        written,
        dated,
        periods,
        close,
        name,
        reached,
        o =>
          inbox.conversations.all
            .find(_.origin == o)
            .flatMap(c => inbox.conversations.posts.get(c.id)),
        (declared, now) =>
          inbox.schedules
            .declare(declared, now)(using TestTx.fake)
            .fold(e => sys.error(e.toString), identity),
        id =>
          inbox.schedules.read(id)(using TestTx.fake).fold(e => sys.error(e.toString), identity),
        now =>
          (for {
            flying <- inbox.schedules.inFlight(1000)(using TestTx.fake)
            due <- inbox.schedules.due(now, 1000)(using TestTx.fake)
          } yield flying ++ due).fold(e => sys.error(e.toString), _.map(_._1)),
        turn => inbox.finish(turn, None, "no job runs here"),
        (slot, version, at) =>
          inbox.schedules
            .replied(slot, version, at)(using TestTx.fake)
            .fold(e => sys.error(e.toString), identity),
        o => inbox.conversations.all.find(_.origin == o).map(_.label),
        o =>
          inbox.conversations.all
            .find(_.origin == o)
            .foreach(c => inbox.conversations.unreadable += c.id),
        v => {
          val _ = voucher.vouch(v)(using TestTx.fake)
        },
        room => inbox.quiet(room)
      )
    )
  }
}

/** What only the fake does: a turn a test ends. */
object InMemoryInboxFinishTests extends TestSuite {

  val tests = Tests {
    test("a finished turn is Done with its reply and outcome") {
      val inbox = InMemoryInbox.fresh()
      val reply: Message.Assistant = Message.Assistant(
        Vector(AssistantBlock.Text("hi")),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val turn = inbox
        .ingest(Origin.Task("t", "r"), SourceId("m"), Message.User("x"), Account.Local)
        .fold(e => sys.error(e.toString), identity)
      inbox.finish(turn, Some(reply), "done")
      inbox.progress(turn) ==> Right(Progress.Done(Some(reply), "done"))
    }
  }
}
