package grit.core.inbox

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Period, TestClosings}
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.store.{Origin, Payload, StoreError}
import grit.dbos.sql.TestTx

import utest.*

/** The inbox contract, kept by the in-memory fake. */
object InMemoryInboxTests extends InboxContract {

  protected def withInbox[A](budget: Budget)(body: (Inbox, InboxContract.Store^) => A): A = {
    val inbox = InMemoryInbox.fresh(budget)
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
    def enroll(id: PrincipalId, name: String): Unit =
      inbox.principals
        .enroll(id, name)(using TestTx.fake)
        .fold(e => sys.error(e.toString), identity)
    body(
      inbox,
      InboxContract.Store(
        spend,
        o => inbox.conversations.all.exists(_.origin == o),
        written,
        dated,
        periods,
        close,
        enroll,
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
          inbox.schedules
            .waiting(now, 1000)(using TestTx.fake)
            .fold(e => sys.error(e.toString), _.map(_._1)),
        turn => inbox.finish(turn, None, "no job runs here")
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
        .ingest(Origin.Task("t", "r"), SourceId("m"), Message.User("x"), PrincipalId.Local)
        .fold(e => sys.error(e.toString), identity)
      inbox.finish(turn, Some(reply), "done")
      inbox.progress(turn) ==> Right(Progress.Done(Some(reply), "done"))
    }
  }
}
