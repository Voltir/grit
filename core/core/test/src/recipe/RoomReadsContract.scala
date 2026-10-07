package grit.core.recipe

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PrincipalId, TurnRef}
import grit.core.identity.{Account, TestAccounts}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.store.{Entry, EntryStore, Origin, Payload, PeriodStore, StoreError, Tx}

import utest.*

/** The contract every [[RoomReads]] keeps, run against the in-memory fake in core and the SQL
  * reads in grit.dbos. Tests share the store's database, so each names its own channel.
  */
abstract class RoomReadsContract extends TestSuite {

  protected def entries: EntryStore

  /** The periods of those entries, which a closing seals. */
  protected def periods: PeriodStore

  /** The reads under test, over [[entries]]. */
  protected def rooms: RoomReads

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** The conversation at `origin`, the same one for the same origin. */
  protected def conversation(origin: Origin): ConversationId

  /** Records that `by`, a person, wrote the inbound entry `entry`. */
  protected def authored(entry: EntryId, by: Account): Unit

  /** The person `account`, an author already recorded, is linked to, as the store keeps it. */
  protected def person(account: Account): PrincipalId

  /** When the message a pool is read for was said. */
  private val T = Instant.parse("2026-10-02T12:00:00Z")

  /** The start of the range read. */
  private val From = T.minusSeconds(600)

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private val ana = TestAccounts.account("slack:T/UA")
  private val ben = TestAccounts.account("slack:T/UB")

  private val usage = Usage(Tokens(900), Tokens.Zero, Tokens.Zero, None)

  private val reply: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("it moved")), StopReason.EndTurn, usage, "m")

  private def thread(channel: String, ts: String): ConversationId =
    conversation(Origin.Slack("T", channel, ts))

  private def room(channel: String) = Place.under(Namespace.Slack, Vector("T", channel))

  /** `payload` as `c`'s next turn, said `at`, its period opened as the inbox opens one; written
    * by `by` when given.
    */
  private def say(
      c: ConversationId,
      payload: Payload,
      at: Instant,
      by: Option[Account] = None
  ): Entry = {
    val e = transaction {
      val next = right(entries.lockNext(c))
      right(periods.openFor(c, next.turnSeq, at))
      val e = Entry(
        EntryId(s"${ConversationId.value(c)}:${next.seq}"),
        c,
        next.turnSeq,
        None,
        next.seq,
        payload,
        at
      )
      right(entries.insert(e))
      e
    }
    by.foreach(authored(e.id, _))
    e
  }

  private def heard(c: ConversationId, at: Instant, by: Account = ana): Entry =
    say(c, Payload.Heard(s"said at $at"), at, Some(by))

  val tests = Tests {
    test("said: in [from, until) within the room, in none of outside, the latest first") {
      val own = thread("said", "1.0")
      val strand = thread("said", "2.0")
      val other = thread("said", "3.0")
      val elsewhere = thread("said-other", "1.0")
      val atFrom = heard(other, From)
      heard(other, From.minusMillis(1))
      val before = heard(other, T.minusMillis(1))
      heard(other, T)
      heard(other, T.plusMillis(1))
      heard(own, T.minusMillis(1))
      heard(strand, T.minusMillis(1))
      heard(elsewhere, T.minusMillis(1))
      right(transaction(rooms.said(room("said"), From, T, Set(own, strand), 10)))
        .map(_.entry) ==> Vector(before, atFrom)
    }

    test("said: a person's message, grit's reply and post; never a draft, summary or closing") {
      val c = thread("kinds", "1.0")
      val asked = say(c, Payload.Message(Message.User("and the date?")), T.minusSeconds(50))
      val replied = say(c, Payload.Message(reply), T.minusSeconds(40))
      say(c, Payload.Draft(reply), T.minusSeconds(30))
      say(c, Payload.Summary("a summary"), T.minusSeconds(25))
      val posted = say(c, Payload.Posted("the engine's open issues"), T.minusSeconds(20))
      val period = right(transaction(periods.of(TurnRef(c, posted.turnSeq))))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      right(
        transaction(
          periods.seal(
            CloseRef(period, posted.turnSeq, T.minusSeconds(10)),
            CloseReason.Lapsed,
            TestClosings.prose("Someone asked about the date."),
            T.minusSeconds(10)
          )
        )
      )
      right(transaction(rooms.said(room("kinds"), From, T, Set.empty, 10))).map(_.entry) ==>
        Vector(posted, replied, asked)
    }

    test("said: the latest `most`; of two said at once the greater id; none under 1") {
      val a = thread("most", "1.0")
      val b = thread("most", "2.0")
      heard(a, T.minusSeconds(30))
      val tiedA = heard(a, T.minusSeconds(10))
      val tiedB = heard(b, T.minusSeconds(10))
      val greater = Vector(tiedA, tiedB).maxBy(e => EntryId.value(e.id))
      right(transaction(rooms.said(room("most"), From, T, Set.empty, 1))).map(_.entry) ==>
        Vector(greater)
      right(transaction(rooms.said(room("most"), From, T, Set.empty, 0))) ==> Vector.empty
    }

    test("saidBy: the author's own, in [from, until), in none of outside, the latest first") {
      val own = thread("by", "1.0")
      val other = thread("by", "2.0")
      val atFrom = heard(other, From)
      heard(other, From.minusMillis(1))
      val before = heard(other, T.minusMillis(1))
      heard(other, T)
      heard(other, T.plusMillis(1))
      heard(own, T.minusMillis(1))
      heard(other, T.minusSeconds(5), by = ben)
      say(other, Payload.Message(reply), T.minusSeconds(4))
      right(transaction(rooms.saidBy(room("by"), person(ana), From, T, Set(own), 10)))
        .map(_.entry) ==> Vector(before, atFrom)
      right(transaction(rooms.saidBy(room("by"), person(ana), From, T, Set(own), 1)))
        .map(_.entry) ==> Vector(before)
    }

    test("author: an inbound entry's; none for one that is not inbound, or not kept") {
      val c = thread("author", "1.0")
      val hers = heard(c, T.minusSeconds(10))
      val replied = say(c, Payload.Message(reply), T.minusSeconds(5))
      transaction(rooms.author(hers.id)) ==> Right(Some(person(ana)))
      transaction(rooms.author(replied.id)) ==> Right(None)
      transaction(rooms.author(EntryId("never:0"))) ==> Right(None)
    }
  }
}
