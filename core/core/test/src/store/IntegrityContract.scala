package grit.core.store

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, EntrySeq, PeriodRef, PeriodSeq, TurnRef}
import grit.core.message.Message
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.visibility.{Compartments, Integrity, TestLabels, Visibility}

import utest.*

/** What a row is believed at (ADR 0036), as every entry store and period store keeps it: an
  * entry at its writer's integrity met with its room's cap, read back so by every read; a turn
  * at the meet of its entries; a closing at the meet of its period's entries.
  */
abstract class IntegrityContract extends TestSuite {
  import IntegrityContract.*

  protected def entries: EntryStore

  protected def periods: PeriodStore

  /** A transaction under [[IntegrityContract.Capping]]. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** The conversation `origin` names, created if new. */
  protected def conversation(origin: Origin): ConversationId

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  private val At = Instant.parse("2026-10-14T09:00:00Z")

  /** A message at `integrity` as `turn`'s next entry, under the id `name`; its id. */
  private def say(turn: TurnRef, name: String, integrity: Integrity): EntryId = transaction {
    val next = right(entries.lockNext(turn.conversationId))
    val id = EntryId(s"${ConversationId.value(turn.conversationId)}:$name")
    val said = Payload.Message(Message.User(name))
    right(
      entries.insert(
        Entry(id, turn.conversationId, turn.turnSeq, None, next.seq, said, At, integrity)
      )
    )
    id
  }

  /** A new turn of `c`, its period opened for it as the inbox opens one. */
  private def turn(c: ConversationId): TurnRef = transaction {
    val next = right(entries.lockNext(c))
    right(periods.openFor(c, next.turnSeq, At))
    TurnRef(c, next.turnSeq)
  }

  private def believed(id: EntryId): Option[Integrity] =
    transaction(right(entries.get(id))).map(_.integrity)

  val tests = Tests {
    test(
      "an entry is believed at its writer's integrity met with its room's cap, and read so by every read"
    ) {
      val capped = turn(conversation(Origin.Task(CappedRoom, "met")))
      val open = turn(conversation(Origin.Task("open", "met")))
      val member = say(capped, "member", Integrity.Member)
      val untrusted = say(capped, "untrusted", Integrity.Untrusted)
      val endorsed = say(open, "endorsed", Trial)
      believed(member) ==> Some(Integrity.Outside)
      believed(untrusted) ==> Some(Integrity.Untrusted)
      believed(endorsed) ==> Some(Trial)
      val c = capped.conversationId
      transaction(right(entries.list(c))).map(_.integrity) ==>
        Vector(Integrity.Outside, Integrity.Untrusted)
      transaction(right(entries.ofTurn(capped))).map(_.integrity) ==>
        Vector(Integrity.Outside, Integrity.Untrusted)
      transaction(right(entries.at(c, Vector(EntrySeq.First)))).map(_.integrity) ==>
        Vector(Integrity.Outside)
    }

    test("a turn is believed at the meet of its entries, and untrusted with none") {
      val c = conversation(Origin.Task("open", "turns"))
      val first = turn(c)
      transaction(right(entries.believed(first))) ==> Integrity.Untrusted
      say(first, "member, endorsed", MemberTrial)
      transaction(right(entries.believed(first))) ==> MemberTrial
      // Incomparable: the meet is neither the first entry's nor the latest's.
      say(first, "authority", Integrity.at(Integrity.Level.Authority))
      transaction(right(entries.believed(first))) ==> Integrity.Member
    }

    test("a closing is believed at the meet of its period's entries") {
      val c = conversation(Origin.Task("open", "closing"))
      val first = turn(c)
      // Incomparable across two turns: the meet is neither the first turn's nor the closing's.
      say(first, "member, endorsed", MemberTrial)
      val second = turn(c)
      say(second, "authority", Integrity.at(Integrity.Level.Authority))
      val period = PeriodRef(c, PeriodSeq.of(1).getOrElse(throw new java.lang.AssertionError(1)))
      val outcome = transaction(
        right(
          periods.seal(
            CloseRef(period, second.turnSeq, At),
            CloseReason.Resolved(
              Probability.of(0.9).getOrElse(throw new java.lang.AssertionError(0.9))
            ),
            TestClosings.prose("done", Some("done: done")),
            At
          )
        )
      )
      outcome ==> Sealed.Closed(period.closingId)
      believed(period.closingId) ==> Some(Integrity.Member)
    }
  }
}

object IntegrityContract {

  /** The task whose room is capped. */
  val CappedRoom: String = "capped"

  /** Authority endorsed in the contracts' compartment: kept apart from member by both parts. */
  val Trial: Integrity = Integrity.at(Integrity.Level.Authority, TestLabels.trial)

  /** Member endorsed in the contracts' compartment: incomparable with authority alone, whose
    * meet with it is member.
    */
  val MemberTrial: Integrity = Integrity.at(Integrity.Level.Member, TestLabels.trial)

  /** The shipped labels, with the room of every [[CappedRoom]] task capped at outside. */
  val Capping: Visibility = Visibility
    .of(
      Compartments
        .of(Vector(TestLabels.trial))
        .fold(c => throw new java.lang.AssertionError(c), identity),
      Visibility.Shipped.rooms,
      Vector.empty,
      Vector.empty,
      capped =
        room => Option.when(room.place == Origin.Task(CappedRoom, "any").room)(Integrity.Outside)
    )
    .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
}
