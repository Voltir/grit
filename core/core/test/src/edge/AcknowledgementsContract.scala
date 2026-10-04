package grit.core.edge

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.store.Tx

import utest.*

/** The contract every [[Acknowledgements]] keeps, run against the in-memory fake in core and
  * the SQL store in grit.dbos. Each test is given a store of its own.
  */
abstract class AcknowledgementsContract extends TestSuite {

  /** A store with nothing wanted, and a conversation turns may be taken from. */
  protected def fresh(): (Acknowledgements, ConversationId)

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def turn(c: ConversationId, n: Long): TurnRef = TurnRef(c, TurnSeq(n))

  private def at(s: Long): Instant = Instant.parse("2026-10-04T12:00:00Z").plusSeconds(s)

  val tests = Tests {
    test("a wanted acknowledgement stands, not shown, where it goes") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 0), "C1/1.0/1.0", at(0))) ==> Right(())
      transaction(acks.standing()) ==>
        Right(Vector(Acknowledgement(turn(c, 0), "C1/1.0/1.0", shown = false)))
    }

    test("wanted twice is wanted once, where it was first wanted") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 0), "a", at(0))) ==> Right(())
      transaction(acks.want(turn(c, 0), "b", at(1))) ==> Right(())
      transaction(acks.standing()) ==> Right(Vector(Acknowledgement(turn(c, 0), "a", false)))
    }

    test("they stand in the order wanted, not the turns' order") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 1), "b", at(0))) ==> Right(())
      transaction(acks.want(turn(c, 0), "a", at(1))) ==> Right(())
      transaction(acks.standing()).map(_.map(_.turn)) ==> Right(Vector(turn(c, 1), turn(c, 0)))
    }

    test("a shown acknowledgement stands, shown") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 0), "a", at(0))) ==> Right(())
      transaction(acks.shown(turn(c, 0), at(1))) ==> Right(())
      transaction(acks.standing()) ==> Right(Vector(Acknowledgement(turn(c, 0), "a", true)))
    }

    test("a cleared acknowledgement no longer stands, and wanting or showing it again does not") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 0), "a", at(0))) ==> Right(())
      transaction(acks.shown(turn(c, 0), at(1))) ==> Right(())
      transaction(acks.cleared(turn(c, 0), at(2))) ==> Right(())
      transaction(acks.want(turn(c, 0), "a", at(3))) ==> Right(())
      transaction(acks.shown(turn(c, 0), at(4))) ==> Right(())
      transaction(acks.standing()) ==> Right(Vector.empty)
    }

    test("one never shown can be cleared, and the others stand") {
      val (acks, c) = fresh()
      transaction(acks.want(turn(c, 0), "a", at(0))) ==> Right(())
      transaction(acks.want(turn(c, 1), "b", at(1))) ==> Right(())
      transaction(acks.cleared(turn(c, 0), at(2))) ==> Right(())
      transaction(acks.standing()) ==> Right(Vector(Acknowledgement(turn(c, 1), "b", false)))
    }

    test("showing or clearing a turn never wanted does nothing, and wants nothing") {
      val (acks, c) = fresh()
      transaction(acks.shown(turn(c, 0), at(0))) ==> Right(())
      transaction(acks.cleared(turn(c, 0), at(1))) ==> Right(())
      transaction(acks.want(turn(c, 0), "a", at(2))) ==> Right(())
      transaction(acks.standing()) ==> Right(Vector(Acknowledgement(turn(c, 0), "a", false)))
    }
  }
}
