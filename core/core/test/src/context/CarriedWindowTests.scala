package grit.core.context

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{Message, Tokens}
import grit.core.store.{Entry, Payload, StoreError}
import grit.core.visibility.Integrity

import utest.*

object CarriedWindowTests extends TestSuite {

  private def drawn(seq: Long): Window = Window(Vector(EntrySeq(seq)))

  /** The window record of turn `turn`, kept as `id`, naming `seq`, carried as `carry`. */
  private def record(id: String, turn: Long, seq: Long, carry: Option[Payload.Carry]): Entry =
    Entry(
      EntryId(id),
      ConversationId("c"),
      TurnSeq(turn),
      None,
      EntrySeq(seq + 100),
      Payload.Window(Vector(EntrySeq(seq)), Vector.empty, carry = carry),
      Instant.parse("2026-10-30T12:00:00Z"),
      Integrity.Member
    )

  private def carried(after: Option[String], cost: Long, dated: String): Option[Payload.Carry] =
    Some(Payload.Carry(after.map(EntryId(_)), Tokens(cost), Some(dated), None))

  /** A get over `kept`, by id. */
  private def from(kept: Entry*): EntryId => Either[StoreError, Option[Entry]] =
    id => Right(kept.find(_.id == id))

  val tests = Tests {
    test(
      "after a boundary the carried window is its part alone; after an append, the parts before it then it"
    ) {
      val first = CarriedWindow.Empty.after(Projection.Boundary(drawn(1), Tokens(10)), TurnSeq(1))
      val second = first.after(Projection.Appended(drawn(2), Tokens(4)), TurnSeq(2))
      val cleared = second.after(Projection.Boundary(drawn(3), Tokens(7)), TurnSeq(3))
      second.parts.map(p => (p.window, p.cost, p.turn)) ==> Vector(
        (drawn(1), Tokens(10), TurnSeq(1)),
        (drawn(2), Tokens(4), TurnSeq(2))
      )
      cleared.parts.map(p => (p.window, p.cost, p.turn)) ==> Vector(
        (drawn(3), Tokens(7), TurnSeq(3))
      )
    }

    test(
      "records read back from the last to their boundary carry the boundary's part, then each appended one's"
    ) {
      val kept = from(
        record("w0", 0, 0, carried(None, 9, "zero")),
        record("w1", 1, 1, carried(None, 10, "one")),
        record("w2", 2, 2, carried(Some("w1"), 4, "two")),
        record("w3", 3, 3, carried(Some("w2"), 6, "three"))
      )
      val chain = CarriedWindow.chain(EntryId("w3"))(kept)
      (
        chain.map(_.map(r => (r.turn, r.carry.dated))),
        chain.map(CarriedWindow.of(_).parts.map(p => (p.window, p.cost, p.turn)))
      ) ==> (
        Right(
          Vector(TurnSeq(1) -> Some("one"), TurnSeq(2) -> Some("two"), TurnSeq(3) -> Some("three"))
        ),
        Right(
          Vector(
            (drawn(1), Tokens(10), TurnSeq(1)),
            (drawn(2), Tokens(4), TurnSeq(2)),
            (drawn(3), Tokens(6), TurnSeq(3))
          )
        )
      )
    }

    test(
      "a chain with a record gone, kept with no carry, not a window, or following a later turn carries nothing"
    ) {
      val boundary = record("w1", 1, 1, carried(None, 10, "one"))
      val last = record("w3", 3, 3, carried(Some("w2"), 6, "three"))
      val said = boundary.copy(id = EntryId("w2"), payload = Payload.Message(Message.User("hi")))
      (
        CarriedWindow.chain(EntryId("w3"))(from(boundary, last)),
        CarriedWindow.chain(EntryId("w3"))(from(boundary, record("w2", 2, 2, None), last)),
        CarriedWindow.chain(EntryId("w3"))(from(boundary, said, last)),
        CarriedWindow.chain(EntryId("w3"))(
          from(boundary, record("w2", 4, 2, carried(Some("w1"), 4, "two")), last)
        ),
        CarriedWindow.chain(EntryId("w3"))(from(record("w3", 3, 3, carried(Some("w3"), 6, "x")))),
        CarriedWindow.chain(EntryId("w3"))(_ => Left(StoreError.Invalid("down")))
      ) ==> (
        Right(Vector.empty),
        Right(Vector.empty),
        Right(Vector.empty),
        Right(Vector.empty),
        Right(Vector.empty),
        Left(StoreError.Invalid("down"))
      )
    }

    test(
      "the window a record showed reads whole: each part's entries, a carried turn's own messages after its part, every part's recalled turns; a record kept with no carry is itself, a broken chain none"
    ) {
      val boundary = record("w1", 1, 1, carried(None, 10, "one"))
      val asked = boundary.copy(
        id = EntryId("in:1"),
        seq = EntrySeq(2),
        payload = Payload.Message(Message.User("one?"))
      )
      val appended = record("w2", 2, 3, carried(Some("w1"), 4, "two")) match {
        case e @ Entry(_, _, _, _, _, w: Payload.Window, _, _) =>
          e.copy(payload = w.copy(recalled = Vector(TurnSeq(0))))
        case other => other
      }
      val older = record("w0", 2, 5, None)
      val all = Vector(boundary, asked, appended)
      (
        CarriedWindow.whole(EntryId("w2"), all),
        CarriedWindow.whole(EntryId("w0"), all :+ older).map(_.entries),
        CarriedWindow.whole(EntryId("w2"), Vector(asked, appended)),
        CarriedWindow.whole(EntryId("in:1"), all)
      ) ==> (
        Some(
          Payload.Window(
            Vector(EntrySeq(1), EntrySeq(2), EntrySeq(3)),
            Vector(TurnSeq(0)),
            carry = None
          )
        ),
        Some(Vector(EntrySeq(5))),
        None,
        None
      )
    }
  }
}
