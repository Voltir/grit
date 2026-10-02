package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.Message
import grit.core.place.Place
import grit.dbos.sql.TestTx

import utest.*

object NearbyTests extends TestSuite {

  val tests = Tests {
    test(
      "read gives the entries sections name, by conversation in the order first named, ascending in each, once each, leaving out gone ones"
    ) {
      given Tx = TestTx.fake
      val store = new InMemoryEntryStore
      val (a, b) = (ConversationId("a"), ConversationId("b"))
      for ((c, seq) <- Vector((a, 0L), (a, 1L), (a, 2L), (b, 0L), (b, 1L))) {
        store.insert(
          Entry(
            EntryId(s"${ConversationId.value(c)}$seq"),
            c,
            TurnSeq.First,
            None,
            EntrySeq(seq),
            Payload.Message(Message.User("hi")),
            Instant.EPOCH
          )
        ) ==> Right(())
      }
      val at = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val sections = Vector(
        Nearby.Open(b, at, Vector(EntrySeq(1), EntrySeq(9))),
        Nearby.Closed(a, at, EntrySeq(2)),
        Nearby.Along(a, at, Vector(EntrySeq(0), EntrySeq(2))),
        Nearby.Asked(b, at, Vector(EntrySeq(0)))
      )
      Nearby.read(sections, store).map(_.map(e => EntryId.value(e.id))) ==>
        Right(Vector("b0", "b1", "a0", "a2"))
    }
  }
}
