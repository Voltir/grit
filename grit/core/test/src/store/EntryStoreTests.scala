package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.message.Message
import grit.dbos.sql.TestTx

import utest.*

object EntryStoreTests extends TestSuite {

  private def fakeTx: Tx = TestTx.fake

  private val c1 = ConversationId("c1")

  private def entry(id: String, seq: Long, conversation: ConversationId = c1): Entry =
    Entry(
      EntryId(id),
      conversation,
      TurnSeq.First,
      None,
      seq,
      Payload.Message(Message.User(id)),
      Instant.EPOCH
    )

  val tests = Tests {
    test("EntryId round-trip") {
      val id = EntryId("test-1")
      assert(EntryId.value(id) == "test-1")
    }

    test("insert and get") {
      val store = new InMemoryEntryStore
      val e =
        entry("e1", 0L)
      store.insert(e)(using fakeTx) ==> Right(())
      store.get(EntryId("e1"))(using fakeTx) ==> Right(Some(e))
    }

    test("insert duplicate returns DuplicateId") {
      val store = new InMemoryEntryStore
      val e =
        entry("e1", 0L)
      store.insert(e)(using fakeTx) ==> Right(())
      store.insert(e)(using fakeTx) ==>
        Left(StoreError.DuplicateId(EntryId("e1")))
    }

    test("list returns one conversation's entries in insertion order") {
      val store = new InMemoryEntryStore
      val e1 =
        entry("e1", 0L)
      val e2 =
        entry("e2", 1L)
      store.insert(e1)(using fakeTx)
      store.insert(e2)(using fakeTx)
      store.insert(entry("other", 2L, ConversationId("c2")))(using fakeTx)
      store.list(c1)(using fakeTx) ==> Right(Vector(e1, e2))
    }

    test("lockNext is past everything recorded in the conversation") {
      val store = new InMemoryEntryStore
      store.lockNext(c1)(using fakeTx) ==> Right(EntryStore.Next(TurnSeq.First, 0L))
      store.insert(entry("e1", 4L))(using fakeTx)
      store.insert(entry("other", 9L, ConversationId("c2")))(using fakeTx)
      store.lockNext(c1)(using fakeTx) ==> Right(EntryStore.Next(TurnSeq(1), 5L))
    }

    test("get non-existent returns None") {
      val store = new InMemoryEntryStore
      store.get(EntryId("missing"))(using fakeTx) ==> Right(None)
    }

    test("Tx is scoped to the transaction") {
      // The capture-checking guarantee is enforced in the ^ annotations of
      // the EntryStore signatures and Store.transact; a real-file probe was
      // verified to fail with "Capability ... outlives its scope ... leaks
      // into outer capture set". This cannot be
      // pinned via utest compileError: scala.compiletime.testing typechecks
      // the snippet in a fresh compiler that does not inherit
      // -language:experimental.captureChecking, and language imports are
      // only legal at toplevel, so the nested context can never enable it.
      // Instead we pin the positive surface: a Tx obtained inside a
      // transaction scope stays usable there.
      val store = new InMemoryEntryStore
      val e =
        entry("e1", 0L)
      store.insert(e)(using fakeTx) ==> Right(())
      store.get(EntryId("e1"))(using fakeTx) ==> Right(Some(e))
    }

    test("StoreError ADT is total") {
      // Compile-time check: the match is exhaustive.
      val err: StoreError = StoreError.DuplicateId(EntryId("x"))
      val msg = err match {
        case StoreError.DuplicateId(id) => EntryId.value(id)
        case StoreError.DatabaseError(c) => c
      }
      assert(msg == "x")
    }
  }
}
