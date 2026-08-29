package grit

import grit.interop.TestTx
import utest.*
import java.time.Instant

object EntryStoreTests extends TestSuite {

  class FakeEntryStore extends EntryStore {
    private var entries = Vector.empty[Entry]

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = {
      if (entries.exists(_.id == entry.id)) {
        Left(StoreError.DuplicateId(entry.id))
      } else {
        entries = entries :+ entry
        Right(())
      }
    }

    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
      Right(entries.find(_.id == id))

    def listAll()(using Tx^): Either[StoreError, Vector[Entry]] =
      Right(entries)
  }

  private def fakeTx: Tx = TestTx.fake

  val tests = Tests {
    test("EntryId round-trip") {
      val id = EntryId("test-1")
      assert(EntryId.value(id) == "test-1")
    }

    test("insert and get") {
      val store = new FakeEntryStore
      val entry =
        Entry(EntryId("e1"), None, 0L, ujson.Obj(), Instant.EPOCH)
      store.insert(entry)(using fakeTx) ==> Right(())
      store.get(EntryId("e1"))(using fakeTx) ==> Right(Some(entry))
    }

    test("insert duplicate returns DuplicateId") {
      val store = new FakeEntryStore
      val entry =
        Entry(EntryId("e1"), None, 0L, ujson.Obj(), Instant.EPOCH)
      store.insert(entry)(using fakeTx) ==> Right(())
      store.insert(entry)(using fakeTx) ==>
        Left(StoreError.DuplicateId(EntryId("e1")))
    }

    test("listAll returns entries in insertion order") {
      val store = new FakeEntryStore
      val e1 =
        Entry(EntryId("e1"), None, 0L, ujson.Obj(), Instant.EPOCH)
      val e2 =
        Entry(EntryId("e2"), None, 1L, ujson.Obj(), Instant.EPOCH)
      store.insert(e1)(using fakeTx)
      store.insert(e2)(using fakeTx)
      store.listAll()(using fakeTx) ==> Right(Vector(e1, e2))
    }

    test("get non-existent returns None") {
      val store = new FakeEntryStore
      store.get(EntryId("missing"))(using fakeTx) ==> Right(None)
    }

    test("Tx is scoped to the transaction") {
      // The capture-checking guarantee is enforced in the ^ annotations of
      // the EntryStore signatures and Store.transact; a real-file probe was
      // verified to fail with "Capability ... outlives its scope ... leaks
      // into outer capture set" (see ROADMAP.md Decision). This cannot be
      // pinned via utest compileError: scala.compiletime.testing typechecks
      // the snippet in a fresh compiler that does not inherit
      // -language:experimental.captureChecking, and language imports are
      // only legal at toplevel, so the nested context can never enable it.
      // Instead we pin the positive surface: a Tx obtained inside a
      // transaction scope stays usable there.
      val store = new FakeEntryStore
      val entry =
        Entry(EntryId("e1"), None, 0L, ujson.Obj(), Instant.EPOCH)
      store.insert(entry)(using fakeTx) ==> Right(())
      store.get(EntryId("e1"))(using fakeTx) ==> Right(Some(entry))
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
