package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq, WorkflowId}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.model.{
  Assignment,
  Catalog,
  Known,
  ModelId,
  ModelRef,
  NameRepair,
  Policy,
  Profile,
  Source,
  StrictSchemas,
  TurnProfile,
  TurnProfileId
}

import utest.*

/** The contract every [[EntryStore]], [[UsageLedger]], [[ModelProfileStore]] and
  * [[ModelFactStore]] keeps, run against one
  * implementation of each: the in-memory fakes in core, the SQL stores in grit.dbos. The
  * fakes stand in for the SQL stores in every other module's tests, so whatever those tests
  * rely on belongs here.
  *
  * Tests share the stores' database, so each names its own conversations, entry ids and
  * workflow ids.
  */
abstract class StoreContract extends TestSuite {

  /** The entry store under test. */
  protected def entries: EntryStore

  /** The ledger under test, over the same database as [[entries]]. */
  protected def ledger: UsageLedger

  /** The profile store under test, over the same database as [[entries]]. */
  protected def profiles: ModelProfileStore

  /** The fact store under test, over the same database as [[entries]]. It keeps every fact
    * in one list, so only one test writes to it.
    */
  protected def facts: ModelFactStore

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** A conversation that does not exist. */
  protected def unknownConversation: ConversationId

  private def entry(
      c: ConversationId,
      id: String,
      seq: Long,
      turn: TurnSeq = TurnSeq.First,
      parent: Option[String] = None
  ): Entry =
    Entry(
      EntryId(id),
      c,
      turn,
      parent.map(EntryId(_)),
      seq,
      Payload.Message(Message.User(id)),
      Instant.EPOCH
    )

  private def ids(listed: Either[StoreError, Vector[Entry]]): Either[StoreError, Vector[String]] =
    listed.map(_.map(e => EntryId.value(e.id)))

  /** A turn profile with every role on `model`, budget `budget`. */
  private def profile(model: String, budget: Int): TurnProfile = {
    val ref = ModelRef(ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)), None)
    val a = Assignment(ref, budget, None)
    Catalog.of(Policy(a, a, a), Vector.empty).pin
  }

  private val usage = Usage(Tokens(10), Tokens(5), Tokens(2), Some(BigDecimal("0.0000123")))

  val tests = Tests {

    test("an inserted entry is got back by its id, unchanged") {
      val e = entry(conversation("get"), "got", 3, TurnSeq(2), Some("its-parent"))
      transaction(entries.insert(e)) ==> Right(())
      transaction(entries.get(EntryId("got"))) ==> Right(Some(e))
    }

    test("an id no entry has is None") {
      transaction(entries.get(EntryId("never-inserted"))) ==> Right(None)
    }

    test("an id already taken is DuplicateId, whatever the rest of the entry") {
      val c = conversation("duplicate")
      transaction(entries.insert(entry(c, "dup", 0))) ==> Right(())
      transaction(entries.insert(entry(c, "dup", 0))) ==>
        Left(StoreError.DuplicateId(EntryId("dup")))
      transaction(entries.insert(entry(c, "dup", 1))) ==>
        Left(StoreError.DuplicateId(EntryId("dup")))
    }

    test("a DuplicateId leaves the transaction usable") {
      val c = conversation("savepoint")
      val outcome = transaction {
        entries.insert(entry(c, "first", 0))
        (entries.insert(entry(c, "first", 1)), entries.insert(entry(c, "after", 1)))
      }
      outcome ==> (Left(StoreError.DuplicateId(EntryId("first"))), Right(()))
      ids(transaction(entries.list(c))) ==> Right(Vector("first", "after"))
    }

    test("a seq already taken in the conversation is a DatabaseError; another's is free") {
      val c = conversation("seq-taken")
      val other = conversation("seq-free")
      transaction(entries.insert(entry(c, "holder", 0))) ==> Right(())
      val taken = transaction(entries.insert(entry(c, "clash", 0)))
      assert(taken match {
        case Left(StoreError.DatabaseError(_)) => true
        case _ => false
      })
      transaction(entries.insert(entry(other, "elsewhere", 0))) ==> Right(())
      ids(transaction(entries.list(c))) ==> Right(Vector("holder"))
    }

    test("list is one conversation's entries by seq, whatever order they were inserted in") {
      val c = conversation("order")
      transaction {
        entries.insert(entry(c, "order-3", 7))
        entries.insert(entry(c, "order-1", 2))
        entries.insert(entry(conversation("order-other"), "not-mine", 4))
        entries.insert(entry(c, "order-2", 5))
      }
      ids(transaction(entries.list(c))) ==> Right(Vector("order-1", "order-2", "order-3"))
    }

    test("list of an unknown conversation is empty") {
      transaction(entries.list(unknownConversation)) ==> Right(Vector.empty)
    }

    test("lockNext is the start of an empty conversation, and past everything in one") {
      val c = conversation("next")
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq.First, 0L))
      transaction {
        entries.insert(entry(c, "late", 4, TurnSeq(2)))
        entries.insert(entry(c, "early", 1, TurnSeq(0)))
        entries.insert(entry(conversation("next-other"), "beyond", 9, TurnSeq(5)))
      }
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq(3), 5L))
    }

    test("the ledger gives back what was recorded, the cost exactly and its absence as None") {
      val c = conversation("ledger")
      val free = usage.copy(costUsd = None)
      transaction {
        entries.insert(entry(c, "costly", 0))
        entries.insert(entry(c, "free", 1))
        ledger.record(EntryId("costly"), WorkflowId("w-exact"), "m", usage, Tokens(12))
        ledger.record(EntryId("free"), WorkflowId("w-exact"), "n", free, Tokens(3))
      }
      transaction(ledger.of(WorkflowId("w-exact"))) ==>
        Right(
          Vector(
            UsageLedger.Row(EntryId("costly"), "m", usage, Tokens(12)),
            UsageLedger.Row(EntryId("free"), "n", free, Tokens(3))
          )
        )
    }

    test("a second record of an entry is DuplicateId, and leaves the transaction usable") {
      val c = conversation("ledger-duplicate")
      val outcome = transaction {
        entries.insert(entry(c, "once", 0))
        entries.insert(entry(c, "next", 1))
        val first = ledger.record(EntryId("once"), WorkflowId("w-dup"), "m", usage, Tokens(1))
        val again = ledger.record(EntryId("once"), WorkflowId("w-dup"), "m", usage, Tokens(2))
        (first, again, ledger.record(EntryId("next"), WorkflowId("w-dup"), "m", usage, Tokens(3)))
      }
      outcome ==> (Right(()), Left(StoreError.DuplicateId(EntryId("once"))), Right(()))
      transaction(ledger.of(WorkflowId("w-dup"))).map(_.map(_.estimatedInput)) ==>
        Right(Vector(Tokens(1), Tokens(3)))
    }

    test("of is one workflow's rows in record order, within a transaction and across them") {
      val c = conversation("ledger-order")
      val w = WorkflowId("w-order")
      transaction {
        Vector("z", "a", "m", "other").zipWithIndex.foreach { (id, seq) =>
          entries.insert(entry(c, id, seq.toLong))
        }
        // Recorded in the reverse of their ids' order, so ordering by id cannot pass.
        ledger.record(EntryId("z"), w, "m", usage, Tokens(1))
        ledger.record(EntryId("other"), WorkflowId("w-order-other"), "m", usage, Tokens(1))
        ledger.record(EntryId("a"), w, "m", usage, Tokens(1))
      }
      transaction(ledger.record(EntryId("m"), w, "m", usage, Tokens(1)))
      transaction(ledger.of(w)).map(_.map(r => EntryId.value(r.entry))) ==>
        Right(Vector("z", "a", "m"))
    }

    test("of a workflow that recorded nothing is empty") {
      transaction(ledger.of(WorkflowId("w-none"))) ==> Right(Vector.empty)
    }

    test("a pinned turn's profile is got back by the turn and by the profile's id") {
      val p = profile("a/pinned", 100)
      transaction(profiles.pin(WorkflowId("w-pin"), p)) ==> Right(())
      transaction(profiles.of(WorkflowId("w-pin"))) ==> Right(Some(p))
      transaction(profiles.get(p.id)) ==> Right(Some(p))
    }

    test("turns under one profile share it; a turn pinned again keeps its first") {
      val p = profile("a/shared", 100)
      val other = profile("a/shared", 200)
      transaction {
        for {
          _ <- profiles.pin(WorkflowId("w-share-1"), p)
          _ <- profiles.pin(WorkflowId("w-share-2"), p)
          _ <- profiles.pin(WorkflowId("w-share-1"), other)
        } yield ()
      } ==> Right(())
      transaction(profiles.of(WorkflowId("w-share-1"))) ==> Right(Some(p))
      transaction(profiles.of(WorkflowId("w-share-2"))) ==> Right(Some(p))
      transaction(profiles.get(other.id)) ==> Right(Some(other))
    }

    test("a turn never pinned, or an id never kept, is None") {
      transaction(profiles.of(WorkflowId("w-unpinned"))) ==> Right(None)
      transaction(profiles.get(TurnProfileId("0000000000000000"))) ==> Right(None)
    }

    test("facts are kept as approved, and read back oldest first") {
      val store = facts
      val ref =
        ModelRef(ModelId.of("a/facts").getOrElse(throw new java.lang.AssertionError("id")), None)
      val on = java.time.LocalDate.of(2026, 9, 25)
      val first =
        Profile(ref, strict = Known.Of(StrictSchemas.Enforced, Source.Measured("probe", on, 5, 5)))
      val second = Profile(ref, names = Known.Of(NameRepair.AsSent, Source.Declared("nick", on)))
      transaction(store.all()) ==> Right(Vector.empty)
      transaction(store.keep(first, "nick", Instant.parse("2026-09-25T10:00:00Z"))) ==> Right(())
      transaction(store.keep(second, "ana", Instant.parse("2026-09-25T11:00:00Z"))) ==> Right(())
      transaction(store.all()) ==> Right(
        Vector(
          ModelFactStore.Kept(first, "nick", Instant.parse("2026-09-25T10:00:00Z")),
          ModelFactStore.Kept(second, "ana", Instant.parse("2026-09-25T11:00:00Z"))
        )
      )
    }
  }
}
