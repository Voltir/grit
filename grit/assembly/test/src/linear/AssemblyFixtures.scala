package grit.assembly.linear

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.store.{Db, Entry, EntryStore, InMemoryEntryStore, Payload, StoreError, Tx}
import grit.dbos.sql.TestTx

/** One conversation in an in-memory store, for the assemblers' tests. */
object AssemblyFixtures {

  /** The conversation [[store]] writes. */
  val c1: ConversationId = ConversationId("c1")

  /** Reads on the fake transaction the in-memory store ignores. A class, not an object: a
    * test naming a shared capability object would have to declare it with `uses`.
    */
  final class FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** `turns` in a fresh store of [[c1]], turn `t` holding the `t`-th payloads, ids
    * `t{turn}:{seq}`.
    */
  def store(turns: Vector[Payload]*): EntryStore = {
    val entries = new InMemoryEntryStore
    given Tx = TestTx.fake
    for (turn <- turns.indices; payload <- turns(turn)) {
      val next = entries.lockNext(c1).getOrElse(sys.error("in-memory store"))
      val _ = entries.insert(
        Entry(
          EntryId(s"t$turn:${next.seq}"),
          c1,
          TurnSeq(turn.toLong),
          None,
          next.seq,
          payload,
          Instant.EPOCH
        )
      )
    }
    entries
  }
}
