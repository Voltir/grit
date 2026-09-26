package grit.assembly.linear

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, TurnSeq}
import grit.core.period.{CloseReason, Closing}
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  Payload,
  StoreError,
  Tx
}
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

  /** A conversation's entries, its periods over them, and the settings in force. */
  final class World(
      val entries: InMemoryEntryStore,
      val periods: InMemoryPeriodStore,
      val lifecycle: InMemoryLifecycleStore
  )

  /** `turns` in a fresh store of [[c1]], turn `t` holding the `t`-th payloads, ids
    * `t{turn}:{seq}`, all in one period.
    */
  def store(turns: Vector[Payload]*): World = closed(Vector(turns.toVector), Vector.empty)

  /** `periods` of turns in a fresh store of [[c1]], numbered on from turn 0 and ids as
    * [[store]]'s; each period but the last is closed after its turns, its closing entry's
    * prose the matching one of `prose`.
    */
  def closed(periods: Vector[Vector[Vector[Payload]]], prose: Vector[String]): World = {
    val entries = new InMemoryEntryStore
    val world = new World(entries, new InMemoryPeriodStore(entries), new InMemoryLifecycleStore)
    given Tx = TestTx.fake
    for (p <- periods.indices) {
      for (turn <- periods(p)) {
        val first = entries.lockNext(c1).getOrElse(sys.error("in-memory store"))
        val _ = world.periods.openFor(c1, first.turnSeq, Instant.EPOCH)
        for (payload <- turn) {
          val next = entries.lockNext(c1).getOrElse(sys.error("in-memory store"))
          val _ = entries.insert(
            Entry(
              EntryId(s"t${TurnSeq.value(first.turnSeq)}:${next.seq}"),
              c1,
              first.turnSeq,
              None,
              next.seq,
              payload,
              Instant.EPOCH
            )
          )
        }
      }
      prose.lift(p).flatMap(Closing.of(_, None, Vector(), Vector(), Vector(), Vector())).foreach {
        closing =>
          val last = entries.lockNext(c1).getOrElse(sys.error("in-memory store")).turnSeq
          val period = PeriodRef(c1, PeriodSeq.of(p + 1L).getOrElse(sys.error("period")))
          val _ = world.periods.seal(
            CloseRef(period, TurnSeq(TurnSeq.value(last) - 1)),
            CloseReason.Resolved,
            closing,
            Instant.EPOCH
          )
      }
    }
    world
  }

  /** The id of period `n`'s closing entry. */
  def closingOf(n: Long): String =
    EntryId.value(PeriodRef(c1, PeriodSeq.of(n).getOrElse(sys.error("period"))).closingId)
}
