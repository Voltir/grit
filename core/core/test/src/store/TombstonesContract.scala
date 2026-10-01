package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, PeriodRef, PeriodSeq, PluginName}
import grit.core.period.CloseOrdinal
import grit.core.retention.{Target, Tombstone}

import utest.*

/** The contract every [[Tombstones]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos. Tests share the store, so each writes tombstones of a kind no other test
  * writes.
  */
abstract class TombstonesContract extends TestSuite {

  protected def tombstones: Tombstones

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def at(minute: Int): Instant =
    Instant.parse("2026-01-01T00:00:00Z").plusSeconds(60L * minute)

  private def period(name: String, seq: Long = 1): PeriodRef =
    PeriodRef(ConversationId(name), PeriodSeq.of(seq).getOrElse(PeriodSeq.First))

  private def plugin(name: String): PluginName = PluginName.of(name).fold(sys.error, identity)

  private def due(kind: Target.Kind, before: Int, n: Int = 100): Vector[Tombstone] =
    transaction(tombstones.due(kind, at(before), n)).fold(e => sys.error(s"$e"), identity)

  val tests = Tests {
    test("a write keeps a pending tombstone's time, and arms a collected or spared one again") {
      val a = Target.Quiet(period("write-a"))
      val b = Target.Quiet(period("write-b"))
      transaction {
        for {
          first <- tombstones.write(a, at(1))
          again <- tombstones.write(a, at(5))
          other <- tombstones.write(b, at(2))
        } yield (first, again, other)
      } ==> Right((true, false, true))
      due(Target.Kind.Quiet, 10) ==> Vector(Tombstone(a, at(1)), Tombstone(b, at(2)))
      transaction {
        for {
          _ <- tombstones.collected(a, at(3))
          _ <- tombstones.spare(b, at(3))
        } yield ()
      }
      due(Target.Kind.Quiet, 10) ==> Vector.empty
      transaction {
        for {
          spared <- tombstones.write(b, at(7))
          collected <- tombstones.write(a, at(6))
        } yield (spared, collected)
      } ==> Right((true, true))
      due(Target.Kind.Quiet, 10) ==> Vector(Tombstone(a, at(6)), Tombstone(b, at(7)))
    }

    test(
      "due is one kind's pending tombstones written before the cutoff, oldest first, at most n"
    ) {
      val older = Target.Raw(period("due-a", 2))
      val newer = Target.Raw(period("due-b"))
      val late = Target.Raw(period("due-c"))
      transaction {
        for {
          _ <- tombstones.write(newer, at(20))
          _ <- tombstones.write(older, at(10))
          _ <- tombstones.write(late, at(40))
          _ <- tombstones.write(Target.Superseded(period("due-d")), at(5))
        } yield ()
      }
      due(Target.Kind.Raw, 30) ==> Vector(Tombstone(older, at(10)), Tombstone(newer, at(20)))
      due(Target.Kind.Raw, 30, 1) ==> Vector(Tombstone(older, at(10)))
      due(Target.Kind.Raw, 20) ==> Vector(Tombstone(older, at(10)))
    }

    test("a deferred tombstone does not hold a later due one") {
      val stuck = Target.PostRuns(plugin("defer"), 1, CloseOrdinal.Start)
      val later =
        Target.PostRuns(plugin("defer"), 1, CloseOrdinal.of(4).getOrElse(CloseOrdinal.Start))
      transaction {
        for {
          _ <- tombstones.write(stuck, at(1))
          _ <- tombstones.write(later, at(2))
        } yield ()
      }
      due(Target.Kind.PostRuns, 10, 1) ==> Vector(Tombstone(stuck, at(1)))
      transaction(tombstones.deferred(stuck, at(8)))
      due(Target.Kind.PostRuns, 10, 1) ==> Vector(Tombstone(later, at(2)))
      due(Target.Kind.PostRuns, 10) ==> Vector(Tombstone(later, at(2)), Tombstone(stuck, at(1)))
    }

    test("forget deletes the tombstones ended before the cutoff, and none still pending") {
      val collected = Target.Disabled(plugin("forget-a"))
      val spared = Target.Disabled(plugin("forget-b"))
      val recent = Target.Disabled(plugin("forget-c"))
      val pending = Target.Disabled(plugin("forget-d"))
      transaction {
        for {
          _ <- tombstones.write(collected, at(1))
          _ <- tombstones.write(spared, at(1))
          _ <- tombstones.write(recent, at(1))
          _ <- tombstones.write(pending, at(1))
          _ <- tombstones.collected(collected, at(2))
          _ <- tombstones.spare(spared, at(3))
          _ <- tombstones.collected(recent, at(9))
        } yield ()
      }
      transaction(tombstones.forget(at(5))) ==> Right(2)
      due(Target.Kind.Disabled, 10) ==> Vector(Tombstone(pending, at(1)))
      // Forgotten, a target is written afresh; the one ended since is armed again.
      transaction {
        for {
          _ <- tombstones.write(collected, at(6))
          _ <- tombstones.write(recent, at(7))
        } yield ()
      }
      due(Target.Kind.Disabled, 10) ==>
        Vector(Tombstone(pending, at(1)), Tombstone(collected, at(6)), Tombstone(recent, at(7)))
    }
  }
}
