package grit.core.job

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{JobName, PluginName, PrincipalId, TurnRef}
import grit.core.store.{Tombstones, Tx}
import grit.dbos.sql.TestTx

/** An empty in-memory store, as the contracts take one. */
private[job] object InMemoryUnder {
  def apply(): ScheduleContract.Under = {
    val s = new InMemorySchedules
    new ScheduleContract.Under {
      val store: ScheduleStore = s
      val tombstones: Tombstones = s.tombstones
      def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        s.start(slot, version, following)
      def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
        s.asking(turn, by, address)
      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ =
        s.desk(plugin, jobs, clock)
    }
  }
}

/** The schedules contract, kept by the in-memory fake. */
object InMemoryScheduleTests extends ScheduleContract {
  protected def fresh(): ScheduleContract.Under = InMemoryUnder()
}
