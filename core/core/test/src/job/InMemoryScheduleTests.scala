package grit.core.job

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{ConversationId, JobName, PluginName, PrincipalId, TurnRef, TurnSeq}
import grit.core.store.{Origin, Tombstones, Tx}
import grit.dbos.sql.TestTx

/** An empty in-memory store, as the contracts take one. */
private[job] object InMemoryUnder {
  def apply(): ScheduleContract.Under = {
    val s = new InMemorySchedules
    // Each conversation's origin, as the database keeps it in the conversation's row.
    val origins = scala.collection.mutable.Map.empty[ConversationId, Origin]
    new ScheduleContract.Under {
      val store: ScheduleStore = s
      val tombstones: Tombstones = s.tombstones
      def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        s.start(slot, version, following)
      def turn(origin: Origin): TurnRef = {
        val id = ConversationId(origin.place.written)
        origins(id) = origin
        TurnRef(id, TurnSeq.First)
      }
      def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
        origins.get(turn.conversationId) match {
          case Some(from) => s.asking(turn, from, by, address)
          case None => sys.error(s"asked from $turn, which no turn(origin) made")
        }
      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ =
        s.desk(plugin, jobs, clock)
    }
  }
}

/** The schedules contract, kept by the in-memory fake. */
object InMemoryScheduleTests extends ScheduleContract {
  protected def fresh(): ScheduleContract.Under = InMemoryUnder()
}
