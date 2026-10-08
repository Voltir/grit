package grit.core.job

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{ConversationId, JobName, PluginName, PrincipalId, TurnRef, TurnSeq}
import grit.core.identity.{Account, TestAccounts}
import grit.core.store.{Origin, Tombstones, Tx}
import grit.core.visibility.{GroupName, InMemoryRecorded, Label, TestLabels}
import grit.dbos.sql.TestTx

/** An empty in-memory store, as the contracts take one. */
private[job] object InMemoryUnder {
  def apply(): ScheduleContract.Under = {
    val records = new InMemoryRecorded()
    val s = new InMemorySchedules(visibility = TestLabels.Trialled, records = records)
    // Each conversation's origin and label, as the database keeps them in its row.
    val origins = scala.collection.mutable.Map.empty[ConversationId, (Origin, Label)]
    new ScheduleContract.Under {
      val store: ScheduleStore = s
      val tombstones: Tombstones = s.tombstones
      def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        s.start(slot, version, following)
      def turn(origin: Origin, label: Label): TurnRef = {
        val id = ConversationId(origin.place.written)
        if (!origins.contains(id)) origins(id) = (origin, label)
        TurnRef(id, TurnSeq.First)
      }
      def asking(turn: TurnRef, by: Account, address: Option[String]): Unit =
        origins.get(turn.conversationId) match {
          case Some((from, label)) => s.asking(turn, from, by, address, label)
          case None => sys.error(s"asked from $turn, which no turn(origin) made")
        }
      def added(account: Account, group: GroupName): Unit = {
        val _ = records.group(group)(_ + account)
      }
      def principal(account: Account): PrincipalId = TestAccounts.principalId(account)
      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ =
        s.desk(plugin, jobs, clock)
    }
  }
}

/** The schedules contract, kept by the in-memory fake. */
object InMemoryScheduleTests extends ScheduleContract {
  protected def fresh(): ScheduleContract.Under = InMemoryUnder()
}
