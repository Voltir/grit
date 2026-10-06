package grit.job.run

import java.time.Instant

import grit.core.clock.SetClock
import grit.core.durable.InMemoryDurable
import grit.core.edge.{InMemoryDeliveries, Pending}
import grit.core.id.{
  ConversationId,
  Declarer,
  PluginName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TestCallSlots,
  TurnRef,
  TurnSeq
}
import grit.core.inbox.{InMemoryInbox, Slotted}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{Declared, Job, JobRun, Jobs, Schedule, Slot, SlotRule, When}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Db, Jot, Origin, Payload, StoreError, Tx}
import grit.dbos.sql.TestTx

/** A run's world over core's in-memory fakes: an inbox that starts slots as the SQL one does,
  * its schedules, the deliveries an edge reads, and a clock.
  */
object RunFixtures {

  /** When the slots below are due. */
  val Due: Instant = Instant.parse("2026-10-07T09:00:00Z")

  /** `remind` at version 1, the job every slot below is started at. */
  val remind = new Counting("remind")

  /** The deployment of `remind` at `version`. */
  def jobs(version: Int): Jobs = jobsOf(new Counting("remind", version))

  def jobsOf(job: Job[?]*): Jobs =
    Jobs.of(job.toVector).fold(n => sys.error(s"two jobs named $n"), identity)

  /** A job named `remind` whose parameters are text: it cannot read a count. */
  final class Texting extends Job[Text] {
    val name = remind.name
    val version = 1
    def write(params: Text): ujson.Value = ujson.Str(params.text)
    def read(params: ujson.Value): Either[String, Text] =
      params.strOpt.map(Text(_)).toRight(s"not text: $params")
    def reply(run: JobRun[Text]): String = run.params.text
  }

  final case class Text(text: String) extends caps.Pure

  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Writes as its transaction is committed, nothing rolled back; with `crashAfter`, the
    * process dies once just after the first write commits.
    */
  final class FakeJot(crashAfter: Boolean = false) extends Jot {
    @caps.unsafe.untrackedCaptures
    private var armed = crashAfter
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
      val written = body(using TestTx.fake)
      if (armed) { armed = false; throw new InMemoryDurable.Crash }
      written
    }
  }

  final class World extends caps.SharedCapability {
    val inbox: InMemoryInbox = InMemoryInbox.fresh()
    val deliveries: InMemoryDeliveries = new InMemoryDeliveries
    val clock: SetClock = new SetClock(Due.plusSeconds(30))

    def env(jot: Jot^ = new FakeJot()): RunEnv^ =
      RunEnv(
        RunRecords(inbox.entries, inbox.conversations, inbox.schedules, deliveries),
        FakeDb,
        jot,
        clock
      )

    /** A declared once schedule of `remind`, keyed `key`, with the count `n`, due at [[Due]]. */
    def declared(key: String, n: Int = 1): ScheduleId = {
      val k = ScheduleKey.of(key).fold(sys.error, identity)
      ok(
        inbox.schedules.declare(
          Vector(Declarer.Deployment -> Declared(k, remind, SlotRule.Once(Due, hour), Count(n))),
          Due.minusSeconds(60)
        )(using TestTx.fake)
      )
      ScheduleId.declared(Declarer.Deployment, k)
    }

    /** A schedule of `remind` with the count `n`, asked in `thread`, a Slack thread whose
      * reply was posted at `address`, due at [[Due]].
      */
    def asked(thread: String, address: String, n: Int = 1): ScheduleId = {
      val origin = Origin.Slack("T1", "C1", thread)
      val turn = TurnRef(ConversationId(thread), TurnSeq.First)
      inbox.schedules.asking(turn, origin, PrincipalId("ann"), Some(address))
      val plugin = PluginName.of("reminders").fold(sys.error, identity)
      val desk =
        inbox.schedules.desk(plugin, Vector(remind.name), new SetClock(Due.minusSeconds(3600)))
      desk
        .ask(TestCallSlots.at(turn), booking(remind), When.At(Due), hour, Count(n))
        .fold(r => sys.error(s"$r"), _.id)
    }

    /** `schedule`'s due slot started at `version`, as the clock edge starts it: its run's turn. */
    def started(schedule: ScheduleId, version: Int = 1): TurnRef =
      inbox.startSlot(schedule, Some(version), Due) match {
        case Right(Slotted.Started(turn, _)) => turn
        case Right(Slotted.Superseding(turn, _)) => turn
        case other => sys.error(s"not started: $other")
      }

    def schedule(id: ScheduleId): Option[Schedule] = ok(inbox.schedules.read(id)(using TestTx.fake))

    /** `turn`'s reply, as its entry holds it. */
    def reply(turn: TurnRef): Option[String] =
      ok(inbox.entries.get(turn.replyId)(using TestTx.fake)).collect {
        case e if e.turnSeq == turn.turnSeq =>
          e.payload match {
            case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
              blocks.collect { case AssistantBlock.Text(t) => t }.mkString
            case other => s"not a reply: $other"
          }
      }

    /** How many entries `turn` has. */
    def entries(turn: TurnRef): Int = ok(inbox.entries.ofTurn(turn)(using TestTx.fake)).size

    def pending: Vector[Pending] = ok(deliveries.pending()(using TestTx.fake))

    /** `slot`'s schedule's run in flight, as the inbox reads it: the slot and its version. */
    def running(id: ScheduleId): Option[(Instant, Option[Int])] =
      inbox.schedules.held(id).flatMap(_._4)
  }

  def ok[A](e: Either[StoreError, A]): A = e.fold(err => sys.error(s"$err"), identity)
}
