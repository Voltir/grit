package grit.core.job

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  JobName,
  PluginName,
  PrincipalId,
  ScheduleId,
  TurnRef
}
import grit.core.retention.Target
import grit.core.store.{InMemoryTombstones, StoreError, Tx}
import grit.dbos.sql.TestTx

/** An in-memory [[ScheduleStore]], with each plugin's [[ScheduleDesk]], for tests, keeping
  * [[ScheduleContract]] and [[DeskContract]]. What a desk derives from a call's turn in the
  * database (who asked, where its reply is posted) it reads from what [[asking]] recorded. It
  * marks ended schedules in `tombstones`, and ignores the `Tx`: nothing is rolled back.
  */
final class InMemorySchedules(val tombstones: InMemoryTombstones = new InMemoryTombstones())
    extends ScheduleStore {
  import InMemorySchedules.Row

  // Only ever replaced by a new immutable map, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[ScheduleId, Row]

  // As `rows`: each recorded turn's asker, and where its reply is posted, if anywhere.
  @caps.unsafe.untrackedCaptures
  private var turns = Map.empty[TurnRef, (PrincipalId, Option[String])]

  /** `turn` recorded as rooted on a message `by` wrote, its reply posted at `address`, or
    * nowhere: what the database holds of a turn as its entries and deliveries.
    */
  def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
    turns = turns.updated(turn, (by, address))

  /** `slot`'s run started at `version`, its schedule's next slot `following`, as the inbox
    * starts one.
    */
  def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
    rows = rows.updatedWith(slot.schedule)(
      _.map(_.copy(next = following, started = Some(slot.nominal), running = Some(version)))
    )

  def declare(declared: Vector[(Declarer, Declared[?])], now: Instant)(using
      Tx^
  ): Either[StoreError, Unit] = {
    val wanted = declared.map((by, d) => d.id(by) -> d)
    val ids = wanted.map(_._1).toSet
    wanted.foreach { (id, d) =>
      val row = rows.get(id) match {
        case None =>
          Row(d.job.name, d.written, PrincipalId.Grit, Report.Kept, d.rule, d.rule.first(now))
        case Some(r) =>
          val ended = r.ended.filterNot(_ == Ending.Undeclared)
          r.copy(
            job = d.job.name,
            params = d.written,
            rule = d.rule,
            next =
              if (ended.nonEmpty) None
              else if (r.rule == d.rule && r.ended.isEmpty) r.next
              else d.rule.first(now),
            ended = ended
          )
      }
      rows = rows.updated(id, row)
    }
    rows.toVector
      .collect { case (id, r) if r.asked.isEmpty && r.ended.isEmpty && !ids(id) => id }
      .foldLeft[Either[StoreError, Unit]](Right(()))((acc, id) =>
        acc.flatMap(_ => end(id, Ending.Undeclared, now))
      )
  }

  def waiting(now: Instant, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    Right(
      rows.toVector
        .collect {
          case (id, r)
              if r.ended.isEmpty && (r.running.nonEmpty || r.next.exists(!_.isAfter(now))) =>
            (r.running.flatMap(_ => r.started).orElse(r.next), id, r.job)
        }
        .sortBy((due, id, _) => (due.getOrElse(Instant.MIN), ScheduleId.value(id)))
        .take(n max 0)
        .map((_, id, job) => (id, job))
    )

  def replied(slot: Slot, version: Int, at: Instant)(using Tx^): Either[StoreError, Unit] =
    rows.get(slot.schedule) match {
      case Some(r) if r.running.contains(version) && r.started.contains(slot.nominal) =>
        rows = rows.updated(slot.schedule, r.copy(running = None))
        r.rule match {
          case SlotRule.Once(_, _) if r.ended.isEmpty => end(slot.schedule, Ending.Ran, at)
          case _ => Right(())
        }
      case _ => Right(())
    }

  def read(id: ScheduleId)(using Tx^): Either[StoreError, Option[Schedule]] =
    Right(rows.get(id).map(r => Schedule(r.job, r.params, r.principal, r.report, r.rule, r.ended)))

  /** `plugin`'s desk, holding the job names `jobs`, its now `clock`'s. */
  def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ =
    new ScheduleDesk {
      def ask[P <: caps.Pure](
          call: CallSlot,
          booking: Booking[P],
          when: When,
          grace: Grace,
          params: P
      ): Either[DeskRefusal, Asked[P]] = {
        val id = ScheduleId.asked(call)
        for {
          _ <- own(booking)
          asked <- rows.get(id) match {
            case Some(r) => kept(id, r, booking.job)
            case None =>
              turns.get(call.turn) match {
                case Some((by, Some(address))) =>
                  val now = clock.now()
                  val at = when.from(now).truncatedTo(java.time.temporal.ChronoUnit.MICROS)
                  val limit = now.plusNanos(ScheduleDesk.Horizon.toNanos)
                  if (!at.isAfter(now)) Left(DeskRefusal.Past(at, now))
                  else if (at.isAfter(limit)) Left(DeskRefusal.TooFar(at, limit))
                  else if (pendingOf(by).size >= ScheduleDesk.PendingCap)
                    Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap))
                  else {
                    rows = rows.updated(
                      id,
                      Row(
                        booking.job.name,
                        booking.job.write(params),
                        by,
                        Report.Posted(address),
                        SlotRule.Once(at, grace),
                        Some(at),
                        asked = Some(call.turn.conversationId)
                      )
                    )
                    Right(Asked(id, at, params))
                  }
                case _ => Left(DeskRefusal.Unaddressed)
              }
          }
        } yield asked
      }

      def pending[P <: caps.Pure](
          call: CallSlot,
          booking: Booking[P]
      ): Either[DeskRefusal, Pending[P]] =
        own(booking).map { _ =>
          val now = clock.now()
          val mine = turns.get(call.turn).fold(Vector.empty)((by, _) => pendingOf(by))
          Pending(
            now,
            mine
              .collect { case (id, r) if r.job == booking.job.name => kept(id, r, booking.job) }
              .collect { case Right(a) => a }
              .sortBy(a => (a.at, ScheduleId.value(a.id)))
          )
        }

      def cancel(call: CallSlot, booking: Booking[?], id: ScheduleId): Either[DeskRefusal, Unit] =
        own(booking).flatMap { _ =>
          val asker = turns.get(call.turn).map(_._1)
          rows.get(id).filter(r => r.asked.nonEmpty && asker.contains(r.principal)) match {
            case Some(r) if r.job == booking.job.name =>
              r.ended match {
                case Some(how) => Left(DeskRefusal.Ended(id, how))
                case None =>
                  end(id, Ending.Cancelled, clock.now())(using TestTx.fake).left
                    .map(DeskRefusal.Unavailable(_))
              }
            case _ => Left(DeskRefusal.NotFound(id))
          }
        }

      private def own(booking: Booking[?]): Either[DeskRefusal, Unit] =
        Either.cond(
          jobs.contains(booking.job.name),
          (),
          DeskRefusal.NotOwn(plugin, booking.job.name)
        )
    }

  /** `by`'s pending asked schedules. */
  private def pendingOf(by: PrincipalId): Vector[(ScheduleId, Row)] =
    rows.toVector.filter((_, r) => r.asked.nonEmpty && r.principal == by && r.ended.isEmpty)

  /** `r`, an asked schedule, as `job` reads it. */
  private def kept[P <: caps.Pure](
      id: ScheduleId,
      r: Row,
      job: Job[P]
  ): Either[DeskRefusal, Asked[P]] =
    r.rule match {
      case SlotRule.Once(at, _) =>
        job
          .read(r.params)
          .map(Asked(id, at, _))
          .left
          .map(why => DeskRefusal.Unavailable(StoreError.Invalid(why)))
      case other => Left(DeskRefusal.Unavailable(StoreError.Invalid(s"asked, but $other")))
    }

  private def end(id: ScheduleId, how: Ending, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    rows = rows.updatedWith(id)(_.map(_.copy(next = None, ended = Some(how))))
    tombstones.write(Target.Schedule(id), at).map(_ => ())
  }
}

object InMemorySchedules {

  /** One schedule's row: `asked`, the conversation that asked for it; `None` for one declared. */
  private final case class Row(
      job: JobName,
      params: ujson.Value,
      principal: PrincipalId,
      report: Report,
      rule: SlotRule,
      next: Option[Instant],
      started: Option[Instant] = None,
      running: Option[Int] = None,
      ended: Option[Ending] = None,
      asked: Option[ConversationId] = None
  )
}
