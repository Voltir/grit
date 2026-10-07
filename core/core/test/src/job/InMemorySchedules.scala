package grit.core.job

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{CallSlot, Declarer, JobName, PluginName, PrincipalId, ScheduleId, TurnRef}
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{InMemoryTombstones, Origin, StoreError, Tx}
import grit.core.visibility.{Clearance, Item, Label, Visibility}
import grit.dbos.sql.TestTx

/** An in-memory [[ScheduleStore]], with each plugin's [[ScheduleDesk]], for tests, keeping
  * [[ScheduleContract]] and [[DeskContract]]. What a desk derives from a call's turn in the
  * database (who asked, where its reply is posted, what a transaction opened for it reads) it
  * reads from what [[asking]] recorded, resolving the asker's clearance under `visibility` as
  * an opener would. It marks ended schedules in `tombstones`, and ignores the `Tx`: nothing is
  * rolled back.
  */
final class InMemorySchedules(
    val tombstones: InMemoryTombstones = new InMemoryTombstones(),
    visibility: Visibility = Visibility.Shipped
) extends ScheduleStore {
  import InMemorySchedules.Row

  // Only ever replaced by a new immutable map, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[ScheduleId, Row]

  // As `rows`: each recorded turn's asker, where its reply is posted, if anywhere, its room,
  // and the clearance a transaction opened for it holds.
  @caps.unsafe.untrackedCaptures
  private var turns = Map.empty[TurnRef, (PrincipalId, Option[Destination], Place, Clearance)]

  /** `turn`, of `from`'s conversation, created at `label`, recorded as rooted on a message `by`
    * wrote, its reply posted at `address`, or nowhere: what the database holds of a turn as its
    * conversation, entries and deliveries.
    */
  def asking(
      turn: TurnRef,
      from: Origin,
      by: PrincipalId,
      address: Option[String],
      label: Label = Label.Public
  ): Unit = {
    // As grit.core.visibility.Subject.Turn resolves: the asker is the first entry's author.
    val clearance = Clearance.inRoom(from.room, label, visibility.cleared(by))
    turns = turns.updated(turn, (by, address.map(Destination(from.edge, _)), from.room, clearance))
  }

  /** `slot`'s run started at `version`, its schedule's next slot `following`, as the inbox
    * starts one.
    */
  def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
    rows = rows.updatedWith(slot.schedule)(
      _.map(_.copy(next = following, started = Some(slot.nominal), running = Some(version)))
    )

  /** `id`'s job, rule, next slot, the run it last started (its slot, and its version while in
    * flight), and its label; `None` when it is gone or has ended: as the SQL store reads a
    * schedule it starts.
    */
  def held(
      id: ScheduleId
  ): Option[(JobName, SlotRule, Option[Instant], Option[(Instant, Option[Int])], Label)] =
    rows
      .get(id)
      .filter(_.ended.isEmpty)
      .map(r => (r.job, r.rule, r.next, r.started.map(_ -> r.running), r.label))

  /** `id`'s run in flight ended without a reply, at `at`: a once schedule ends failed; a
    * recurrence lets it go and keeps its next slot.
    */
  def failed(id: ScheduleId, at: Instant): Either[StoreError, Unit] =
    rows.get(id) match {
      case Some(r) =>
        rows = rows.updated(id, r.copy(running = None))
        r.rule match {
          case SlotRule.Once(_, _) => end(id, Ending.Failed, at)(using TestTx.fake)
          case _ => Right(())
        }
      case None => Right(())
    }

  /** `id`'s once slot missed, at `at`. */
  def missed(id: ScheduleId, at: Instant): Either[StoreError, Unit] =
    end(id, Ending.Missed, at)(using TestTx.fake)

  /** `id`'s next slot was run already, at `at`: a once schedule ends ran; a recurrence's next
    * slot becomes `following`.
    */
  def passed(id: ScheduleId, following: Option[Instant], at: Instant): Either[StoreError, Unit] =
    rows.get(id) match {
      case Some(r) =>
        rows = rows.updated(id, r.copy(next = following))
        r.rule match {
          case SlotRule.Once(_, _) => end(id, Ending.Ran, at)(using TestTx.fake)
          case _ => Right(())
        }
      case None => Right(())
    }

  /** `id`'s row deleted when it has ended; whether none is left: as the SQL store's collector
    * forgets an ended schedule.
    */
  def forget(id: ScheduleId): Boolean =
    rows.get(id) match {
      case Some(r) if r.ended.nonEmpty => rows = rows.removed(id); true
      case Some(_) => false
      case None => true
    }

  def declare(declared: Vector[(Declarer, Declared[?])], now: Instant)(using
      Tx^
  ): Either[StoreError, Unit] = {
    val wanted = declared.map((by, d) => d.id(by) -> d)
    val ids = wanted.map(_._1).toSet
    wanted.foreach { (id, d) =>
      val row = rows.get(id) match {
        case None =>
          Row(
            d.job.name,
            d.written,
            PrincipalId.Grit,
            Report.Kept,
            d.rule,
            d.rule.first(now).map(Slot.kept),
            d.clearance
          )
        case Some(r) =>
          val ended = r.ended.filterNot(_ == Ending.Undeclared)
          r.copy(
            job = d.job.name,
            params = d.written,
            rule = d.rule,
            label = d.clearance,
            next =
              if (ended.nonEmpty) None
              else if (r.rule == d.rule && r.ended.isEmpty) r.next
              else d.rule.first(now).map(Slot.kept),
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

  def due(now: Instant, n: Int)(using Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    Right(named(n) {
      case (id, r) if r.ended.isEmpty && r.running.isEmpty && r.next.exists(!_.isAfter(now)) =>
        (r.next, id, r.job)
    })

  def inFlight(n: Int)(using Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    Right(named(n) {
      case (id, r) if r.ended.isEmpty && r.running.nonEmpty => (r.started, id, r.job)
    })

  /** At most `n` of the rows `pick` selects, by the instant it gives them, ties by id. */
  private def named(n: Int)(
      pick: PartialFunction[(ScheduleId, Row), (Option[Instant], ScheduleId, JobName)]
  ): Vector[(ScheduleId, JobName)] =
    rows.toVector
      .collect(pick)
      .sortBy((at, id, _) => (at.getOrElse(Instant.MIN), ScheduleId.value(id)))
      .take(n max 0)
      .map((_, id, job) => (id, job))

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
    Right(
      rows
        .get(id)
        .map(r => Schedule(r.job, r.params, r.principal, r.report, r.rule, r.ended, r.label))
    )

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
                case Some((by, Some(to), room, clearance)) =>
                  val now = clock.now()
                  val at = Slot.kept(when.from(now))
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
                        Report.Posted(to),
                        SlotRule.Once(at, grace),
                        Some(at),
                        clearance.floor,
                        asked = Some(room)
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
          val mine =
            turns.get(call.turn).fold(Vector.empty)((by, _, _, c) => pendingOf(by).filter(read(c)))
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
          val asker = turns.get(call.turn)
          rows
            .get(id)
            .filter(r =>
              asker.exists((by, _, _, c) => r.principal == by && read(c)(id -> r))
            ) match {
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

  /** Whether `clearance` reads an asked schedule: as anything recorded in the room it was asked in. */
  private def read(clearance: Clearance)(schedule: (ScheduleId, Row)): Boolean =
    schedule._2.asked.exists(room => clearance.reads(Item.InRoom(room), schedule._2.label))

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

  /** One schedule's row: `asked`, the room of the conversation that asked for it; `None` for one declared. */
  private final case class Row(
      job: JobName,
      params: ujson.Value,
      principal: PrincipalId,
      report: Report,
      rule: SlotRule,
      next: Option[Instant],
      label: Label,
      started: Option[Instant] = None,
      running: Option[Int] = None,
      ended: Option[Ending] = None,
      asked: Option[Place] = None
  )
}
