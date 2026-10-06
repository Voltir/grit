package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.util.Using

import grit.core.clock.Clock
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  JobName,
  PluginName,
  PrincipalId,
  ScheduleId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.job.{
  Asked,
  Booking,
  Declared,
  DeskRefusal,
  Destination,
  Ending,
  Grace,
  Job,
  Pending,
  Report,
  ReportJson,
  Schedule,
  ScheduleDesk,
  ScheduleStore,
  Slot,
  SlotRule,
  SlotRuleJson,
  When
}
import grit.core.retention.Target
import grit.core.store.{Jot, StoreError, Tombstones, Tx}

/** [[ScheduleStore]] over `grit.schedules`, marking in `tombstones` each schedule it ends, and
  * each plugin's [[ScheduleDesk]] over the same rows. A desk's asker is the author
  * (`grit.inbound`) of its call's turn's first entry, and its address that turn's
  * `grit.deliveries` row's. Ids order bytewise (`COLLATE "C"`), as the in-memory fake orders
  * them; instants are kept to the microsecond.
  */
final class SqlSchedules(tombstones: Tombstones) extends ScheduleStore {
  import SqlEntryStore.attempt
  import SqlSchedules.*

  def declare(declared: Vector[(Declarer, Declared[?])], now: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val wanted = declared.map((by, d) => d.id(by) -> d)
    for {
      _ <- wanted.foldLeft[Either[StoreError, Unit]](Right(())) { case (acc, (id, d)) =>
        acc.flatMap(_ => declareOne(id, d, now))
      }
      dropped <- many(
        """UPDATE grit.schedules SET ended = 'undeclared', ended_at = ?, next_at = NULL
          | WHERE source = 'declared' AND ended IS NULL
          |   AND id NOT IN (SELECT jsonb_array_elements_text(?::jsonb))
          | RETURNING id""".stripMargin
      ) { ps =>
        ps.setObject(1, utc(now))
        ps.setString(
          2,
          ujson.Arr.from(wanted.map((id, _) => ujson.Str(ScheduleId.value(id)))).render()
        )
      }(rs => rs.getString(1))
      _ <- dropped.sorted.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, text) =>
        acc.flatMap(_ =>
          ScheduleId
            .of(text)
            .left
            .map(StoreError.Invalid(_))
            .flatMap(id => tombstones.write(Target.Schedule(id), now).map(_ => ()))
        )
      }
    } yield ()
  }

  /** `d`, as `id`, made the stored one at `now`. */
  private def declareOne(id: ScheduleId, d: Declared[?], now: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    many("SELECT rule, ended FROM grit.schedules WHERE id = ? FOR UPDATE")(
      _.setString(1, ScheduleId.value(id))
    )(rs => (SlotRuleJson.read(ujson.read(rs.getString(1))), Option(rs.getString(2))))
      .flatMap(_.headOption match {
        case None =>
          update(
            """INSERT INTO grit.schedules
              |  (id, source, job, rule, params, principal, report, created_at, next_at)
              |VALUES (?, 'declared', ?, ?::jsonb, ?::jsonb, ?, ?::jsonb, ?, ?)""".stripMargin
          ) { ps =>
            ps.setString(1, ScheduleId.value(id))
            ps.setString(2, JobName.value(d.job.name))
            ps.setString(3, SlotRuleJson.write(d.rule).render())
            ps.setString(4, d.written.render())
            ps.setString(5, PrincipalId.value(PrincipalId.Grit))
            ps.setString(6, ReportJson.write(Report.Kept).render())
            ps.setObject(7, utc(now))
            ps.setObject(8, d.rule.first(now).map(Slot.kept).map(utc).orNull)
          }
        case Some((rule, ended)) =>
          val revived = ended.contains(Ending.Undeclared.word)
          val keepsEnd = ended.nonEmpty && !revived
          val keepsNext = !keepsEnd && !revived && rule == Right(d.rule)
          update(
            """UPDATE grit.schedules
              |   SET job = ?, rule = ?::jsonb, params = ?::jsonb,
              |       next_at = CASE WHEN ? THEN next_at ELSE ? END,
              |       ended = CASE WHEN ? THEN NULL ELSE ended END,
              |       ended_at = CASE WHEN ? THEN NULL ELSE ended_at END
              | WHERE id = ?""".stripMargin
          ) { ps =>
            ps.setString(1, JobName.value(d.job.name))
            ps.setString(2, SlotRuleJson.write(d.rule).render())
            ps.setString(3, d.written.render())
            ps.setBoolean(4, keepsNext)
            ps.setObject(
              5,
              if (keepsEnd) null else d.rule.first(now).map(Slot.kept).map(utc).orNull
            )
            ps.setBoolean(6, revived)
            ps.setBoolean(7, revived)
            ps.setString(8, ScheduleId.value(id))
          }
      })
      .map(_ => ())

  def due(now: Instant, n: Int)(using tx: Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    named(
      """SELECT id, job FROM grit.schedules
        | WHERE ended IS NULL AND running IS NULL AND next_at <= ?
        | ORDER BY next_at, id COLLATE "C"
        | LIMIT ?""".stripMargin
    ) { ps =>
      ps.setObject(1, utc(now))
      ps.setInt(2, n max 0)
    }

  def inFlight(n: Int)(using tx: Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    named(
      """SELECT id, job FROM grit.schedules
        | WHERE ended IS NULL AND running IS NOT NULL
        | ORDER BY started_at, id COLLATE "C"
        | LIMIT ?""".stripMargin
    )(_.setInt(1, n max 0))

  /** The schedules `sql` selects, as their ids and jobs. */
  private def named(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Vector[(ScheduleId, JobName)]] =
    many(sql)(bind)(rs => (rs.getString(1), rs.getString(2))).flatMap(rows =>
      traverse(rows) { (id, job) =>
        for {
          i <- ScheduleId.of(id)
          j <- JobName.of(job).left.map(why => s"schedule $id: $why")
        } yield (i, j)
      }
    )

  def replied(slot: Slot, version: Int, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    many(
      """UPDATE grit.schedules SET running = NULL
        | WHERE id = ? AND running = ? AND started_at = ?
        | RETURNING rule, ended IS NULL""".stripMargin
    ) { ps =>
      ps.setString(1, ScheduleId.value(slot.schedule))
      ps.setInt(2, version)
      ps.setObject(3, utc(slot.nominal))
    }(rs => (SlotRuleJson.read(ujson.read(rs.getString(1))), rs.getBoolean(2))).flatMap(
      _.headOption match {
        case Some((Right(SlotRule.Once(_, _)), true)) => end(slot.schedule, Ending.Ran, at)
        case Some((Left(why), _)) => Left(StoreError.Invalid(s"schedule ${slot.key}: $why"))
        case _ => Right(())
      }
    )

  def read(id: ScheduleId)(using tx: Tx^): Either[StoreError, Option[Schedule]] =
    many(
      "SELECT job, params, principal, report, rule, ended FROM grit.schedules WHERE id = ?"
    )(_.setString(1, ScheduleId.value(id)))(stored).flatMap(_.headOption match {
      case None => Right(None)
      case Some(row) => row.left.map(why => invalid(id, why)).map(Some(_))
    })

  /** `id`'s job, its rule, its next slot, and the run it last started (its slot, and the
    * version of that run while it is in flight), its row locked until the transaction ends;
    * `None` when it is gone or has ended. What the inbox reads as it starts a slot.
    */
  private[dbos] def held(id: ScheduleId)(using
      tx: Tx^
  ): Either[StoreError, Option[SqlSchedules.Waiting]] =
    many(
      """SELECT job, rule, next_at, started_at, running FROM grit.schedules
        | WHERE id = ? AND ended IS NULL FOR UPDATE""".stripMargin
    )(_.setString(1, ScheduleId.value(id))) { rs =>
      val started = Option(rs.getObject("started_at", classOf[OffsetDateTime])).map(_.toInstant)
      val running = Option(rs.getObject("running", classOf[Integer])).map(_.intValue)
      for {
        job <- JobName.of(rs.getString("job"))
        rule <- SlotRuleJson.read(ujson.read(rs.getString("rule")))
        last <- (started, running) match {
          case (Some(slot), flight) => Right(Some(slot -> flight))
          case (None, None) => Right(None)
          case (None, Some(v)) => Left(s"a run at v$v is in flight, of no slot")
        }
      } yield SqlSchedules.Waiting(
        job,
        rule,
        Option(rs.getObject("next_at", classOf[OffsetDateTime])).map(_.toInstant),
        last
      )
    }.flatMap(_.headOption match {
      case None => Right(None)
      case Some(row) => row.left.map(why => invalid(id, why)).map(Some(_))
    })

  /** `id`'s run in flight ended without a reply, at `at`: a once schedule ends
    * [[Ending.Failed]]; a recurrence lets it go and keeps its next slot.
    */
  private[dbos] def failed(id: ScheduleId, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    many("UPDATE grit.schedules SET running = NULL WHERE id = ? RETURNING rule")(
      _.setString(1, ScheduleId.value(id))
    )(rs => SlotRuleJson.read(ujson.read(rs.getString(1)))).flatMap(_.headOption match {
      case Some(Right(SlotRule.Once(_, _))) => end(id, Ending.Failed, at)
      case Some(Left(why)) => Left(invalid(id, why))
      case _ => Right(())
    })

  /** `id`'s once slot missed, at `at`: it ends [[Ending.Missed]]. */
  private[dbos] def missed(id: ScheduleId, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    end(id, Ending.Missed, at)

  /** `id`'s next slot was run already, as `Starting.Passed` says, at `at`: a once schedule ends
    * [[Ending.Ran]]; a recurrence's next slot becomes `following`.
    */
  private[dbos] def passed(id: ScheduleId, following: Option[Instant], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    many("UPDATE grit.schedules SET next_at = ? WHERE id = ? RETURNING rule") { ps =>
      ps.setObject(1, following.map(utc).orNull)
      ps.setString(2, ScheduleId.value(id))
    }(rs => SlotRuleJson.read(ujson.read(rs.getString(1)))).flatMap(_.headOption match {
      case Some(Right(SlotRule.Once(_, _))) => end(id, Ending.Ran, at)
      case Some(Left(why)) => Left(invalid(id, why))
      case _ => Right(())
    })

  /** `slot`'s run started at `version`, its schedule's next slot `following`: what the inbox
    * writes as it starts one.
    */
  private[dbos] def started(slot: Slot, version: Int, following: Option[Instant])(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    update(
      "UPDATE grit.schedules SET next_at = ?, started_at = ?, running = ? WHERE id = ?"
    ) { ps =>
      ps.setObject(1, following.map(utc).orNull)
      ps.setObject(2, utc(slot.nominal))
      ps.setInt(3, version)
      ps.setString(4, ScheduleId.value(slot.schedule))
    }.map(_ => ())

  /** `id`'s row deleted when it has ended; whether none is left (`false`: it is pending, revived
    * by a declaration since it ended). For the collector, under `id`'s due tombstone.
    */
  private[dbos] def forget(id: ScheduleId)(using tx: Tx^): Either[StoreError, Boolean] =
    many(
      """WITH gone AS (DELETE FROM grit.schedules WHERE id = ? AND ended IS NOT NULL RETURNING id)
        |SELECT EXISTS (SELECT 1 FROM gone)
        |    OR NOT EXISTS (SELECT 1 FROM grit.schedules WHERE id = ?)""".stripMargin
    ) { ps =>
      ps.setString(1, ScheduleId.value(id))
      ps.setString(2, ScheduleId.value(id))
    }(_.getBoolean(1)).map(_.headOption.contains(true))

  /** `plugin`'s desk, holding the job names `jobs`, writing through `jot`, its now `clock`'s. */
  def desk(
      plugin: PluginName,
      jobs: Vector[JobName],
      jot: Jot^,
      clock: Clock^
  ): ScheduleDesk^ =
    new ScheduleDesk {
      def ask[P <: caps.Pure](
          call: CallSlot,
          booking: Booking[P],
          when: When,
          grace: Grace,
          params: P
      ): Either[DeskRefusal, Asked[P]] =
        own(booking).flatMap { _ =>
          val id = ScheduleId.asked(call)
          val now = clock.now()
          val at = Slot.kept(when.from(now))
          val limit = now.plusNanos(ScheduleDesk.Horizon.toNanos)
          written(jot.write {
            row(id).flatMap {
              case Some(r) => Right(kept(id, r.schedule, booking.job))
              case None =>
                asker(call.turn).flatMap {
                  case Some((by, Some(to))) =>
                    if (!at.isAfter(now)) Right(Left(DeskRefusal.Past(at, now)))
                    else if (at.isAfter(limit)) Right(Left(DeskRefusal.TooFar(at, limit)))
                    else
                      pendingOf(by).flatMap { mine =>
                        if (mine.size >= ScheduleDesk.PendingCap)
                          Right(Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap)))
                        else
                          insertAsked(id, call, booking.job, params, by, to, at, grace, now)
                            .map(_ => Right(Asked(id, at, params)))
                      }
                  case _ => Right(Left(DeskRefusal.Unaddressed))
                }
            }
          })
        }

      def pending[P <: caps.Pure](
          call: CallSlot,
          booking: Booking[P]
      ): Either[DeskRefusal, Pending[P]] =
        own(booking).flatMap { _ =>
          val now = clock.now()
          jot
            .write {
              asker(call.turn).flatMap {
                case None => Right(Vector.empty)
                case Some((by, _)) => pendingOf(by)
              }
            }
            .left
            .map(DeskRefusal.Unavailable(_))
            .map { mine =>
              Pending(
                now,
                mine
                  .collect { case (id, r) if r.job == booking.job.name => kept(id, r, booking.job) }
                  .collect { case Right(a) => a }
                  .sortBy(a => (a.at, ScheduleId.value(a.id)))
              )
            }
        }

      def cancel(call: CallSlot, booking: Booking[?], id: ScheduleId): Either[DeskRefusal, Unit] =
        own(booking).flatMap { _ =>
          val now = clock.now()
          written(jot.write {
            for {
              by <- asker(call.turn).map(_.map(_._1))
              r <- row(id, lock = true)
              done <- r.filter(r => r.asked && by.contains(r.schedule.principal)) match {
                case Some(r) if r.schedule.job == booking.job.name =>
                  r.schedule.ended match {
                    case Some(how) => Right(Left(DeskRefusal.Ended(id, how)))
                    case None => end(id, Ending.Cancelled, now).map(Right(_))
                  }
                case _ => Right(Left(DeskRefusal.NotFound(id)))
              }
            } yield done
          })
        }

      private def own(booking: Booking[?]): Either[DeskRefusal, Unit] =
        Either.cond(
          jobs.contains(booking.job.name),
          (),
          DeskRefusal.NotOwn(plugin, booking.job.name)
        )
    }

  /** A desk's transaction's outcome: a store failure is [[DeskRefusal.Unavailable]]. */
  private def written[A](
      e: Either[StoreError, Either[DeskRefusal, A]]
  ): Either[DeskRefusal, A] =
    e.left.map(DeskRefusal.Unavailable(_)).flatMap(identity)

  /** The author of `turn`'s first entry, and where its reply is posted, through its
    * conversation's edge; `None` when that entry is not one a principal wrote, or `turn` holds
    * none.
    */
  private def asker(turn: TurnRef)(using
      tx: Tx^
  ): Either[StoreError, Option[(PrincipalId, Option[Destination])]] =
    many(
      """SELECT i.author, d.address, c.origin::text
        |  FROM (SELECT id FROM grit.entries WHERE conversation_id = ?::uuid AND turn_seq = ?
        |         ORDER BY seq LIMIT 1) e
        |  JOIN grit.inbound i ON i.entry_id = e.id
        |  JOIN grit.conversations c ON c.id = ?::uuid
        |  LEFT JOIN grit.deliveries d ON d.workflow = ?""".stripMargin
    ) { ps =>
      ps.setString(1, ConversationId.value(turn.conversationId))
      ps.setLong(2, TurnSeq.value(turn.turnSeq))
      ps.setString(3, ConversationId.value(turn.conversationId))
      ps.setString(4, WorkflowId.value(turn.workflowId))
    }(rs =>
      SqlConversationStore
        .readOrigin(ujson.read(rs.getString(3)))
        .map(origin =>
          (PrincipalId(rs.getString(1)), Option(rs.getString(2)).map(Destination(origin.edge, _)))
        )
    ).flatMap(rows =>
      rows.headOption match {
        case None => Right(None)
        case Some(row) => row.left.map(StoreError.Invalid(_)).map(Some(_))
      }
    )

  /** `by`'s pending asked schedules, of any job, under a lock on `by` that serialises every
    * desk's asks for them, so none passes the cap beside another.
    */
  private def pendingOf(by: PrincipalId)(using
      tx: Tx^
  ): Either[StoreError, Vector[(ScheduleId, Schedule)]] =
    for {
      _ <- many("SELECT 1 FROM grit.principals WHERE id = ? FOR NO KEY UPDATE")(
        _.setString(1, PrincipalId.value(by))
      )(_ => ()).map(_.size)
      rows <- many(
        """SELECT id, job, params, principal, report, rule, ended FROM grit.schedules
          | WHERE source = 'asked' AND ended IS NULL AND principal = ?""".stripMargin
      )(_.setString(1, PrincipalId.value(by)))(rs => (rs.getString("id"), stored(rs)))
      read <- traverse(rows) { (id, row) =>
        ScheduleId.of(id).flatMap(i => row.map(i -> _))
      }
    } yield read

  /** `id`'s row, whether it was asked, locked for update when `lock`. */
  private def row(id: ScheduleId, lock: Boolean = false)(using
      tx: Tx^
  ): Either[StoreError, Option[Row]] =
    many(
      "SELECT job, params, principal, report, rule, ended, source FROM grit.schedules " +
        "WHERE id = ?" + (if (lock) " FOR UPDATE" else "")
    )(_.setString(1, ScheduleId.value(id))) { rs =>
      stored(rs).map(Row(_, rs.getString("source") == "asked"))
    }.flatMap(_.headOption match {
      case None => Right(None)
      case Some(r) => r.left.map(why => invalid(id, why)).map(Some(_))
    })

  private def insertAsked[P <: caps.Pure](
      id: ScheduleId,
      call: CallSlot,
      job: Job[P],
      params: P,
      by: PrincipalId,
      to: Destination,
      at: Instant,
      grace: Grace,
      now: Instant
  )(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.schedules
        |  (id, source, job, rule, params, principal, report, asked_in, created_at, next_at)
        |VALUES (?, 'asked', ?, ?::jsonb, ?::jsonb, ?, ?::jsonb, ?, ?, ?)""".stripMargin
    ) { ps =>
      ps.setString(1, ScheduleId.value(id))
      ps.setString(2, JobName.value(job.name))
      ps.setString(3, SlotRuleJson.write(SlotRule.Once(at, grace)).render())
      ps.setString(4, job.write(params).render())
      ps.setString(5, PrincipalId.value(by))
      ps.setString(6, ReportJson.write(Report.Posted(to)).render())
      ps.setString(7, call.key)
      ps.setObject(8, utc(now))
      ps.setObject(9, utc(at))
    }.map(_ => ())

  /** `id` ended `how` at `at`, with no slot left, and marked for deletion. */
  private def end(id: ScheduleId, how: Ending, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    update(
      """UPDATE grit.schedules SET ended = ?, ended_at = ?, next_at = NULL
        | WHERE id = ? AND ended IS NULL""".stripMargin
    ) { ps =>
      ps.setString(1, how.word)
      ps.setObject(2, utc(at))
      ps.setString(3, ScheduleId.value(id))
    }.flatMap(_ => tombstones.write(Target.Schedule(id), at)).map(_ => ())

  private def many[A](sql: String)(set: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        set(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[A]
          while (rs.next()) rows += read(rs)
          rows.result()
        }
      }
    }
  }

  private def update(sql: String)(set: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        set(ps)
        ps.executeUpdate()
      }
    }
  }
}

private[dbos] object SqlSchedules {

  /** A stored schedule, and whether it was asked rather than declared. */
  private final case class Row(schedule: Schedule, asked: Boolean)

  /** A pending schedule as the inbox starts it: its job and rule, its next slot, and the slot its
    * last run started for, with that run's version while it is in flight.
    */
  final case class Waiting(
      job: JobName,
      rule: SlotRule,
      next: Option[Instant],
      last: Option[(Instant, Option[Int])]
  )

  private def utc(at: Instant): OffsetDateTime = at.atOffset(ZoneOffset.UTC)

  private def invalid(id: ScheduleId, why: String): StoreError =
    StoreError.Invalid(s"schedule ${ScheduleId.value(id)}: $why")

  /** The schedule the current row of `rs` holds, or why it is none. */
  private def stored(rs: ResultSet): Either[String, Schedule] =
    for {
      job <- JobName.of(rs.getString("job"))
      report <- ReportJson.read(ujson.read(rs.getString("report")))
      rule <- SlotRuleJson.read(ujson.read(rs.getString("rule")))
      ended <- Option(rs.getString("ended")) match {
        case None => Right(None)
        case Some(word) => Ending.read(word).map(Some(_)).toRight(s"no ending $word")
      }
    } yield Schedule(
      job,
      ujson.read(rs.getString("params")),
      PrincipalId(rs.getString("principal")),
      report,
      rule,
      ended
    )

  /** `rows`, each read by `f`, or the first reason one is not. */
  private def traverse[A, B](
      rows: Vector[A]
  )(f: A => Either[String, B]): Either[StoreError, Vector[B]] =
    rows.foldLeft[Either[StoreError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
      acc.flatMap(done => f(a).left.map(StoreError.Invalid(_)).map(done :+ _))
    }

  /** `s`, an asked schedule, as `job` reads it. */
  private def kept[P <: caps.Pure](
      id: ScheduleId,
      s: Schedule,
      job: Job[P]
  ): Either[DeskRefusal, Asked[P]] =
    s.rule match {
      case SlotRule.Once(at, _) =>
        job
          .read(s.params)
          .map(Asked(id, at, _))
          .left
          .map(why => DeskRefusal.Unavailable(StoreError.Invalid(why)))
      case other => Left(DeskRefusal.Unavailable(StoreError.Invalid(s"asked, but $other")))
    }
}
