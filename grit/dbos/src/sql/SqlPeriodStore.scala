package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.util.Using

import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq}
import grit.core.period.{
  Activity,
  CloseOrdinal,
  CloseReason,
  Closing,
  Period,
  PeriodState,
  Purgeable
}
import grit.core.store.{
  ClosedPeriod,
  Entry,
  EntryStore,
  Payload,
  PayloadJson,
  PeriodStore,
  Sealed,
  StoreError,
  Tx
}

/** [[PeriodStore]] over the `grit.periods` table and `entries`' own. */
final class SqlPeriodStore(entries: EntryStore) extends PeriodStore {
  import SqlEntryStore.attempt
  import SqlPeriodStore.*

  def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Period] =
    for {
      _ <- entries.lockNext(conversation)
      open <- one(
        s"SELECT $columns FROM grit.periods WHERE conversation_id = ?::uuid AND closed_at IS NULL"
      ) {
        _.setString(1, ConversationId.value(conversation))
      }(readPeriod)
      period <- open match {
        case Some(p) => Right(p)
        case None =>
          one(
            s"""INSERT INTO grit.periods (conversation_id, seq, first_turn, opened_at)
               |SELECT ?::uuid, coalesce(max(seq), 0) + 1, ?, ?
               |  FROM grit.periods WHERE conversation_id = ?::uuid
               |RETURNING $columns""".stripMargin
          ) { ps =>
            ps.setString(1, ConversationId.value(conversation))
            ps.setLong(2, TurnSeq.value(turn))
            ps.setObject(3, at.atOffset(ZoneOffset.UTC))
            ps.setString(4, ConversationId.value(conversation))
          }(readPeriod)
            .flatMap(_.toRight(StoreError.DatabaseError("a new period was not returned")))
      }
    } yield period

  def get(period: PeriodRef)(using tx: Tx^): Either[StoreError, Option[Period]] =
    one(s"SELECT $columns FROM grit.periods WHERE conversation_id = ?::uuid AND seq = ?") { ps =>
      ps.setString(1, ConversationId.value(period.conversationId))
      ps.setLong(2, PeriodSeq.value(period.seq))
    }(readPeriod)

  def of(turn: TurnRef)(using tx: Tx^): Either[StoreError, Option[Period]] =
    one(
      s"""SELECT $columns FROM grit.periods
         | WHERE conversation_id = ?::uuid AND first_turn <= ? AND (last_turn IS NULL OR last_turn >= ?)
         | ORDER BY seq DESC LIMIT 1""".stripMargin
    ) { ps =>
      ps.setString(1, ConversationId.value(turn.conversationId))
      ps.setLong(2, TurnSeq.value(turn.turnSeq))
      ps.setLong(3, TurnSeq.value(turn.turnSeq))
    }(readPeriod)

  def signal(conversation: ConversationId, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] =
    for {
      _ <- entries.lockNext(conversation)
      open <- activities("p.conversation_id = ?::uuid")(
        _.setString(1, ConversationId.value(conversation))
      )
      signalled <- open.headOption match {
        case None => Right(false)
        case Some(a) =>
          update(
            "UPDATE grit.periods SET signalled_at = ? WHERE conversation_id = ?::uuid AND seq = ?"
          ) { ps =>
            ps.setObject(1, a.signal(at).atOffset(ZoneOffset.UTC))
            ps.setString(2, ConversationId.value(conversation))
            ps.setLong(3, PeriodSeq.value(a.period.seq))
          }.map(_ => true)
      }
    } yield signalled

  def open()(using tx: Tx^): Either[StoreError, Vector[Activity]] =
    activities("true")(_ => ())

  def activity(period: PeriodRef)(using tx: Tx^): Either[StoreError, Option[Activity]] =
    activities("p.conversation_id = ?::uuid AND p.seq = ?") { ps =>
      ps.setString(1, ConversationId.value(period.conversationId))
      ps.setLong(2, PeriodSeq.value(period.seq))
    }.map(_.headOption)

  /** The open periods `where` picks (over `p`, the periods), as their deadlines see them. */
  private def activities(where: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Vector[Activity]] =
    many(
      s"""SELECT p.conversation_id, p.seq, p.signalled_at,
         |       greatest(p.opened_at, max(e.created_at)) AS newest,
         |       coalesce(max(e.turn_seq), p.first_turn) AS last
         |  FROM grit.periods p
         |  LEFT JOIN grit.entries e
         |    ON e.conversation_id = p.conversation_id AND e.turn_seq >= p.first_turn
         | WHERE p.closed_at IS NULL AND $where
         | GROUP BY p.conversation_id, p.seq""".stripMargin
    )(bind) { rs =>
      Activity(
        ref(rs),
        instant(rs, "newest"),
        TurnSeq(rs.getLong("last")),
        Option(rs.getObject("signalled_at", classOf[OffsetDateTime])).map(_.toInstant)
      )
    }

  def seal(attempt: CloseRef, reason: CloseReason, closing: Closing, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Sealed] = {
    val period = attempt.period
    for {
      next <- entries.lockNext(period.conversationId)
      open <- activity(period)
      outcome <-
        if (open.isEmpty || next.turnSeq != attempt.last.next) Right(Sealed.Abandoned)
        else
          for {
            // Held to commit: a seal that takes an ordinal commits before the next one takes
            // its own, so a cursor never passes an ordinal whose seal is still to commit.
            _ <- one("SELECT pg_advisory_xact_lock(?)")(_.setLong(1, OrdinalLock))(_ => ())
            _ <- entries.insert(
              Entry(
                period.closingId,
                period.conversationId,
                attempt.last,
                None,
                next.seq,
                Payload.Closed(period.seq, reason, closing),
                at
              )
            )
            _ <- update(
              """UPDATE grit.periods
                |   SET last_turn = ?, closed_at = ?, reason = ?, closing_id = ?,
                |       close_ordinal = (SELECT coalesce(max(close_ordinal), 0) + 1 FROM grit.periods)
                | WHERE conversation_id = ?::uuid AND seq = ?""".stripMargin
            ) { ps =>
              ps.setLong(1, TurnSeq.value(attempt.last))
              ps.setObject(2, at.atOffset(ZoneOffset.UTC))
              ps.setString(3, PayloadJson.reasonName(reason))
              ps.setString(4, EntryId.value(period.closingId))
              ps.setString(5, ConversationId.value(period.conversationId))
              ps.setLong(6, PeriodSeq.value(period.seq))
            }
          } yield Sealed.Closed(period.closingId)
    } yield outcome
  }

  def closingsBefore(turn: TurnRef, n: Int)(using tx: Tx^): Either[StoreError, Vector[Entry]] =
    for {
      ids <- many(
        """SELECT closing_id FROM (
          |  SELECT closing_id, seq FROM grit.periods
          |   WHERE conversation_id = ?::uuid AND closed_at IS NOT NULL AND last_turn < ?
          |   ORDER BY seq DESC LIMIT ?
          |) newest ORDER BY seq""".stripMargin
      ) { ps =>
        ps.setString(1, ConversationId.value(turn.conversationId))
        ps.setLong(2, TurnSeq.value(turn.turnSeq))
        ps.setInt(3, n max 0)
      }(rs => EntryId(rs.getString("closing_id")))
      found <- ids.foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty)) { (acc, id) =>
        acc.flatMap(done => entries.get(id).map(done ++ _))
      }
    } yield found

  def closedAfter(after: CloseOrdinal, n: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[ClosedPeriod]] =
    many(
      """SELECT p.conversation_id, p.seq, p.reason, p.closed_at, p.close_ordinal, c.origin, e.payload
        |  FROM grit.periods p
        |  JOIN grit.conversations c ON c.id = p.conversation_id
        |  JOIN grit.entries e ON e.id = p.closing_id
        | WHERE p.close_ordinal > ?
        | ORDER BY p.close_ordinal LIMIT ?""".stripMargin
    ) { ps =>
      ps.setLong(1, CloseOrdinal.value(after))
      ps.setInt(2, n max 0)
    } { rs =>
      val closing = PayloadJson.read(ujson.read(rs.getString("payload"))) match {
        case Right(Payload.Closed(_, _, c)) => c
        // grit's own bug, not a caller's expected failure: `attempt` reports it.
        case other => throw new IllegalStateException(s"not a closing entry: $other")
      }
      ClosedPeriod(
        ref(rs),
        SqlConversationStore.readOrigin(ujson.read(rs.getString("origin"))) match {
          case Right(o) => o
          case Left(why) => throw new IllegalStateException(s"unreadable origin: $why")
        },
        reasonOf(rs),
        closing,
        instant(rs, "closed_at"),
        ordinalOf(rs)
      )
    }

  def expired(cutoff: Instant)(using tx: Tx^): Either[StoreError, Vector[Purgeable]] =
    many(
      """SELECT conversation_id, seq, first_turn, last_turn FROM grit.periods
        | WHERE closed_at < ? AND purged_at IS NULL
        | ORDER BY close_ordinal""".stripMargin
    )(_.setObject(1, cutoff.atOffset(ZoneOffset.UTC))) { rs =>
      Purgeable(ref(rs), TurnSeq(rs.getLong("first_turn")), TurnSeq(rs.getLong("last_turn")))
    }

  def purge(period: PeriodRef, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    for {
      found <- get(period)
      _ <- found.map(p => p -> p.state) match {
        case Some((p, PeriodState.Closed(last, _, _, closing, _, None))) =>
          for {
            _ <- update(
              """DELETE FROM grit.entries
                | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ? AND id <> ?""".stripMargin
            ) { ps =>
              ps.setString(1, ConversationId.value(period.conversationId))
              ps.setLong(2, TurnSeq.value(p.first))
              ps.setLong(3, TurnSeq.value(last))
              ps.setString(4, EntryId.value(closing))
            }
            _ <- update(
              "UPDATE grit.periods SET purged_at = ? WHERE conversation_id = ?::uuid AND seq = ?"
            ) { ps =>
              ps.setObject(1, at.atOffset(ZoneOffset.UTC))
              ps.setString(2, ConversationId.value(period.conversationId))
              ps.setLong(3, PeriodSeq.value(period.seq))
            }
          } yield ()
        case _ => Right(())
      }
    } yield ()

  private def one[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Option[A]] =
    many(sql)(bind)(read).map(_.headOption)

  private def many[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[A]
          while (rs.next()) rows += read(rs)
          rows.result()
        }
      }
    }
  }

  private def update(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        ps.executeUpdate()
      }
    }
  }
}

private object SqlPeriodStore {

  /** Column order shared by every statement that reads a whole period, and `readPeriod`. */
  private val columns =
    "conversation_id, seq, first_turn, opened_at, signalled_at, last_turn, closed_at, reason, " +
      "closing_id, close_ordinal, purged_at"

  /** The advisory lock every seal takes to number itself: grit's alone among the locks a
    * transaction may take, by its value.
    */
  private val OrdinalLock = 0x67726974_636c6f73L

  private def ref(rs: ResultSet): PeriodRef =
    PeriodRef(
      ConversationId(rs.getString("conversation_id")),
      PeriodSeq.of(rs.getLong("seq")).getOrElse(throw new IllegalStateException("seq below 1"))
    )

  private def instant(rs: ResultSet, column: String): Instant =
    rs.getObject(column, classOf[OffsetDateTime]).toInstant

  private def optInstant(rs: ResultSet, column: String): Option[Instant] =
    Option(rs.getObject(column, classOf[OffsetDateTime])).map(_.toInstant)

  private def reasonOf(rs: ResultSet): CloseReason =
    PayloadJson.readReason(rs.getString("reason")) match {
      case Right(r) => r
      case Left(why) => throw new IllegalStateException(why)
    }

  private def ordinalOf(rs: ResultSet): CloseOrdinal =
    CloseOrdinal
      .of(rs.getLong("close_ordinal"))
      .getOrElse(throw new IllegalStateException("ordinal"))

  /** A period row; the table's CHECK keeps a closed one's four columns set together. */
  private def readPeriod(rs: ResultSet): Period = {
    val state = optInstant(rs, "closed_at") match {
      case None => PeriodState.Open(optInstant(rs, "signalled_at"))
      case Some(closedAt) =>
        PeriodState.Closed(
          TurnSeq(rs.getLong("last_turn")),
          closedAt,
          reasonOf(rs),
          EntryId(rs.getString("closing_id")),
          ordinalOf(rs),
          optInstant(rs, "purged_at")
        )
    }
    Period(ref(rs), TurnSeq(rs.getLong("first_turn")), instant(rs, "opened_at"), state)
  }
}
