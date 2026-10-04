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
  Judgement,
  Period,
  PeriodState,
  Probability,
  Verdict
}
import grit.core.store.{
  ClosedElsewhere,
  ClosedPeriod,
  ClosingEntry,
  Entry,
  EntryStore,
  OpenPeriod,
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

  def all(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Vector[Period]] =
    many(s"SELECT $columns FROM grit.periods WHERE conversation_id = ?::uuid ORDER BY seq")(
      _.setString(1, ConversationId.value(conversation))
    )(readPeriod)

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

  def judged(period: PeriodRef, verdict: Verdict)(using tx: Tx^): Either[StoreError, Boolean] =
    for {
      _ <- entries.lockNext(period.conversationId)
      open <- activity(period)
      kept <- open match {
        case Some(a) if a.last == verdict.last =>
          update(
            """INSERT INTO grit.verdicts (conversation_id, seq, last_turn, at, nobody,
              |  waiting_person, waiting_other, model, unanswered)
              |VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin
          ) { ps =>
            ps.setString(1, ConversationId.value(period.conversationId))
            ps.setLong(2, PeriodSeq.value(period.seq))
            ps.setLong(3, TurnSeq.value(verdict.last))
            ps.setObject(4, verdict.at.atOffset(ZoneOffset.UTC))
            verdict.judgement match {
              case Judgement.Weighed(nobody, onPerson, onOther, model) =>
                Vector(nobody, onPerson, onOther).zipWithIndex.foreach { (p: Probability, i: Int) =>
                  ps.setDouble(5 + i, Probability.value(p))
                }
                ps.setString(8, model)
                ps.setNull(9, java.sql.Types.VARCHAR)
              case Judgement.Unanswered(why) =>
                (5 to 7).foreach(ps.setNull(_, java.sql.Types.DOUBLE))
                ps.setNull(8, java.sql.Types.VARCHAR)
                ps.setString(9, why)
            }
          }.map(_ => true)
        case _ => Right(false)
      }
    } yield kept

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
      s"""SELECT p.conversation_id, p.seq, a.newest, a.last, n.asked,
         |       v.at, v.last_turn, v.nobody, v.waiting_person, v.waiting_other,
         |       v.model, v.unanswered
         |  FROM grit.periods p
         | CROSS JOIN LATERAL (
         |   SELECT greatest(p.opened_at,
         |                   max(e.created_at) FILTER (WHERE e.payload ->> 'kind' <> 'draft')) AS newest,
         |          coalesce(max(e.turn_seq), p.first_turn) AS last
         |     FROM grit.entries e
         |    WHERE e.conversation_id = p.conversation_id AND e.turn_seq >= p.first_turn) a
         | CROSS JOIN LATERAL (
         |   SELECT count(*) AS asked FROM grit.verdicts
         |    WHERE conversation_id = p.conversation_id AND seq = p.seq) n
         |  LEFT JOIN LATERAL (
         |   SELECT * FROM grit.verdicts
         |    WHERE conversation_id = p.conversation_id AND seq = p.seq
         |    ORDER BY at DESC, ordinal DESC LIMIT 1) v ON true
         | WHERE p.closed_at IS NULL AND $where""".stripMargin
    )(bind) { rs =>
      Activity(
        ref(rs),
        instant(rs, "newest"),
        TurnSeq(rs.getLong("last")),
        verdictOf(rs),
        rs.getInt("asked")
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
            // its own, so a cursor never passes an ordinal whose seal is still to commit. The
            // ordinal comes from a sequence, never the greatest kept: a dropped period's, or a
            // removed conversation's, is never taken again, so no cursor skips a new close.
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
                |   SET last_turn = ?, closed_at = ?, reason = ?, confidence = ?, closing_id = ?,
                |       close_ordinal = nextval('grit.close_ordinals')
                | WHERE conversation_id = ?::uuid AND seq = ?""".stripMargin
            ) { ps =>
              ps.setLong(1, TurnSeq.value(attempt.last))
              ps.setObject(2, at.atOffset(ZoneOffset.UTC))
              ps.setString(3, PayloadJson.reasonName(reason))
              PayloadJson.reasonConfidence(reason) match {
                case Some(c) => ps.setDouble(4, c)
                case None => ps.setNull(4, java.sql.Types.DOUBLE)
              }
              ps.setString(5, EntryId.value(period.closingId))
              ps.setString(6, ConversationId.value(period.conversationId))
              ps.setLong(7, PeriodSeq.value(period.seq))
            }
          } yield Sealed.Closed(period.closingId)
    } yield outcome
  }

  def closingBefore(turn: TurnRef)(using tx: Tx^): Either[StoreError, Option[ClosingEntry]] =
    for {
      id <- one(
        """SELECT closing_id FROM grit.periods
          | WHERE conversation_id = ?::uuid AND closed_at IS NOT NULL AND last_turn < ?
          | ORDER BY seq DESC LIMIT 1""".stripMargin
      ) { ps =>
        ps.setString(1, ConversationId.value(turn.conversationId))
        ps.setLong(2, TurnSeq.value(turn.turnSeq))
      }(rs => EntryId(rs.getString("closing_id")))
      entry <- id.fold[Either[StoreError, Option[Entry]]](Right(None))(entries.get)
      closing <- entry match {
        case None => Right(None)
        case Some(e) =>
          ClosingEntry
            .of(e)
            .map(Some(_))
            .toRight(StoreError.Invalid(s"${EntryId.value(e.id)} is not a closing entry"))
      }
    } yield closing

  def openElsewhere(conversation: ConversationId)(using
      tx: Tx^
  ): Either[StoreError, Vector[OpenPeriod]] =
    many(
      """SELECT p.conversation_id, p.first_turn, c.origin
        |  FROM grit.periods p
        |  JOIN grit.conversations c ON c.id = p.conversation_id
        | WHERE p.closed_at IS NULL AND p.conversation_id <> ?::uuid
        | ORDER BY p.opened_at, p.conversation_id""".stripMargin
    )(_.setString(1, ConversationId.value(conversation))) { rs =>
      // The place is the origin's (Origin.place), the one definition grit.places is built from.
      OpenPeriod(
        ConversationId(rs.getString("conversation_id")),
        SqlConversationStore.readOrigin(ujson.read(rs.getString("origin"))) match {
          case Right(o) => o.place
          case Left(why) => throw new IllegalStateException(s"unreadable origin: $why")
        },
        TurnSeq(rs.getLong("first_turn"))
      )
    }

  def closedElsewhere(conversation: ConversationId)(using
      tx: Tx^
  ): Either[StoreError, Vector[ClosedElsewhere]] =
    many(
      """SELECT conversation_id, closing_id, origin FROM (
        |  SELECT DISTINCT ON (p.conversation_id) p.conversation_id, p.closing_id,
        |         p.close_ordinal, c.origin
        |    FROM grit.periods p
        |    JOIN grit.conversations c ON c.id = p.conversation_id
        |   WHERE p.closed_at IS NOT NULL AND p.reason <> 'unearned'
        |     AND p.conversation_id <> ?::uuid
        |   ORDER BY p.conversation_id, p.seq DESC
        |) newest
        |ORDER BY close_ordinal""".stripMargin
    )(_.setString(1, ConversationId.value(conversation))) { rs =>
      // The place is the origin's (Origin.place), as in openElsewhere.
      ClosedElsewhere(
        ConversationId(rs.getString("conversation_id")),
        SqlConversationStore.readOrigin(ujson.read(rs.getString("origin"))) match {
          case Right(o) => o.place
          case Left(why) => throw new IllegalStateException(s"unreadable origin: $why")
        },
        EntryId(rs.getString("closing_id"))
      )
    }

  def closedAfter(after: CloseOrdinal, n: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[ClosedPeriod]] =
    many(
      """SELECT p.conversation_id, p.seq, p.reason, p.confidence, p.closed_at, p.close_ordinal, c.origin, e.payload
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

  def drop(period: PeriodRef)(using tx: Tx^): Either[StoreError, Boolean] =
    for {
      found <- get(period)
      dropped <- found.map(_.state) match {
        case Some(PeriodState.Closed(_, _, _, closing, _, Some(_))) =>
          for {
            _ <- update("DELETE FROM grit.periods WHERE conversation_id = ?::uuid AND seq = ?") {
              ps =>
                ps.setString(1, ConversationId.value(period.conversationId))
                ps.setLong(2, PeriodSeq.value(period.seq))
            }
            _ <- update("DELETE FROM grit.entries WHERE id = ?")(
              _.setString(1, EntryId.value(closing))
            )
          } yield true
        case _ => Right(false)
      }
    } yield dropped

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
            // Its turns' tool requests hold their arguments and outcomes: raw, like the
            // entries (Target.Raw).
            _ <- update(
              """DELETE FROM grit.tool_requests
                | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?""".stripMargin
            ) { ps =>
              ps.setString(1, ConversationId.value(period.conversationId))
              ps.setLong(2, TurnSeq.value(p.first))
              ps.setLong(3, TurnSeq.value(last))
            }
            // Its turns' deliveries: which replies an edge posted, and as what (Target.Raw).
            _ <- update(
              """DELETE FROM grit.deliveries
                | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?""".stripMargin
            ) { ps =>
              ps.setString(1, ConversationId.value(period.conversationId))
              ps.setLong(2, TurnSeq.value(p.first))
              ps.setLong(3, TurnSeq.value(last))
            }
            // Its turns' acknowledgements: which messages an edge marked (Target.Raw).
            _ <- update(
              """DELETE FROM grit.acknowledgements
                | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?""".stripMargin
            ) { ps =>
              ps.setString(1, ConversationId.value(period.conversationId))
              ps.setLong(2, TurnSeq.value(p.first))
              ps.setLong(3, TurnSeq.value(last))
            }
            _ <- update(
              "DELETE FROM grit.verdicts WHERE conversation_id = ?::uuid AND seq = ?"
            ) { ps =>
              ps.setString(1, ConversationId.value(period.conversationId))
              ps.setLong(2, PeriodSeq.value(period.seq))
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
    "conversation_id, seq, first_turn, opened_at, last_turn, closed_at, reason, confidence, " +
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
    PayloadJson.readReason(rs.getString("reason"), optDouble(rs, "confidence")) match {
      case Right(r) => r
      case Left(why) => throw new IllegalStateException(why)
    }

  private def optDouble(rs: ResultSet, column: String): Option[Double] = {
    val d = rs.getDouble(column)
    Option.when(!rs.wasNull())(d)
  }

  private def probability(rs: ResultSet, column: String): Option[Probability] =
    optDouble(rs, column).flatMap(Probability.of)

  /** The verdict a row of [[activities]] holds, if any; the table's CHECK keeps a weighed
    * one's columns set together.
    */
  private def verdictOf(rs: ResultSet): Option[Verdict] =
    optInstant(rs, "at").map { at =>
      val judgement =
        (
          probability(rs, "nobody"),
          probability(rs, "waiting_person"),
          probability(rs, "waiting_other"),
          Option(rs.getString("model"))
        ) match {
          case (Some(n), Some(p), Some(o), Some(model)) =>
            Judgement.Weighed(n, p, o, model)
          case _ => Judgement.Unanswered(Option(rs.getString("unanswered")).getOrElse(""))
        }
      Verdict(at, TurnSeq(rs.getLong("last_turn")), judgement)
    }

  private def ordinalOf(rs: ResultSet): CloseOrdinal =
    CloseOrdinal
      .of(rs.getLong("close_ordinal"))
      .getOrElse(throw new IllegalStateException("ordinal"))

  /** A period row; the table's CHECK keeps a closed one's four columns set together. */
  private def readPeriod(rs: ResultSet): Period = {
    val state = optInstant(rs, "closed_at") match {
      case None => PeriodState.Open
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
