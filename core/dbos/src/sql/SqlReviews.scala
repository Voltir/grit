package grit.dbos.sql

import java.sql.ResultSet
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.collection.immutable.VectorMap
import scala.util.Using

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, PrincipalId, QuestionName, ShadowName}
import grit.core.review.{
  Candidate,
  Considered,
  Label,
  Prompt,
  Reason,
  ReviewStore,
  Reviewed,
  Settled,
  Verdict
}
import grit.core.speech.SpeechJson
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{ShadowAnswers, ShadowedJson}

/** [[ReviewStore]] over `grit.reviews`, a ledger whose rows cascade from `grit.conversations`
  * alone, reading candidates and prompts from `grit.entries`, `grit.speech` and
  * `grit.triage_shadows`.
  */
final class SqlReviews extends ReviewStore {
  import SqlEntryStore.attempt
  import SqlReviews.*

  def unposted()(using tx: Tx^): Either[StoreError, Vector[Prompt]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT r.entry_id, c.origin::text AS origin, r.shadow, r.reason, s.drafting,
            |       s.silence::text AS silence, s.outcome::text AS outcome,
            |       t.answers::text AS answers
            |  FROM grit.reviews r
            |  JOIN grit.conversations c ON c.id = r.conversation_id
            |  JOIN grit.entries e ON e.id = r.entry_id
            |  JOIN grit.speech s ON s.conversation_id = e.conversation_id
            |                    AND s.turn_seq = e.turn_seq
            |  JOIN grit.triage_shadows t ON t.entry_id = r.entry_id AND t.name = r.shadow
            | WHERE r.picked_at IS NOT NULL AND r.address IS NULL AND t.failure IS NULL
            | ORDER BY r.picked_at, r.entry_id""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Either[String, Prompt]]
          while (rs.next())
            rows += (for {
              origin <- SqlConversationStore.readOrigin(ujson.read(rs.getString("origin")))
              shadow <- ShadowName.of(rs.getString("shadow"))
              reason <- Option(rs.getString("reason"))
                .toRight("a picked review has no reason")
                .flatMap(reasonRead)
              live <- settled(rs)
              answers <- named(rs.getString("answers"))
            } yield Prompt(
              EntryId(rs.getString("entry_id")),
              origin,
              shadow,
              reason,
              live,
              answers
            ))
          rows.result()
        }
      }
    }.flatMap(collected)
  }

  def posted(entry: EntryId, address: String, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """UPDATE grit.reviews SET address = ?, posted_at = ?
            | WHERE entry_id = ? AND picked_at IS NOT NULL AND address IS NULL
            |   AND NOT EXISTS (SELECT 1 FROM grit.reviews WHERE address = ?)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, address)
        ps.setObject(2, at.atOffset(ZoneOffset.UTC))
        ps.setString(3, EntryId.value(entry))
        ps.setString(4, address)
        ps.executeUpdate() == 1
      }
    }
  }

  def reacted(address: String, rater: PrincipalId, verdict: Verdict, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "UPDATE grit.reviews SET verdict = ?, rater = ?, labelled_at = ? WHERE address = ?"
        )
      ) { ps =>
        ps.setString(1, verdictWritten(verdict))
        ps.setString(2, PrincipalId.value(rater))
        ps.setObject(3, at.atOffset(ZoneOffset.UTC))
        ps.setString(4, address)
        ps.executeUpdate() == 1
      }
    }
  }

  def unreacted(address: String, rater: PrincipalId, verdict: Verdict)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """UPDATE grit.reviews SET verdict = NULL, rater = NULL, labelled_at = NULL
            | WHERE address = ? AND verdict = ? AND rater = ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, address)
        ps.setString(2, verdictWritten(verdict))
        ps.setString(3, PrincipalId.value(rater))
        ps.executeUpdate() == 1
      }
    }
  }

  def candidates(shadow: ShadowName, since: Instant, limit: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[Candidate]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // A question set's answers each carry their question's name (ShadowedJson); a
      // wording's carry none.
      Using.resource(
        conn.prepareStatement(
          """SELECT e.id, e.conversation_id, e.created_at, s.drafting,
            |       s.silence::text AS silence, s.outcome::text AS outcome,
            |       t.answers::text AS answers
            |  FROM grit.triage_shadows t
            |  JOIN grit.entries e ON e.id = t.entry_id
            |  JOIN grit.speech s ON s.conversation_id = e.conversation_id
            |                    AND s.turn_seq = e.turn_seq
            | WHERE t.name = ? AND t.failure IS NULL AND (t.answers -> 0 ->> 'name') IS NOT NULL
            |   AND e.created_at >= ?
            |   AND (NOT s.drafting OR s.outcome IS NOT NULL)
            |   AND NOT EXISTS (SELECT 1 FROM grit.reviews r WHERE r.entry_id = e.id)
            | ORDER BY e.created_at, e.id
            | LIMIT ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ShadowName.value(shadow))
        ps.setObject(2, since.atOffset(ZoneOffset.UTC))
        ps.setInt(3, limit)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Either[String, Candidate]]
          while (rs.next())
            rows += (for {
              live <- settled(rs)
              answers <- named(rs.getString("answers"))
            } yield Candidate(
              EntryId(rs.getString("id")),
              ConversationId(rs.getString("conversation_id")),
              instant(rs, "created_at"),
              live,
              shadow,
              answers
            ))
          rows.result()
        }
      }
    }.flatMap(collected)
  }

  def considered(candidate: Candidate, as: Considered, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.reviews (entry_id, conversation_id, shadow, reason, considered_at,
            |  picked_at)
            |VALUES (?, ?::uuid, ?, ?, ?, ?)
            |ON CONFLICT (entry_id) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, EntryId.value(candidate.entry))
        ps.setString(2, ConversationId.value(candidate.conversation))
        ps.setString(3, ShadowName.value(candidate.shadow))
        val (reason, picked) = as match {
          case Considered.Picked(r) => (Some(r), true)
          case Considered.Passed(r) => (Some(r), false)
          case Considered.Unread => (None, false)
        }
        reason match {
          case Some(r) => ps.setString(4, reasonWritten(r))
          case None => ps.setNull(4, java.sql.Types.VARCHAR)
        }
        ps.setObject(5, at.atOffset(ZoneOffset.UTC))
        if (picked) ps.setObject(6, at.atOffset(ZoneOffset.UTC))
        else ps.setNull(6, java.sql.Types.TIMESTAMP_WITH_TIMEZONE)
        ps.executeUpdate() == 1
      }
    }
  }

  def reviewed(since: Instant)(using tx: Tx^): Either[StoreError, Vector[Reviewed]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT entry_id, conversation_id, shadow, reason, considered_at, picked_at,
            |       verdict, rater, labelled_at
            |  FROM grit.reviews WHERE considered_at >= ?
            | ORDER BY considered_at, entry_id""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, since.atOffset(ZoneOffset.UTC))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Either[String, Reviewed]]
          while (rs.next()) {
            val picked = Option(rs.getObject("picked_at", classOf[OffsetDateTime])).isDefined
            rows += (for {
              shadow <- ShadowName.of(rs.getString("shadow"))
              reason <- Option(rs.getString("reason")) match {
                case Some(r) => reasonRead(r).map(Some(_))
                case None => Right(None)
              }
              as = (reason, picked) match {
                case (Some(r), true) => Considered.Picked(r)
                case (Some(r), false) => Considered.Passed(r)
                // The schema refuses a picked row without a reason.
                case (None, _) => Considered.Unread
              }
              label <- Option(rs.getString("verdict")) match {
                case Some(v) =>
                  verdictRead(v).map(verdict =>
                    Some(
                      Label(
                        verdict,
                        PrincipalId(rs.getString("rater")),
                        instant(rs, "labelled_at")
                      )
                    )
                  )
                case None => Right(None)
              }
            } yield Reviewed(
              EntryId(rs.getString("entry_id")),
              ConversationId(rs.getString("conversation_id")),
              shadow,
              instant(rs, "considered_at"),
              as,
              label
            ))
          }
          rows.result()
        }
      }
    }.flatMap(collected)
  }
}

private[sql] object SqlReviews {

  /** A reason as `grit.reviews` keeps it. */
  def reasonWritten(r: Reason): String = r match {
    case Reason.ShadowOnly => "shadow-only"
    case Reason.LiveOnly => "live-only"
    case Reason.Both => "both"
    case Reason.Neither => "neither"
  }

  def reasonRead(s: String): Either[String, Reason] =
    Reason.values.find(reasonWritten(_) == s).toRight(s"unknown reason: $s")

  /** A verdict as `grit.reviews` keeps it. */
  def verdictWritten(v: Verdict): String = v match {
    case Verdict.Welcome => "welcome"
    case Verdict.Interruption => "interruption"
    case Verdict.CutIn => "cut-in"
  }

  def verdictRead(s: String): Either[String, Verdict] =
    Verdict.values.find(verdictWritten(_) == s).toRight(s"unknown verdict: $s")

  def instant(rs: ResultSet, column: String): Instant =
    rs.getObject(column, classOf[OffsetDateTime]).toInstant

  /** Live's settled decision from a row's `drafting`, `silence` and `outcome`. */
  def settled(rs: ResultSet): Either[String, Settled] =
    if (rs.getBoolean("drafting"))
      Option(rs.getString("outcome"))
        .toRight("a settled draft has no outcome")
        .flatMap(o => SpeechJson.readOutcome(ujson.read(o)))
        .map(Settled.Drafted(_))
    else
      Option(rs.getString("silence"))
        .toRight("a hold has no silence")
        .flatMap(s => SpeechJson.readSilence(ujson.read(s)))
        .map(Settled.Held(_))

  /** A shadow row's answers, as a question set's. */
  def named(answers: String): Either[String, VectorMap[QuestionName, Answer]] =
    ShadowedJson.readAnswers(ujson.read(answers)).flatMap {
      case ShadowAnswers.Named(a) => Right(a)
      case ShadowAnswers.Worded(_) => Left("a wording's answers, not a question set's")
    }

  /** Every row read, or the first that did not read, as grit's own bug. */
  def collected[A](rows: Vector[Either[String, A]]): Either[StoreError, Vector[A]] =
    rows.foldLeft[Either[StoreError, Vector[A]]](Right(Vector.empty)) { (acc, r) =>
      acc.flatMap(done => r.map(done :+ _).left.map(why => StoreError.Invalid(s"reviews: $why")))
    }
}
