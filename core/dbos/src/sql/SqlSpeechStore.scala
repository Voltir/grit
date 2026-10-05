package grit.dbos.sql

import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.util.Using

import grit.core.id.{ConversationId, EntrySeq, PrincipalId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Cost
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.speech.{Decision, Heard, Outcome, Reach, SpeechJson, SpeechStore, Spoken, Stage}
import grit.core.spend.{Day, Spend}
import grit.core.store.{StoreError, Tx}

/** [[SpeechStore]] over `grit.heard`, whose rows cascade from `grit.entries`, and
  * `grit.speech`, the ledger of decisions.
  */
final class SqlSpeechStore extends SpeechStore {
  import SqlEntryStore.attempt

  def heard(turn: TurnRef, reach: Reach)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // From the turn's first entry, so only a heard message's turn inserts a row.
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.heard (entry_id, conversation_id, turn_seq, reply_to, asked)
            |SELECT id, conversation_id, turn_seq, ?, ?::jsonb FROM (
            |  SELECT id, conversation_id, turn_seq, payload FROM grit.entries
            |   WHERE conversation_id = ?::uuid AND turn_seq = ? ORDER BY seq LIMIT 1) first
            | WHERE payload ->> 'kind' = 'heard'
            |ON CONFLICT DO NOTHING""".stripMargin
        )
      ) { ps =>
        reach.replyTo match {
          case Some(to) => ps.setString(1, to)
          case None => ps.setNull(1, java.sql.Types.VARCHAR)
        }
        // As JSON, so no Java array crosses JDBC (separation checking).
        ps.setString(
          2,
          ujson.Arr.from(reach.asked.toVector.map(p => ujson.Str(PrincipalId.value(p)))).render()
        )
        ps.setString(3, ConversationId.value(turn.conversationId))
        ps.setLong(4, TurnSeq.value(turn.turnSeq))
        ps.executeUpdate()
      }
    }.flatMap { _ =>
      this.reach(turn).flatMap {
        case Some(_) => Right(())
        case None =>
          Left(
            StoreError
              .Invalid(s"${WorkflowId.value(turn.workflowId)} is not a heard message's turn")
          )
      }
    }
  }

  def reach(turn: TurnRef)(using tx: Tx^): Either[StoreError, Option[Reach]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT reply_to, asked::text FROM grit.heard
            | WHERE conversation_id = ?::uuid AND turn_seq = ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(turn.conversationId))
        ps.setLong(2, TurnSeq.value(turn.turnSeq))
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) None
          else {
            val asked = ujson
              .read(rs.getString(2))
              .arrOpt
              .fold(Set.empty[PrincipalId])(_.flatMap(_.strOpt).map(PrincipalId(_)).toSet)
            Some(Reach(Option(rs.getString(1)), asked))
          }
        }
      }
    }
  }

  def spoken(since: Instant)(using tx: Tx^): Either[StoreError, Vector[Spoken]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT workflow, room, decided_at, outcome_kind, posted_seq FROM grit.speech
            | WHERE drafting AND NOT answering AND decided_at >= ?
            | ORDER BY decided_at, workflow""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, since.atOffset(ZoneOffset.UTC))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Either[StoreError, Spoken]]
          while (rs.next()) {
            val workflow = rs.getString(1)
            val posted = rs.getLong(5)
            val stage =
              if (!rs.wasNull()) Stage.Posted(EntrySeq(posted))
              else if (Option(rs.getString(4)).isEmpty) Stage.Drafting
              else Stage.Settled
            rows += (for {
              turn <- TurnRef
                .fromWorkflowId(WorkflowId(workflow))
                .toRight(StoreError.Invalid(s"speech: $workflow is not a turn"))
              room <- Place.read(rs.getString(2)).left.map(StoreError.Invalid(_))
            } yield Spoken(turn, room, rs.getObject(3, classOf[OffsetDateTime]).toInstant, stage))
          }
          rows.result()
        }
      }
    }.flatMap(_.foldLeft[Either[StoreError, Vector[Spoken]]](Right(Vector.empty)) { (acc, r) =>
      acc.flatMap(done => r.map(done :+ _))
    })
  }

  def spentOn(day: Day)(using tx: Tx^): Either[StoreError, Spend] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT count(*), count(u.cost_usd), coalesce(sum(u.cost_usd), 0)
            |  FROM grit.usage_ledger u JOIN grit.speech s ON s.workflow = u.workflow_id
            | WHERE s.drafting AND NOT s.answering AND u.created_at >= ? AND u.created_at < ?""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, day.from.atOffset(ZoneOffset.UTC))
        ps.setObject(2, day.until.atOffset(ZoneOffset.UTC))
        Using.resource(ps.executeQuery()) { rs =>
          val _ = rs.next()
          val (calls, priced, usd) = (rs.getInt(1), rs.getInt(2), BigDecimal(rs.getBigDecimal(3)))
          Spend(calls, if (priced == calls) Cost.Exact(usd) else Cost.AtLeast(usd))
        }
      }
    }
  }

  def decided(heard: Heard, decision: Decision, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.speech (workflow, conversation_id, turn_seq, room, decided_at,
            |  drafting, silence, answering)
            |VALUES (?, ?::uuid, ?, ?, ?, ?, ?::jsonb, ?)
            |ON CONFLICT (workflow) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, WorkflowId.value(heard.turn.workflowId))
        ps.setString(2, ConversationId.value(heard.turn.conversationId))
        ps.setLong(3, TurnSeq.value(heard.turn.turnSeq))
        ps.setString(4, heard.room.written)
        ps.setObject(5, at.atOffset(ZoneOffset.UTC))
        decision match {
          case Decision.Drafting(_) =>
            ps.setBoolean(6, true)
            ps.setNull(7, java.sql.Types.VARCHAR)
            ps.setBoolean(8, false)
          case Decision.Answering(_, _) =>
            ps.setBoolean(6, true)
            ps.setNull(7, java.sql.Types.VARCHAR)
            ps.setBoolean(8, true)
          case Decision.Held(why) =>
            ps.setBoolean(6, false)
            ps.setString(7, SpeechJson.writeSilence(why).render())
            ps.setBoolean(8, false)
        }
        ps.executeUpdate() == 1
      }
    }
  }

  def answering(turn: TurnRef)(using tx: Tx^): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement("SELECT 1 FROM grit.speech WHERE workflow = ? AND answering")
      ) { ps =>
        ps.setString(1, WorkflowId.value(turn.workflowId))
        Using.resource(ps.executeQuery())(_.next())
      }
    }
  }

  def drafted(turn: TurnRef, outcome: Outcome, draft: Option[String], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val workflow = WorkflowId.value(turn.workflowId)
    val judged = SpeechJson.judgedOf(outcome)
    val posted = outcome match {
      case Outcome.Posted(_) => true
      case _ => false
    }
    attempt {
      Using.resource(
        conn.prepareStatement(
          """UPDATE grit.speech SET outcome = ?::jsonb, outcome_kind = ?, grounded = ?, worth = ?,
            |       post_at = ?, judge_model = ?, excerpt = ?, judged_at = ?,
            |       posted_seq = CASE WHEN ? THEN (SELECT seq FROM grit.entries WHERE id = ?) END
            | WHERE workflow = ? AND drafting AND outcome IS NULL
            |   AND (NOT ? OR EXISTS (SELECT 1 FROM grit.entries WHERE id = ?))""".stripMargin
        )
      ) { ps =>
        ps.setString(1, SpeechJson.writeOutcome(outcome).render())
        ps.setString(2, SpeechJson.outcomeName(outcome))
        def probability(i: Int, p: Option[Probability]): Unit = p match {
          case Some(x) => ps.setDouble(i, Probability.value(x))
          case None => ps.setNull(i, java.sql.Types.DOUBLE)
        }
        probability(3, judged.map(_.grounded))
        probability(4, judged.map(_.worth))
        probability(
          5,
          outcome match {
            case Outcome.Below(_, postAt) => Some(postAt)
            case _ => None
          }
        )
        judged match {
          case Some(j) => ps.setString(6, j.model)
          case None => ps.setNull(6, java.sql.Types.VARCHAR)
        }
        draft match {
          case Some(d) => ps.setString(7, d.take(SpeechStore.Excerpt))
          case None => ps.setNull(7, java.sql.Types.VARCHAR)
        }
        ps.setObject(8, at.atOffset(ZoneOffset.UTC))
        ps.setBoolean(9, posted)
        ps.setString(10, grit.core.id.EntryId.value(turn.replyId))
        ps.setString(11, workflow)
        ps.setBoolean(12, posted)
        ps.setString(13, grit.core.id.EntryId.value(turn.replyId))
        ps.executeUpdate() == 1
      }
    }.flatMap { updated =>
      if (updated) Right(true)
      else
        // Nothing updated: kept already (false), or refused.
        state(turn).flatMap {
          case Some((true, true)) => Right(false)
          case Some((true, false)) => Left(StoreError.Invalid(s"$workflow is posted with no reply"))
          case _ => Left(StoreError.Invalid(s"$workflow was never decided on"))
        }
    }
  }

  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """DELETE FROM grit.speech
            | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        ps.setLong(2, TurnSeq.value(from))
        ps.setLong(3, TurnSeq.value(to))
        ps.executeUpdate()
        ()
      }
    }
  }

  /** Whether `turn` was decided on drafting, and whether it has an outcome; `None` when it
    * was never decided on, or was held.
    */
  private def state(
      turn: TurnRef
  )(using tx: Tx^): Either[StoreError, Option[(Boolean, Boolean)]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "SELECT drafting, outcome IS NOT NULL FROM grit.speech WHERE workflow = ? AND drafting"
        )
      ) { ps =>
        ps.setString(1, WorkflowId.value(turn.workflowId))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())((rs.getBoolean(1), rs.getBoolean(2)))
        }
      }
    }
  }
}
