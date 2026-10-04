package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.edge.{Acknowledgement, Acknowledgements}
import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{StoreError, Tx}

/** [[Acknowledgements]] over `grit.acknowledgements`: a turn's mark, where it goes, its stage
  * (`wanted`, `shown`, `cleared`) and when each was reached.
  */
final class SqlAcknowledgements extends Acknowledgements {
  import SqlEntryStore.attempt

  def want(turn: TurnRef, to: String, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.acknowledgements
        |  (workflow, conversation_id, turn_seq, address, stage, wanted_at)
        |VALUES (?, ?::uuid, ?, ?, 'wanted', ?) ON CONFLICT (workflow) DO NOTHING""".stripMargin
    ) { ps =>
      ps.setString(1, WorkflowId.value(turn.workflowId))
      ps.setString(2, ConversationId.value(turn.conversationId))
      ps.setLong(3, TurnSeq.value(turn.turnSeq))
      ps.setString(4, to)
      ps.setObject(5, at.atOffset(ZoneOffset.UTC))
    }

  def standing()(using tx: Tx^): Either[StoreError, Vector[Acknowledgement]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT workflow, address, stage = 'shown' FROM grit.acknowledgements
            | WHERE stage <> 'cleared' ORDER BY wanted""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String, Boolean)]
          while (rs.next()) rows += ((rs.getString(1), rs.getString(2), rs.getBoolean(3)))
          rows.result()
        }
      }
    }.flatMap { rows =>
      rows.foldLeft[Either[StoreError, Vector[Acknowledgement]]](Right(Vector.empty)) {
        case (acc, (w, to, shown)) =>
          acc.flatMap(done =>
            TurnRef
              .fromWorkflowId(WorkflowId(w))
              .toRight(StoreError.DatabaseError(s"acknowledgement of $w: not a turn's workflow"))
              .map(turn => done :+ Acknowledgement(turn, to, shown))
          )
      }
    }
  }

  def shown(turn: TurnRef, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """UPDATE grit.acknowledgements SET stage = 'shown', shown_at = ?
        | WHERE workflow = ? AND stage <> 'cleared'""".stripMargin
    ) { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setString(2, WorkflowId.value(turn.workflowId))
    }

  def cleared(turn: TurnRef, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """UPDATE grit.acknowledgements SET stage = 'cleared', cleared_at = ?
        | WHERE workflow = ? AND stage <> 'cleared'""".stripMargin
    ) { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setString(2, WorkflowId.value(turn.workflowId))
    }

  private def update(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        val _ = ps.executeUpdate()
      }
    }
  }
}
