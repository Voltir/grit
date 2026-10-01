package grit.dbos.sql

import java.sql.PreparedStatement

import scala.util.Using

import grit.core.edge.{Deliveries, Part, Pending}
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.store.{StoreError, Tx}

/** [[Deliveries]] over `grit.deliveries` (a turn awaited, where it goes, whether delivered)
  * and `grit.delivery_parts` (each part begun, and what it was posted as).
  */
final class SqlDeliveries extends Deliveries {
  import SqlEntryStore.attempt

  def await(turn: TurnRef, to: String)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.deliveries (workflow, conversation_id, turn_seq, address)
        |VALUES (?, ?::uuid, ?, ?) ON CONFLICT (workflow) DO NOTHING""".stripMargin
    ) { ps =>
      ps.setString(1, WorkflowId.value(turn.workflowId))
      ps.setString(2, grit.core.id.ConversationId.value(turn.conversationId))
      ps.setLong(3, grit.core.id.TurnSeq.value(turn.turnSeq))
      ps.setString(4, to)
    }.map(_ => ())

  def pending()(using tx: Tx^): Either[StoreError, Vector[Pending]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT d.workflow, d.address, p.part, p.posted_as
            |  FROM grit.deliveries d LEFT JOIN grit.delivery_parts p ON p.workflow = d.workflow
            | WHERE NOT d.delivered
            | ORDER BY d.awaited, p.part""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String, Option[(Int, Part)])]
          while (rs.next()) {
            val part = Option(rs.getObject(3)).map { _ =>
              rs.getInt(3) -> Option(rs.getString(4)).fold(Part.Posting)(Part.Posted(_))
            }
            rows += ((rs.getString(1), rs.getString(2), part))
          }
          rows.result()
        }
      }
    }.flatMap { rows =>
      val order = rows.map(_._1).distinct
      val byWorkflow = rows.groupBy(_._1)
      order.foldLeft[Either[StoreError, Vector[Pending]]](Right(Vector.empty)) { (acc, w) =>
        acc.flatMap { done =>
          TurnRef.fromWorkflowId(WorkflowId(w)) match {
            case None => Left(StoreError.DatabaseError(s"delivery of $w: not a turn's workflow"))
            case Some(turn) =>
              val mine = byWorkflow.getOrElse(w, Vector.empty)
              val to = mine.headOption.fold("")(_._2)
              Right(done :+ Pending(turn, to, mine.flatMap(_._3).toMap))
          }
        }
      }
    }
  }

  def posting(turn: TurnRef, part: Int)(using tx: Tx^): Either[StoreError, Unit] =
    awaited(turn).flatMap { _ =>
      update(
        """INSERT INTO grit.delivery_parts (workflow, part) VALUES (?, ?)
          |ON CONFLICT (workflow, part) DO UPDATE SET posted_as = NULL""".stripMargin
      ) { ps =>
        ps.setString(1, WorkflowId.value(turn.workflowId))
        ps.setInt(2, part)
      }.map(_ => ())
    }

  def posted(turn: TurnRef, part: Int, id: String)(using tx: Tx^): Either[StoreError, Unit] =
    awaited(turn).flatMap { _ =>
      update(
        """INSERT INTO grit.delivery_parts (workflow, part, posted_as) VALUES (?, ?, ?)
          |ON CONFLICT (workflow, part) DO UPDATE SET posted_as = EXCLUDED.posted_as""".stripMargin
      ) { ps =>
        ps.setString(1, WorkflowId.value(turn.workflowId))
        ps.setInt(2, part)
        ps.setString(3, id)
      }.map(_ => ())
    }

  def delivered(turn: TurnRef)(using tx: Tx^): Either[StoreError, Unit] =
    update("UPDATE grit.deliveries SET delivered = true WHERE workflow = ?") { ps =>
      ps.setString(1, WorkflowId.value(turn.workflowId))
    }.flatMap(n => if (n == 0) missing(turn) else Right(()))

  private def missing(turn: TurnRef): Either[StoreError, Unit] =
    Left(StoreError.Invalid(s"no delivery awaited for ${WorkflowId.value(turn.workflowId)}"))

  /** `Right` when `turn` is awaited (delivered or not); otherwise [[missing]]. */
  private def awaited(turn: TurnRef)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement("SELECT 1 FROM grit.deliveries WHERE workflow = ?")) {
        ps =>
          ps.setString(1, WorkflowId.value(turn.workflowId))
          Using.resource(ps.executeQuery())(_.next())
      }
    }.flatMap(found => if (found) Right(()) else missing(turn))
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
