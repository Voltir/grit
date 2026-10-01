package grit.dbos.sql

import scala.util.Using

import grit.core.id.WorkflowId
import grit.core.prompt.{Fragment, FragmentId, Layer, SystemPrompt}
import grit.core.store.{PromptStore, StoreError, Tx}

/** [[PromptStore]] over `grit.prompt_fragments` and `grit.turn_prompts`. Ids cross JDBC as
  * JSON arrays, so no Java array does.
  */
final class SqlPromptStore extends PromptStore {
  import SqlEntryStore.attempt

  def keep(fragments: Vector[Fragment])(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.prompt_fragments (id, layer, source, text) VALUES (?, ?, ?, ?)
            |ON CONFLICT (id) DO NOTHING""".stripMargin
        )
      ) { ps =>
        fragments.foreach { f =>
          ps.setString(1, FragmentId.value(f.id))
          ps.setString(2, f.layer.key)
          ps.setString(3, f.source)
          ps.setString(4, f.text)
          ps.executeUpdate()
        }
      }
    }
  }

  def record(workflow: WorkflowId, prompt: SystemPrompt)(using tx: Tx^): Either[StoreError, Unit] =
    keep(prompt.fragments).flatMap { _ =>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      attempt {
        Using.resource(
          conn.prepareStatement(
            """INSERT INTO grit.turn_prompts (workflow_id, fragments) VALUES (?, ?::jsonb)
              |ON CONFLICT (workflow_id) DO NOTHING""".stripMargin
          )
        ) { ps =>
          ps.setString(1, WorkflowId.value(workflow))
          ps.setString(2, SqlPromptStore.idsJson(prompt.ids))
          ps.executeUpdate()
          ()
        }
      }
    }

  def of(workflow: WorkflowId)(using tx: Tx^): Either[StoreError, Option[SystemPrompt]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement("SELECT fragments::text FROM grit.turn_prompts WHERE workflow_id = ?")
      ) { ps =>
        ps.setString(1, WorkflowId.value(workflow))
        Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(rs.getString(1)))
      }
    }.flatMap {
      case None => Right(None)
      case Some(text) =>
        SqlPromptStore.readIds(text).flatMap(prompt).map(Some(_))
    }
  }

  def prompt(ids: Vector[FragmentId])(using tx: Tx^): Either[StoreError, SystemPrompt] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT id, layer, source, text FROM grit.prompt_fragments
            | WHERE id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
        )
      ) { ps =>
        ps.setString(1, SqlPromptStore.idsJson(ids))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String, String, String)]
          while (rs.next())
            rows += ((rs.getString(1), rs.getString(2), rs.getString(3), rs.getString(4)))
          rows.result()
        }
      }
    }.flatMap { rows =>
      rows
        .foldLeft[Either[StoreError, Vector[Fragment]]](Right(Vector.empty)) {
          case (acc, (id, layer, source, text)) =>
            acc.flatMap(done =>
              Layer
                .of(layer)
                .map(l => done :+ Fragment(l, source, text))
                .toRight(StoreError.DatabaseError(s"prompt fragment $id has no layer $layer"))
            )
        }
        .flatMap { found =>
          // In the ids' order, which a prompt's within-layer order is; the rows come unordered.
          ids
            .foldLeft[Either[StoreError, Vector[Fragment]]](Right(Vector.empty)) { (acc, id) =>
              acc.flatMap(done =>
                found
                  .find(_.id == id)
                  .map(done :+ _)
                  .toRight(
                    StoreError.Invalid(s"prompt fragment ${FragmentId.value(id)} is not kept")
                  )
              )
            }
            .map(SystemPrompt.of)
        }
    }
  }

  def forget(workflows: Vector[WorkflowId])(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """DELETE FROM grit.turn_prompts
            | WHERE workflow_id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ujson.Arr.from(workflows.map(w => ujson.Str(WorkflowId.value(w)))).render())
        ps.executeUpdate()
        ()
      }
    }
  }
}

private[sql] object SqlPromptStore {

  def idsJson(ids: Vector[FragmentId]): String =
    ujson.Arr.from(ids.map(id => ujson.Str(FragmentId.value(id)))).render()

  def readIds(text: String): Either[StoreError, Vector[FragmentId]] =
    ujson
      .read(text)
      .arrOpt
      .toRight("not an array")
      .flatMap(_.toVector.foldLeft[Either[String, Vector[FragmentId]]](Right(Vector.empty)) {
        (acc, v) =>
          acc.flatMap(done =>
            v.strOpt.toRight("an id is not a string").flatMap(FragmentId.of).map(done :+ _)
          )
      })
      .left
      .map(why => StoreError.DatabaseError(s"a turn's prompt is unreadable: $why"))
}
