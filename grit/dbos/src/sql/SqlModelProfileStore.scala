package grit.dbos.sql

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.WorkflowId
import grit.core.model.{CatalogJson, TurnProfile, TurnProfileId}
import grit.core.store.{ModelProfileStore, StoreError, Tx}

/** [[ModelProfileStore]] over `grit.model_profiles` and `grit.turn_model_profiles`. */
final class SqlModelProfileStore extends ModelProfileStore {

  def pin(workflow: WorkflowId, profile: TurnProfile)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.model_profiles (id, profile) VALUES (?, ?::jsonb) ON CONFLICT (id) DO NOTHING"
        )
      ) { ps =>
        ps.setString(1, TurnProfileId.value(profile.id))
        ps.setString(2, ujson.write(CatalogJson.writeTurn(profile)))
        ps.executeUpdate()
      }
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.turn_model_profiles (workflow_id, model_profile_id) VALUES (?, ?)
            |ON CONFLICT (workflow_id) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, WorkflowId.value(workflow))
        ps.setString(2, TurnProfileId.value(profile.id))
        ps.executeUpdate()
      }
      Right(())
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }

  def of(workflow: WorkflowId)(using tx: Tx^): Either[StoreError, Option[TurnProfile]] =
    one(
      """SELECT p.profile::text FROM grit.turn_model_profiles t
        |JOIN grit.model_profiles p ON p.id = t.model_profile_id WHERE t.workflow_id = ?""".stripMargin,
      WorkflowId.value(workflow)
    )

  def get(id: TurnProfileId)(using tx: Tx^): Either[StoreError, Option[TurnProfile]] =
    one("SELECT profile::text FROM grit.model_profiles WHERE id = ?", TurnProfileId.value(id))

  /** The profile the one-column `sql` finds for `key`, read back through its codec. */
  private def one(sql: String, key: String)(using
      tx: Tx^
  ): Either[StoreError, Option[TurnProfile]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      val text = Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, key)
        Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(rs.getString(1)))
      }
      text match {
        case None => Right(None)
        case Some(t) =>
          CatalogJson
            .readTurn(ujson.read(t))
            .map(Some(_))
            .left
            .map(why => StoreError.DatabaseError(s"model profile for $key is unreadable: $why"))
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }
}
