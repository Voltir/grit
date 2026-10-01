package grit.dbos.sql

import scala.util.Using

import grit.core.store.{StoreError, Tx}
import grit.core.tool.{ToolSet, ToolSetId, ToolSets}

/** [[ToolSets]] over `grit.tool_sets`. */
final class SqlToolSets extends ToolSets {
  import SqlEntryStore.attempt

  def keep(set: ToolSet)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.tool_sets (id, tools) VALUES (?, ?::jsonb) ON CONFLICT (id) DO NOTHING"
        )
      ) { ps =>
        ps.setString(1, ToolSetId.value(set.id))
        ps.setString(2, ujson.write(ToolSet.write(set)))
        ps.executeUpdate()
        ()
      }
    }
  }

  def get(id: ToolSetId)(using tx: Tx^): Either[StoreError, ToolSet] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement("SELECT tools::text FROM grit.tool_sets WHERE id = ?")) {
        ps =>
          ps.setString(1, ToolSetId.value(id))
          Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(rs.getString(1)))
      }
    }.flatMap {
      case None => Left(StoreError.Invalid(s"no tool set ${ToolSetId.value(id)} is kept"))
      case Some(text) =>
        ToolSet
          .read(ujson.read(text))
          .left
          .map(why =>
            StoreError.DatabaseError(s"tool set ${ToolSetId.value(id)} is unreadable: $why")
          )
    }
  }
}
