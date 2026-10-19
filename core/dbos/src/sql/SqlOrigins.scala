package grit.dbos.sql

import grit.core.edge.Origins
import grit.core.id.ConversationId
import grit.core.store.{Origin, StoreError, Tx}

/** [[Origins]] over `grit.conversations`, read through [[SqlClearance.conversation]]. */
final class SqlOrigins extends Origins {

  def of(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Option[Origin]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT c.origin::text AS origin
           |  FROM grit.conversations c
           | WHERE c.id = ?::uuid AND ${SqlClearance.conversation("c")}""".stripMargin
      )(ps => ps.setString(SqlClearance.Params + 1, ConversationId.value(conversation)))(
        _.getString("origin")
      )
      .flatMap(_.headOption match {
        case None => Right(None)
        case Some(origin) =>
          SqlConversationStore
            .readOrigin(ujson.read(origin))
            .map(Some(_))
            .left
            .map(why =>
              StoreError.DatabaseError(
                s"conversation ${ConversationId.value(conversation)}: $why"
              )
            )
      })
}
