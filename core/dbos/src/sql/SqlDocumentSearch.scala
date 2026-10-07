package grit.dbos.sql

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, Document, DocumentSearch, DocumentTerms, Shelved}
import grit.core.id.{DocumentVersion, PluginName}
import grit.core.store.{StoreError, Tx}

/** [[DocumentSearch]] over `grit.documents` and `grit.document_terms`: reads only, so it needs
  * no tombstones, and what it is handed to cannot write a document.
  */
final class SqlDocumentSearch extends DocumentSearch {
  import SqlDocuments.*

  def declared()(using tx: Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
    rows(
      """SELECT plugin, label, weight, (extract(epoch FROM retention) * 1000000)::bigint AS micros,
        |       bound
        |  FROM grit.document_terms WHERE enabled ORDER BY plugin COLLATE "C"""".stripMargin
    )(_ => ()) { rs =>
      for {
        plugin <- PluginName.of(rs.getString("plugin"))
        label <- DocLabel.of(rs.getString("label"))
        weight <- DocWeight.of(rs.getDouble("weight"))
        terms <- DocumentTerms.of(label, weight, rs.getLong("micros").micros, rs.getInt("bound"))
      } yield plugin -> terms
    }

  def shelved(plugins: Vector[PluginName], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[Shelved]] =
    if (plugins.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT plugin, array_to_json(place)::text AS place FROM (
           |  SELECT DISTINCT plugin, place FROM grit.documents d
           |   WHERE plugin IN (SELECT jsonb_array_elements_text(?::jsonb))
           |     AND body IS NOT NULL AND $CurrentAt AND ${SqlClearance.kept("d")}
           |) shelved""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, strings(plugins.map(PluginName.value)))
        ps.setObject(SqlClearance.Params + 2, utc(at))
        ps.setObject(SqlClearance.Params + 3, utc(at))
      } { rs =>
        for {
          plugin <- PluginName.of(rs.getString("plugin"))
          place <- placeOf(rs.getString("place"))
        } yield Shelved(plugin, place)
      }

  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]] =
    if (query.isBlank || limit <= 0 || shelves.isEmpty) Right(Vector.empty)
    else {
      val wanted = ujson.Arr.from(shelves.map { s =>
        ujson.Obj("p" -> PluginName.value(s.plugin), "place" -> path(s.place))
      })
      cleared(Ranked) { ps =>
        ps.setString(SqlClearance.Params + 1, query)
        ps.setString(SqlClearance.Params + 2, wanted.render())
        ps.setObject(SqlClearance.Params + 3, utc(at))
        ps.setObject(SqlClearance.Params + 4, utc(at))
        ps.setInt(SqlClearance.Params + 5, limit)
      }(rs => document(rs).map(DocumentSearch.Hit(_, -rs.getDouble("s"))))
    }

  def read(versions: Vector[DocumentVersion])(using
      tx: Tx^
  ): Either[StoreError, Vector[Document]] =
    if (versions.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $Columns FROM $Labelled d
           | WHERE version IN (SELECT v::bigint FROM jsonb_array_elements_text(?::jsonb) AS e(v))
           |   AND body IS NOT NULL AND ${SqlClearance.kept("d")}""".stripMargin
      )(_.setString(SqlClearance.Params + 1, numbers(versions)))(document).map { found =>
        val byVersion = found.map(d => d.version -> d).toMap
        versions.flatMap(byVersion.get)
      }
}
