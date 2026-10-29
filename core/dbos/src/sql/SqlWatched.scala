package grit.dbos.sql

import scala.util.Try

import grit.core.document.DocRef
import grit.core.edge.{WatchFor, Watched}
import grit.core.id.{ClaimHolder, DocKey, Harness, PluginName, SessionId}
import grit.core.session.Event
import grit.core.store.{StoreError, Tx}

/** What meets a watch, as the transaction reads it. */
private[dbos] object SqlWatched {

  import SqlDocuments.{Labelled, cleared, instant, refsJson, versionOf}

  /** What meets `what` as this transaction reads it ([[Watched]] says which), or none. */
  def met(what: WatchFor)(using Tx^): Either[StoreError, Option[Watched]] = what match {
    case h: WatchFor.Holding => held(h)
    case c: WatchFor.Committed => committed(c)
  }

  private def held(h: WatchFor.Holding)(using Tx^): Either[StoreError, Option[Watched]] =
    cleared(
      s"""WITH ${SqlClearance.With}
         |SELECT d.version, d.written_at, m.met::text AS met
         |  FROM $Labelled d
         |  JOIN LATERAL (
         |    SELECT json_agg(json_build_object('kind', r.kind, 'ref', r.ref)) AS met
         |      FROM grit.document_refs r
         |      JOIN jsonb_to_recordset(?::jsonb) AS w(kind text, ref text)
         |        ON w.kind = r.kind AND w.ref = r.ref
         |     WHERE r.version = d.version) m ON m.met IS NOT NULL
         | WHERE d.plugin = ? AND d.key = ? AND d.superseded_at IS NULL AND d.body IS NOT NULL
         |   AND ${SqlClearance.kept("d")}
         | ORDER BY d.written_at DESC, d.version DESC LIMIT 1""".stripMargin
    ) { ps =>
      ps.setString(SqlClearance.Params + 1, refsJson(h.refs))
      ps.setString(SqlClearance.Params + 2, PluginName.value(h.plugin))
      ps.setString(SqlClearance.Params + 3, DocKey.value(h.key))
    } { rs =>
      for {
        version <- versionOf(rs)
        met <- refs(rs.getString("met"))
      } yield Watched.Held(version, met, instant(rs, "written_at"))
    }.map(_.headOption)

  /** The refs `json` holds, a JSON array of `{"kind":…,"ref":…}`, or why not. */
  private def refs(json: String): Either[String, Set[DocRef]] =
    Try(ujson.read(json).arr.toVector).toOption
      .toRight(s"no refs in $json")
      .flatMap(_.foldLeft[Either[String, Set[DocRef]]](Right(Set.empty)) { (acc, r) =>
        for {
          got <- acc
          o <- r.objOpt.toRight(s"no ref in $r")
          kind <- o.get("kind").flatMap(_.strOpt).toRight(s"no kind in $r")
          ref <- o.get("ref").flatMap(_.strOpt).toRight(s"no ref in $r")
          one <- DocRef.of(kind, ref)
        } yield got + one
      })

  private def committed(c: WatchFor.Committed)(using Tx^): Either[StoreError, Option[Watched]] = {
    val (where, by) = c.by match {
      case WatchFor.Worker.As(holder) => ("s.holder = ?", ClaimHolder.value(holder))
      case WatchFor.Worker.At(worktree) =>
        (s"e.shape->>'${Event.Shape.Worktree}' = ?", worktree.written)
    }
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT s.harness, s.local, e.at, e.shape::text AS shape
           |  FROM grit.sessions s
           |  JOIN grit.session_events e ON e.session = s.id
           | WHERE e.kind = 'commit' AND e.at >= ? AND $where AND ${SqlClearance.kept("s")}
           | ORDER BY e.at, s.harness, s.local, e.seq LIMIT 1""".stripMargin
      ) { ps =>
        ps.setObject(SqlClearance.Params + 1, SqlDocuments.utc(c.since))
        ps.setString(SqlClearance.Params + 2, by)
      } { rs =>
        for {
          harness <- Harness.of(rs.getString("harness"))
          session <- SessionId.of(harness, rs.getString("local"))
          shape <- Try(ujson.read(rs.getString("shape")).obj).toOption
            .toRight("a commit's shape is no JSON object")
            .flatMap(o => Event.Shape.of(ujson.Obj.from(o)))
        } yield Watched.Committed(session, instant(rs, "at"), shape)
      }
      .flatMap(SqlStatements.valid)
      .map(_.headOption)
  }
}
