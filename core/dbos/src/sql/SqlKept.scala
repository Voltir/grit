package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.Instant

import grit.core.id.PluginName
import grit.core.retention.Target
import grit.core.store.{StoreError, Tombstones, Tx}

/** A plugin's kept units as rows of `table` keyed by `id`, one current per `name` and label from
  * `written_at` until `superseded_at`, marked for deletion as `kind`: what [[SqlDocuments]] and
  * [[SqlPieces]] write alike over their own tables (ADR 0046).
  */
private[sql] final case class SqlKept(table: String, id: String, name: String, kind: Target.Kind) {
  import SqlDocuments.{rows, update, utc}

  /** `table` with each row's label's and integrity's columns, for a read to select from. */
  val labelled: String =
    s"(SELECT d.*, ${SqlLabels.columns("l")}, ${SqlIntegrity.columns("d.integrity_id")} " +
      s"FROM $table d JOIN grit.classifications l ON l.id = d.classification_id)"

  /** `plugin`'s current row under `named` at the label whose id is `label`, if `held` is true of
    * it, locked: its `id` as `read` reads it.
    */
  def current[A](plugin: PluginName, named: String, label: Int, held: String = "true")(
      read: ResultSet => Either[String, A]
  )(using Tx^): Either[StoreError, Option[A]] =
    rows(
      s"""SELECT $id FROM $table
         | WHERE plugin = ? AND $name = ? AND classification_id = ? AND superseded_at IS NULL
         |   AND $held FOR UPDATE""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, named)
      ps.setInt(3, label)
    }(read).map(_.headOption)

  def forget(key: Long)(using Tx^): Either[StoreError, Unit] =
    update(s"DELETE FROM $table WHERE $id = ?")(_.setLong(1, key)).map(_ => ())

  /** The rows `where` matches deleted, their pending tombstones ended at `at`: `bind` sets
    * `where`'s parameters, the first at the index it is given.
    */
  def delete(where: String, at: Instant)(bind: (PreparedStatement, Int) => Unit)(using
      Tx^
  ): Either[StoreError, Unit] =
    update(
      "WITH fixed AS (SELECT ?::timestamptz AS at, ?::text AS kind),\n" +
        s"gone AS (DELETE FROM $table WHERE $where RETURNING $id)\n" +
        "UPDATE grit.tombstones t SET collected_at = f.at FROM fixed f WHERE t.kind = f.kind " +
        s"AND t.collected_at IS NULL AND t.target IN (SELECT $id::text FROM gone)"
    ) { ps =>
      ps.setObject(1, utc(at))
      ps.setString(2, kind.name)
      bind(ps, 3)
    }.map(_ => ())

  /** The row `target` names, one of `kind`'s, no longer current from `at`, and marked for
    * deletion.
    */
  def supersede(target: Target, tombstones: Tombstones, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit] =
    update(s"UPDATE $table SET superseded_at = ? WHERE $id = ?::bigint") { ps =>
      ps.setObject(1, utc(at))
      ps.setString(2, Target.key(target))
    }.flatMap(_ => tombstones.write(target, at).map(_ => ()))
}

private[sql] object SqlKept {

  val Documents: SqlKept = SqlKept("grit.documents", "version", "key", Target.Kind.Document)

  val PieceSets: SqlKept = SqlKept("grit.piece_sets", "id", "ref", Target.Kind.PieceSet)

  /** `alias` current at the instant bound twice here: written before it, superseded by none
    * written before it.
    */
  def currentAt(alias: String): String =
    s"$alias.written_at < ? AND ($alias.superseded_at IS NULL OR $alias.superseded_at >= ?)"

  /** `alias` of the plugins [[of]] binds, [[currentAt]] the instant it binds, and read by the
    * transaction.
    */
  def currentOf(alias: String): String =
    s"$alias.plugin IN (SELECT jsonb_array_elements_text(?::jsonb)) AND ${currentAt(alias)} " +
      s"AND ${SqlClearance.kept(alias)}"

  /** [[currentOf]]'s parameters, from the `i`th. */
  def of(ps: PreparedStatement, i: Int, plugins: Vector[PluginName], at: Instant): Unit = {
    ps.setString(i, SqlDocuments.strings(plugins.map(PluginName.value)))
    ps.setObject(i + 1, SqlDocuments.utc(at))
    ps.setObject(i + 2, SqlDocuments.utc(at))
  }

  /** Of the rows `sized` selects, each with its `measure`, those but the one `spared` matches
    * that go, first in `order`, until all left measure at most the bound: in that order. Its
    * parameters are `sized`'s, `spared`'s, then the bound.
    */
  def cut(sized: String, order: String, spared: String): String =
    // Not stripMargin: `sized` may hold a `||`.
    s"WITH sized AS (SELECT s.*, sum(s.measure) OVER () AS whole FROM ($sized) s),\n" +
      s"ordered AS (SELECT *, sum(measure) OVER (ORDER BY $order ROWS BETWEEN UNBOUNDED " +
      s"PRECEDING AND 1 PRECEDING) AS freed FROM sized WHERE NOT ($spared))\n" +
      s"SELECT * FROM ordered WHERE whole - coalesce(freed, 0) > ? ORDER BY $order"
}
