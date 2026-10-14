package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}

import grit.core.visibility.{Integrity, IntegrityParts}

/** Integrity as `grit.integrity_labels` holds it (ADR 0036). IntegrityParts is the one
  * translation between an [[Integrity]] and its stored form; every statement that writes or
  * reads a row's integrity goes through here.
  */
private[dbos] object SqlIntegrity {

  /** An integrity's id in `grit.integrity_labels`, interned on first use
    * (`grit.intern_integrity`, the table's one writer): two parameters, which [[bind]] sets.
    */
  val Interned: String =
    "grit.intern_integrity(ROW(?::smallint, ARRAY(SELECT e FROM jsonb_array_elements_text(?::jsonb) " +
      "WITH ORDINALITY AS t(e, i) ORDER BY i))::grit.integrity)"

  /** Sets `integrity` as the [[Interned]] whose first parameter is `at`: `at` and `at + 1`. */
  def bind(ps: PreparedStatement, at: Int, integrity: Integrity): Unit = {
    ps.setInt(at, IntegrityParts.rank(integrity))
    ps.setString(
      at + 1,
      ujson.Arr.from(IntegrityParts.compartments(integrity).map(ujson.Str(_))).render()
    )
  }

  /** The columns that select, for [[read]], the `grit.integrity_labels` row whose id the
    * column `id` holds: subselects, so a statement selecting them needs no join.
    */
  def columns(id: String): String =
    s"(SELECT il.level FROM grit.integrity_labels il WHERE il.id = $id) AS integrity_level, " +
      s"(SELECT to_jsonb(il.compartments)::text FROM grit.integrity_labels il WHERE il.id = $id) " +
      "AS integrity_compartments"

  /** The integrity the current row of `rs` holds in [[columns]], failing closed as
    * [[IntegrityParts.of]] does; compartments that are not a JSON array of strings read as none.
    * Throws as `rs` does when the row has no such columns.
    */
  def read(rs: ResultSet): Integrity = {
    val names = scala.util
      .Try(ujson.read(rs.getString("integrity_compartments")).arr.toVector.map(_.str))
      .getOrElse(Vector.empty)
    IntegrityParts.of(rs.getInt("integrity_level"), names)
  }
}
