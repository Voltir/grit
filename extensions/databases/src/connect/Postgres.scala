package grit.databases.connect

import java.sql.SQLException

/** What reading a database says to Postgres, and how its failures read: the one place a second
  * engine would differ.
  */
private[connect] object Postgres {

  val Begin: String = "BEGIN READ ONLY"

  /** A statement timeout of `millis`, for the transaction alone. */
  def timeout(millis: Long): String = s"SET LOCAL statement_timeout = $millis"

  val Who: String = "SELECT current_user, session_user"

  val Rollback: String = "ROLLBACK"

  /** What clears the session state a rollback leaves: its advisory locks and its `LISTEN`s. */
  val Reset: Vector[String] = Vector("SELECT pg_advisory_unlock_all()", "UNLISTEN *")

  /** Each column the role may select, of each table, view, materialized view or foreign table
    * outside the system schemas and in a schema it may use, no partition, by schema, relation
    * and column order: schema, relation, its kind, its comment, the column, its type and
    * comment.
    */
  val Columns: String =
    """SELECT n.nspname, c.relname, c.relkind::text, obj_description(c.oid, 'pg_class'),
      |       a.attname, format_type(a.atttypid, a.atttypmod), col_description(c.oid, a.attnum)
      |FROM pg_class c
      |JOIN pg_namespace n ON n.oid = c.relnamespace
      |JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped
      |WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f') AND NOT c.relispartition
      |  AND n.nspname NOT IN ('pg_catalog', 'information_schema')
      |  AND n.nspname NOT LIKE 'pg\_toast%' AND n.nspname NOT LIKE 'pg\_temp\_%'
      |  AND has_schema_privilege(n.oid, 'USAGE')
      |  AND has_column_privilege(c.oid, a.attnum, 'SELECT')
      |ORDER BY n.nspname, c.relname, a.attnum""".stripMargin

  /** Each foreign key whose columns the role may all select, a row per column pair in the key's
    * order: its schema and relation, the referenced schema and relation, the key's name, the
    * column and the referenced column.
    */
  val References: String =
    """SELECT s, r, fs, fr, k, col, fcol FROM (
      |  SELECT cn.nspname AS s, cr.relname AS r, fn.nspname AS fs, fr.relname AS fr,
      |         k.conname AS k, u.ord, a.attname AS col, fa.attname AS fcol,
      |         bool_and(has_column_privilege(k.conrelid, u.attnum, 'SELECT'))
      |           OVER (PARTITION BY k.oid) AS readable
      |  FROM pg_constraint k
      |  JOIN pg_class cr ON cr.oid = k.conrelid
      |  JOIN pg_namespace cn ON cn.oid = cr.relnamespace
      |  JOIN pg_class fr ON fr.oid = k.confrelid
      |  JOIN pg_namespace fn ON fn.oid = fr.relnamespace
      |  CROSS JOIN LATERAL unnest(k.conkey, k.confkey) WITH ORDINALITY AS u(attnum, fattnum, ord)
      |  JOIN pg_attribute a ON a.attrelid = k.conrelid AND a.attnum = u.attnum
      |  JOIN pg_attribute fa ON fa.attrelid = k.confrelid AND fa.attnum = u.fattnum
      |  WHERE k.contype = 'f'
      |) q WHERE readable
      |ORDER BY s, r, k, ord""".stripMargin

  /** `e` as grit keeps it: its SQLSTATE, and its message's first line with a server's severity
    * (`ERROR: `, `FATAL: `) taken off.
    */
  def failure(e: SQLException): Failure = {
    val first = Option(e.getMessage).getOrElse("").linesIterator.nextOption().getOrElse("")
    Failure(Option(e.getSQLState), Severity.replaceFirstIn(first, ""))
  }

  private val Severity = "^(ERROR|FATAL|PANIC): ".r

  /** Whether `f` says the connection itself failed: SQLSTATE class 08, or the server shutting
    * the session down (`57P01` to `57P03`), or no SQLSTATE at all.
    */
  def broken(f: Failure): Boolean =
    f.state.forall(s => s.startsWith("08") || s == "57P01" || s == "57P02" || s == "57P03")

  /** Whether `f` is the driver's buffer for one answer running over. */
  def overflowed(f: Failure): Boolean = f.message.startsWith("Result set exceeded maxResultBuffer")
}
