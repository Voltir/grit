package grit.dbos.sql

import java.sql.PreparedStatement

import grit.core.visibility.Clearance

/** A statement's transaction's clearance ([[grit.core.store.Tx.clearance]]) as SQL (ADR 0030): the one place a
  * read of labelled rows is filtered, written once for every store. A statement begins
  * `WITH ${SqlClearance.With}` (more `WITH` items may follow it), sets its first [[Params]]
  * parameters by [[bind]], and puts a predicate below in its `WHERE`, inside any subquery that
  * ranks or limits, so a row it does not read never takes a readable one's place.
  */
private[sql] object SqlClearance {

  /** The `WITH` item `clearance`, one row: the ids of the labels the transaction reads
    * everywhere (`everywhere`), the ids of those its own room's label dominates (`own`), and its
    * own room's `grit.places` id (`room`, NULL when it has none, or the place was never
    * recorded). Dominance is `grit.label_dominates` over `grit.labels`, run with the statement,
    * so a label interned earlier in the same transaction is covered.
    */
  val With: String =
    s"""clearance AS (
       |  SELECT ARRAY(SELECT l.id FROM grit.labels l
       |                WHERE grit.label_dominates(${SqlLabels.Arg},
       |                                           ROW(l.level, l.compartments)::grit.label)) AS everywhere,
       |         ARRAY(SELECT l.id FROM grit.labels l
       |                WHERE grit.label_dominates(${SqlLabels.Arg},
       |                                           ROW(l.level, l.compartments)::grit.label)) AS own,
       |         (SELECT p.id FROM grit.places p
       |           WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))) AS room
       |)""".stripMargin

  /** How many parameters [[With]] takes, all before any other of its statement's. */
  val Params: Int = 5

  /** Sets [[With]]'s parameters, from `at`, to `clearance`: the statement's transaction's
    * ([[grit.core.store.Tx.clearance]]), read before the statement is bound, since a binding closure may not
    * capture the transaction its statement runs in. With no own room, the own branch binds
    * `everywhere` and a path no place has, so it reads nothing more.
    */
  def bind(ps: PreparedStatement, at: Int, clearance: Clearance): Unit = {
    SqlLabels.bind(ps, at, clearance.everywhere)
    SqlLabels.bind(ps, at + 2, clearance.own.fold(clearance.everywhere)(_.label))
    ps.setString(
      at + 4,
      ujson.Arr.from(clearance.own.toVector.flatMap(_.room.segments).map(ujson.Str(_))).render()
    )
  }

  /** Whether the transaction reads the `grit.entries` row aliased `e`
    * ([[grit.core.visibility.Item.InRoom]] of its conversation's room).
    */
  def entry(e: String): String =
    s"""($e.label_id = ANY ((SELECT everywhere FROM clearance)::smallint[])
       |  OR ($e.label_id = ANY ((SELECT own FROM clearance)::smallint[])
       |      AND $e.conversation_id IN (SELECT rc.id FROM grit.conversations rc
       |                                  WHERE rc.room_id = (SELECT room FROM clearance))))""".stripMargin

  /** Whether the transaction reads what the `grit.conversations` row aliased `c` holds: its
    * entries, every one kept at its label, and where it happens.
    */
  def conversation(c: String): String =
    s"""($c.label_id = ANY ((SELECT everywhere FROM clearance)::smallint[])
       |  OR ($c.label_id = ANY ((SELECT own FROM clearance)::smallint[])
       |      AND $c.room_id = (SELECT room FROM clearance)))""".stripMargin

  /** Whether the transaction reads the `grit.documents` or `grit.plugin_docs` row aliased `d`
    * ([[grit.core.visibility.Item.Kept]] in its `room_id`, NULL for none).
    */
  def kept(d: String): String =
    s"""($d.label_id = ANY ((SELECT everywhere FROM clearance)::smallint[])
       |  OR ($d.label_id = ANY ((SELECT own FROM clearance)::smallint[])
       |      AND $d.room_id = (SELECT room FROM clearance)))""".stripMargin
}
