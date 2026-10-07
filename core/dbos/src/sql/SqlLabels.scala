package grit.dbos.sql

import java.sql.PreparedStatement

import scala.util.Using

import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Compartment, Compartments, Label, LabelParts}

/** Labels as `grit.labels` holds them (ADR 0030), and the compartment sets this database has
  * run under (`grit.compartments`). LabelParts is the one translation between a [[Label]] and
  * its stored form; every statement that writes or reads a label goes through here.
  */
private[dbos] object SqlLabels {
  import SqlEntryStore.attempt

  /** A label as a statement's argument, of type `grit.label`: two parameters, which [[bind]]
    * sets.
    */
  val Arg: String =
    "ROW(?::smallint, ARRAY(SELECT e FROM jsonb_array_elements_text(?::jsonb) " +
      "WITH ORDINALITY AS t(e, i) ORDER BY i))::grit.label"

  /** An [[Arg]]'s id in `grit.labels`, interned on first use (`grit.intern_label`, the table's
    * one writer).
    */
  val Interned: String = s"grit.intern_label($Arg)"

  /** Sets `label` as the [[Arg]] whose first parameter is `at`: `at` and `at + 1`. */
  def bind(ps: PreparedStatement, at: Int, label: Label): Unit = {
    ps.setInt(at, LabelParts.rank(label))
    ps.setString(at + 1, ujson.Arr.from(LabelParts.compartments(label).map(ujson.Str(_))).render())
  }

  /** The columns that read a `grit.labels` row aliased `alias` back: its level, and its
    * compartments as JSON, which [[read]] takes.
    */
  def columns(alias: String): String = s"$alias.level, to_jsonb($alias.compartments)::text"

  /** The label stored as `rank` and `compartments` (JSON text, as [[columns]] reads it), failing
    * closed as [[LabelParts.of]] does; compartments that are not a JSON array of strings read as
    * one name that is no compartment's.
    */
  def read(rank: Int, compartments: String): Label = {
    val names = scala.util
      .Try(ujson.read(compartments).arr.toVector.map(_.str))
      .getOrElse(Vector(""))
    LabelParts.of(rank, names)
  }

  /** Records `declared` as the compartments this database runs under: nothing when they are
    * the newest set recorded, the next version when they hold every one of it and more (the
    * first version on a database with none). Otherwise writes nothing and returns the first
    * compartment of the newest set that `declared` drops, by name: dropping or renaming one
    * could make a stored label readable by a clearance that could not read it.
    */
  def reconcile(declared: Compartments)(using tx: Tx^): Either[StoreError, Option[Compartment]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val newest = attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT version, compartment FROM grit.compartments
            | WHERE version = (SELECT max(version) FROM grit.compartments)""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(Int, String)]
          while (rs.next()) rows += ((rs.getInt(1), rs.getString(2)))
          rows.result()
        }
      }
    }
    for {
      rows <- newest
      version = rows.map(_._1).maxOption.getOrElse(0)
      before <- rows
        .foldLeft[Either[String, Vector[Compartment]]](Right(Vector.empty)) { (acc, row) =>
          acc.flatMap(cs => Compartment.of(row._2).map(cs :+ _))
        }
        .flatMap(Compartments.of(_).left.map(c => s"${Compartment.name(c)} twice"))
        .left
        .map(why => StoreError.Invalid(s"grit.compartments' version $version: $why"))
      dropped <-
        if (!declared.keeps(before)) {
          Right(
            before.declared.diff(declared.declared).toVector.sortBy(Compartment.name).headOption
          )
        } else if (version > 0 && declared == before) Right(None)
        else
          attempt {
            Using.resource(
              conn.prepareStatement(
                """INSERT INTO grit.compartments (version, compartment)
                  |SELECT ?, jsonb_array_elements_text(?::jsonb)""".stripMargin
              )
            ) { ps =>
              ps.setInt(1, version + 1)
              ps.setString(
                2,
                ujson.Arr
                  .from(declared.declared.toVector.map(Compartment.name).sorted.map(ujson.Str(_)))
                  .render()
              )
              ps.executeUpdate()
              None
            }
          }
    } yield dropped
  }
}
