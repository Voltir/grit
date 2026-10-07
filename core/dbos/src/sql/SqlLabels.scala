package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}

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

  /** `label`'s id in `grit.labels`, interned on first use: for a statement that must then read
    * the row back, which one that interns it in passing cannot (its snapshot predates the row).
    */
  def intern(label: Label)(using tx: Tx^): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(s"SELECT $Interned")) { ps =>
        bind(ps, 1, label)
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          rs.getInt(1)
        }
      }
    }
  }

  /** Sets `label` as the [[Arg]] whose first parameter is `at`: `at` and `at + 1`. */
  def bind(ps: PreparedStatement, at: Int, label: Label): Unit = {
    ps.setInt(at, LabelParts.rank(label))
    ps.setString(at + 1, ujson.Arr.from(LabelParts.compartments(label).map(ujson.Str(_))).render())
  }

  /** The columns that select the `grit.labels` row aliased `alias` for [[read]]. */
  def columns(alias: String): String =
    s"$alias.level AS label_level, to_jsonb($alias.compartments)::text AS label_compartments"

  /** The label the current row of `rs` holds in [[columns]], failing closed as
    * [[LabelParts.of]] does; compartments that are not a JSON array of strings read as one name
    * that is no compartment's. Throws as `rs` does when the row has no such columns.
    */
  def read(rs: ResultSet): Label = {
    val names = scala.util
      .Try(ujson.read(rs.getString("label_compartments")).arr.toVector.map(_.str))
      .getOrElse(Vector(""))
    LabelParts.of(rs.getInt("label_level"), names)
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
