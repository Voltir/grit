package grit.dbos.sql

import scala.util.Using

import grit.core.place.{Namespace, Place}
import grit.core.store.{StoreError, Tx}

/** `grit.places`: each place grit has recorded anything at, by its segments. */
private[dbos] object SqlPlaces {
  import SqlEntryStore.attempt

  /** `place`'s id, its row inserted first when there is none. The insert is `ON CONFLICT DO
    * NOTHING`, never `DO UPDATE`, so it never locks an existing row: a concurrent insert of the
    * same place is waited for and its row's id returned.
    */
  def id(place: Place)(using tx: Tx^): Either[StoreError, String] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.places (path)
            |VALUES (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))
            |ON CONFLICT (path) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, path(place))
        ps.executeUpdate()
      }
      // Read committed: this statement sees the row whichever insert made it.
      Using.resource(
        conn.prepareStatement(
          "SELECT id FROM grit.places WHERE path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))"
        )
      ) { ps =>
        ps.setString(1, path(place))
        Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(rs.getString(1)))
      }
    }.flatMap(_.toRight(StoreError.Invalid(s"the place ${place.written} was not recorded")))
  }

  /** `place`'s segments as a JSON array, as every statement here binds a path; no Java array
    * crosses JDBC.
    */
  def path(place: Place): String = ujson.Arr.from(place.segments.map(ujson.Str(_))).render()

  /** The place whose segments `json` holds (`array_to_json(path)`); why not, when its first
    * segment is no namespace's.
    */
  def read(json: String): Either[String, Place] =
    ujson.read(json).arrOpt.fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt)) match {
      case ns +: rest =>
        Namespace.of(ns).map(Place.under(_, rest)).toRight(s"a place: no namespace $ns")
      case _ => Right(Place.Everywhere)
    }
}
