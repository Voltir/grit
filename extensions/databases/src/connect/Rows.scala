package grit.databases.connect

/** A result: `columns`, then each row's values as Postgres writes them as text, `None` for
  * NULL, each row as wide as `columns`.
  */
final case class Rows private[databases] (
    columns: Vector[String],
    rows: Vector[Vector[Option[String]]],
    clip: Option[Clip]
)

/** Why a result stops short. */
enum Clip {

  /** It had more than the limits' rows; that many are kept. */
  case Rows

  /** The next row, with the columns' names and the rows before it, would pass the limits'
    * bytes; none may be kept.
    */
  case Bytes
}
