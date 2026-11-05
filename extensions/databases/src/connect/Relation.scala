package grit.databases.connect

/** A relation the role may read. */
final case class Relation(
    schema: String,
    name: String,
    kind: Relation.Kind,
    comment: Option[String],
    columns: Vector[Relation.Column],
    references: Vector[Relation.Reference]
)

object Relation {

  enum Kind {
    case Table, View, MaterializedView, ForeignTable
  }

  /** `typeName` as `format_type` writes it (`character varying(40)`). */
  final case class Column(name: String, typeName: String, comment: Option[String])

  /** A foreign key to `schema.table`: each pair a column of this relation and the column of
    * `table` its values are, in the key's order.
    */
  final case class Reference(schema: String, table: String, pairs: Vector[(String, String)])
}
