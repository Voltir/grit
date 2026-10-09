package grit.core.schema

/** Why some JSON is not a [[JsonSchema]]: at `path` (as `properties.edits.items`, `""` at the
  * root), `why`.
  */
final case class SchemaError(path: String, why: String) {

  /** `"{path}: {why}"`, or `why` at the root. */
  def message: String = if (path.isEmpty) why else s"$path: $why"
}
