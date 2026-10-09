package grit.core.schema

/** Where some JSON does not conform to a [[JsonSchema]]: at `path` (as `edits[2].oldText`,
  * `""` at the root), `why`.
  */
final case class Mismatch(path: String, why: String) {

  /** `"{path}: {why}"`, or `why` at the root. */
  def message: String = if (path.isEmpty) why else s"$path: $why"
}
