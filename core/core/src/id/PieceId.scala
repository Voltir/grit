package grit.core.id

/** One piece of a kept file (`grit.core.files.Piece`): numbered across every plugin and never
  * reused. A piece kept again in a later revision of its file, its words unchanged, keeps its id.
  * At least 1.
  */
opaque type PieceId = Long

object PieceId {

  /** `n` as a piece's id, or `None` below 1. */
  def of(n: Long): Option[PieceId] = Option.when(n >= 1)(n)

  def value(p: PieceId): Long = p
}

/** One revision of one kept file's pieces, as a plugin kept them: numbered across every plugin
  * and never reused. At least 1.
  */
opaque type PieceSetId = Long

object PieceSetId {

  /** `n` as a set's id, or `None` below 1. */
  def of(n: Long): Option[PieceSetId] = Option.when(n >= 1)(n)

  def value(s: PieceSetId): Long = s
}
