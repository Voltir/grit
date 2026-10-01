package grit.core.id

/** A balance line's id: the first 16 hex digits of the SHA-256 of its section's key, a
  * newline and its text, in UTF-8. A line has the same id in every closing that carries it,
  * in any conversation.
  */
opaque type LineId = String

object LineId {

  /** The id of the line `text` in the section keyed `section`. */
  private[core] def of(section: String, text: String): LineId = ShortHash.of(s"$section\n$text")

  def value(id: LineId): String = id
}
