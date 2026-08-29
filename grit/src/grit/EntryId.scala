package grit

/** Identifier for an [[Entry]]. Opaque so it cannot be transposed with another
  * identifier, or with a bare `String`, at a call site.
  */
opaque type EntryId = String

object EntryId {
  def apply(value: String): EntryId = value
  def value(id: EntryId): String = id
}
