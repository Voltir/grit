package grit.core.id

/** A served edge's name, as the log and a refusal state it (`slack`). */
opaque type EdgeName = String

object EdgeName {
  def apply(value: String): EdgeName = value
  def value(name: EdgeName): String = name
}
