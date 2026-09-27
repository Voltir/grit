package grit.core.id

/** An edge's registration: one per principal, machine and set of places hosted at once. */
opaque type EdgeId = String

object EdgeId {
  def apply(value: String): EdgeId = value
  def value(id: EdgeId): String = id
}
