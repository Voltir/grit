package grit.core.id

/** Who an action is done for: a person, or grit itself. */
opaque type PrincipalId = String

object PrincipalId {

  /** The one person every edge acts for until principals are registered. */
  val Local: PrincipalId = "local"

  /** grit itself: what the engine writes on no one's behalf. */
  val Grit: PrincipalId = "grit"

  def apply(value: String): PrincipalId = value

  def value(p: PrincipalId): String = p
}
