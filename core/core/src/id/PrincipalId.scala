package grit.core.id

/** Who an action is done for: a person, or grit itself. A person's id is the store's
  * ([[grit.core.store.Principals]]), made when it first sees one of their accounts, and is never
  * read for meaning.
  */
opaque type PrincipalId = String

object PrincipalId {

  /** The person the local edge acts for: its registrations and its tool calls. */
  val Local: PrincipalId = "local"

  /** grit itself: what the engine writes on no one's behalf. */
  val Grit: PrincipalId = "grit"

  def value(p: PrincipalId): String = p

  private[id] def stored(text: String): PrincipalId = text
}

/** A principal's id as the store keeps it, for `grit.dbos` alone (enola-intent.yaml): text from
  * anywhere else names no one the store made.
  */
object PrincipalIds {
  def stored(text: String): PrincipalId = PrincipalId.stored(text)
}
