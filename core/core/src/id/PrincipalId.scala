package grit.core.id

/** Who an action is done for: a person, or grit itself. A person's id is minted by the store
  * ([[grit.core.store.Principals]]), for an account's home when it first sees the account, or
  * for an email's person when a trusted realm first attests the address (ADR 0032); it names
  * no account and is never read for meaning.
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
