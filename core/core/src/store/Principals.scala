package grit.core.store

import grit.core.id.{EntryId, PrincipalId}

/** The people actions are done for, by name, beyond [[PrincipalId.Local]] and
  * [[PrincipalId.Grit]]; and whose names a window shows on the inbound entries they wrote.
  */
trait Principals {

  /** Makes `id` a person named `name`, trimmed, or renames them. `Invalid` for a blank name,
    * or for `local` or `grit`, which are never enrolled.
    */
  def enroll(id: PrincipalId, name: String)(using Tx^): Either[StoreError, Unit]

  /** The names of the enrolled people who wrote `entries`: an entry that is not inbound, or
    * whose author was never enrolled, is not among them.
    */
  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers]

  /** Makes `id`, the assistant of a workspace ([[Origin.assistant]]), known as `name`,
    * trimmed, or renames it; it is never a speaker. `Invalid` for a blank name, or for
    * `local` or `grit`.
    */
  def enrollAssistant(id: PrincipalId, name: String)(using Tx^): Either[StoreError, Unit]

  /** The name `id` was last enrolled under, as a person or an assistant; none for one never
    * enrolled.
    */
  def name(id: PrincipalId)(using Tx^): Either[StoreError, Option[String]]
}

object Principals {

  /** Why `id` may not be enrolled as `name`, as [[Principals.enroll]] refuses it; `None` when
    * it may. Every store refuses by this one rule.
    */
  def refusal(id: PrincipalId, name: String): Option[StoreError] =
    if (id == PrincipalId.Local || id == PrincipalId.Grit)
      Some(StoreError.Invalid(s"${PrincipalId.value(id)} is never enrolled"))
    else Option.when(name.trim.isEmpty)(StoreError.Invalid("a person's name is not blank"))
}
