package grit.core.store

import grit.core.id.EntryId
import grit.core.identity.Account

/** The accounts actions come through, and the names they go by; and whose names a window
  * shows on the inbound entries they wrote.
  */
trait Principals {

  /** Records that `account` goes by `name`, trimmed, as its source names it now; an account not
    * seen before is a new person's one account. `Invalid` for a blank name, or for
    * [[Account.Local]] or [[Account.Grit]], which are never named.
    */
  def name(account: Account, name: String)(using Tx^): Either[StoreError, Unit]

  /** The names their accounts go by, of whoever wrote `entries`: an entry that is not
    * inbound, or whose account has no name, is not among them.
    */
  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers]
}

object Principals {

  /** Why `account` may not be named `name`, as [[Principals.name]] refuses it; `None` when it
    * may. Every store refuses by this one rule.
    */
  def refusal(account: Account, name: String): Option[StoreError] =
    if (account == Account.Local || account == Account.Grit)
      Some(StoreError.Invalid(s"${Account.written(account)} is never named"))
    else Option.when(name.trim.isEmpty)(StoreError.Invalid("a person's name is not blank"))
}
