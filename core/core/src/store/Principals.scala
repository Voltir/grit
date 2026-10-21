package grit.core.store

import java.time.ZoneId

import grit.core.id.EntryId
import grit.core.identity.Account

/** The accounts actions come through, and the names they go by; and whose names a window
  * shows on the inbound entries they wrote. An account is a person of its own until a trusted
  * realm attests it an email the deployment claims (ADR 0032).
  */
trait Principals {

  /** Records that `account` goes by `name`, trimmed, as its source names it now, writing
    * nothing when that is already its name; an account not seen before is a new person's one
    * account. `Invalid` for a blank name, or for
    * [[Account.Local]] or [[Account.Grit]], which are never named.
    */
  def name(account: Account, name: String)(using Tx^): Either[StoreError, Unit]

  /** The names their accounts go by, of whoever wrote `entries`, and the link each was recorded
    * with ([[grit.core.inbox.Inbox.ingest]]): an entry that is not inbound is not among them;
    * one whose account has no name is among them by its link alone, when it has one.
    */
  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers]

  /** Records that `account`'s source places it in `zone` now, writing nothing when it already does;
    * an account not seen before is a new person's one account. `Invalid` for [[Account.Local]] or
    * [[Account.Grit]], which no source places: a turn they begin is dated in the deployment's zone.
    */
  def zoned(account: Account, zone: ZoneId)(using Tx^): Either[StoreError, Unit]

  /** The zone its source last placed the account that wrote `entry` in; `None` for an entry that is
    * not inbound, or whose account no source has placed.
    */
  def zoneOf(entry: EntryId)(using Tx^): Either[StoreError, Option[ZoneId]]
}

object Principals {

  /** Why `account` may not be named `name`, as [[Principals.name]] refuses it; `None` when it
    * may. Every store refuses by this one rule.
    */
  def refusal(account: Account, name: String): Option[StoreError] =
    if (account == Account.Local || account == Account.Grit)
      Some(StoreError.Invalid(s"${Account.written(account)} is never named"))
    else Option.when(name.trim.isEmpty)(StoreError.Invalid("a person's name is not blank"))

  /** Why `account` may not be placed in a zone, as [[Principals.zoned]] refuses it; `None` when
    * it may. Every store refuses by this one rule.
    */
  def refusal(account: Account): Option[StoreError] =
    Option.when(account == Account.Local || account == Account.Grit)(
      StoreError.Invalid(s"${Account.written(account)} is placed by no source")
    )
}
