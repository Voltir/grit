package grit.core.visibility

import grit.core.place.Place

/** One item of a service's results as the extension reading them accounts for it (ADR 0036):
  * `place`, where in the service it is kept (a repository, a mailbox, a folder), a place under
  * the service's own; and `vouched`, what its author is believed at, as that extension judges
  * from the result and the declarations it was built with, [[Integrity.Untrusted]] when the
  * result does not say who wrote it. What the item is believed at is its service's
  * declaration's to say ([[Visibility.believed]]).
  */
final case class Provenance(place: Place, vouched: Integrity) derives CanEqual

/** What a service's extension read of one result's items: none ([[Accounted.Unread]]), their
  * distinct provenances ([[Accounted.Items]], made by [[Accounted.of]]), or that it could not
  * account for them ([[Accounted.Unaccountable]]).
  */
sealed trait Accounted derives CanEqual

object Accounted {

  /** The extension reads no item of the result: it is believed at its service's declared output.
    */
  case object Unread extends Accounted

  /** Each distinct provenance of the result's items, in the order first read, at most [[Most]];
    * none for a result holding no item.
    */
  final case class Items private[visibility] (items: Vector[Provenance]) extends Accounted

  /** The result could not be read whole, or held more than [[Most]] distinct provenances: it is
    * believed at [[Integrity.Untrusted]].
    */
  case object Unaccountable extends Accounted

  /** The most distinct provenances one result carries: 32. */
  val Most: Int = 32

  /** `items`' distinct provenances, in the order first read; [[Unaccountable]] past [[Most]]. */
  def of(items: Vector[Provenance]): Accounted = {
    val distinct = items.distinct
    if (distinct.size > Most) Unaccountable else Items(distinct)
  }
}

/** How a service's extension accounts for the items of its results (ADR 0036), `R` one result
  * as that extension holds it. Pure: it reads only the result it is given.
  */
trait Accounts[-R] extends caps.Pure {

  /** `result`'s items ([[Accounted.of]]); [[Accounted.Unread]] when this reads no result of its
    * tool; [[Accounted.Unaccountable]] when it reads them but cannot read this one.
    */
  def items(result: R): Accounted
}

object Accounts {

  /** Reads no result: every one [[Accounted.Unread]]. */
  def none[R]: Accounts[R] = new Accounts[R] {
    def items(result: R): Accounted = Accounted.Unread
  }
}
