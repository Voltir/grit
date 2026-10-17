package grit.core.visibility

import grit.core.identity.Email
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

/** Who may read one item of a service's result. */
enum Readers derives CanEqual {

  /** Whoever reads what its item's place contains ([[grit.core.store.Tx.readsFrom]]). */
  case AtPlace

  /** Only its originator: read only in the direct message of the person holding `email`, as
    * the opener claims it for them ([[Clearance.readsOriginated]]), whatever else a reader is
    * cleared for; by no one when no person holds it.
    */
  case Originator(email: Email)
}

/** One item of a service's result as its extension cut it: `item`, where it is kept and what
  * its author is vouched at; `readers`, who may read it; `text`, what the model reads of it;
  * `facts`, what `text` is rendered from, withheld with it.
  */
final case class ResultPart(
    item: Provenance,
    readers: Readers,
    text: String,
    facts: Option[Facts] = None
) derives CanEqual

/** What a service's extension read of one result. */
enum Itemised derives CanEqual {

  /** Shown whole, as every reader of its service reads it, its items `items`. */
  case Whole(items: Accounted)

  /** `head`, read by every reader of its service, then each of `parts` read only by those its
    * readers admit; `facts`, what `head` is rendered from, read as it is.
    */
  case Parts(head: String, parts: Vector[ResultPart], facts: Option[Facts] = None)

  /** Read as its tool's but not placed: nothing of it is shown, saying `why` in words the model
    * reads.
    */
  case Unplaced(why: String)
}

/** How a service's extension cuts and accounts for the items of its results (ADR 0036), `R`
  * one result as that extension holds it. Pure: it reads only the result it is given.
  */
trait Accounts[-R] extends caps.Pure {

  /** `result` as this extension reads it: [[Itemised.Whole]] of [[Accounted.Unread]] when it
    * reads no result of its tool. A whole result's items are [[Accounted.of]] them, and
    * [[Accounted.Unaccountable]] when it reads results of its tool but cannot read this one.
    */
  def items(result: R): Itemised
}

object Accounts {

  /** Reads no result: every one whole and [[Accounted.Unread]]. */
  def none[R]: Accounts[R] = new Accounts[R] {
    def items(result: R): Itemised = Itemised.Whole(Accounted.Unread)
  }
}
