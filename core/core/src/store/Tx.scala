package grit.core.store

import grit.core.place.{Place, Service}
import grit.core.visibility.{Clearance, Item, Label, Labelled, Room, Visibility}

/** Capability to read and write inside one database transaction, opened for a
  * [[grit.core.visibility.Subject]]: what it reads of labelled rows, and the least label it
  * writes at, are its [[Tx.clearance]]. Obtained from an opener; valid only within its
  * callback, which the type system enforces.
  */
opaque type Tx = Tx.Opened

object Tx {

  /* A class, so `Tx^{c}` keeps the connection's capture set; the clearance and the visibility
   * are pure, so they add nothing to it. */
  private[store] final class Opened(
      val connection: java.sql.Connection^,
      val clearance: Clearance,
      val visibility: Visibility
  )

  /** `c`'s transaction, read and written at `clearance`, labelling places as `visibility`, the
    * deployment's, does: the one way to make a `Tx`.
    */
  private[grit] def open(
      c: java.sql.Connection^,
      clearance: Clearance,
      visibility: Visibility
  ): Tx^{c} =
    new Opened(c, clearance, visibility)

  private[grit] def connection(tx: Tx^): java.sql.Connection^{tx} = tx.connection

  /** The clearance it was opened at: what stores filter and floor by. */
  def clearance(tx: Tx^): Clearance = tx.clearance

  /** What it reads of documents and plugins' data: every label this dominates. For choosing
    * which variant of derived data to serve, never for enforcing: a read of a variant above it
    * returns nothing, so a wrong choice cannot read up. Under posting and a job's run it is
    * also the floor what is written is kept at.
    */
  def cleared(tx: Tx^): Label = tx.clearance.everywhere

  /** The least label it writes at: its clearance's floor, its own room's label when it has
    * one. What a row made from its own room's content is kept at.
    */
  def floor(tx: Tx^): Label = tx.clearance.floor

  /** The label `to` is written at outside grit: the label the deployment's rooms' labeller maps
    * it to as a room whose access is not reported, when it maps it explicitly with only declared
    * compartments; `None` otherwise, and then nothing writes to it. `None` for a direct
    * message's room ([[Place.direct]]): only its own turns write there.
    */
  def writable(to: Place)(using tx: Tx^): Option[Label] =
    if (to.direct) None
    else
      tx.visibility.rooms.label(Room(to, None)) match {
        case Labelled.Mapped(l) if tx.visibility.compartments.admit(l) == l => Some(l)
        case _ => None
      }

  /** Whether what this transaction knows may be written to `to` outside grit: `to` is
    * [[writable]] and its label dominates [[floor]] (ADR 0030, no write down).
    */
  def writesTo(to: Place)(using tx: Tx^): Boolean =
    writable(to).exists(_.dominates(floor(tx)))

  /** Whether what `source` contains may be read in this transaction: as anything recorded in a
    * room at `source` is read ([[Clearance.reads]]), at the label the deployment gives it, an
    * unplaced one at [[grit.core.visibility.Compartment.Unmapped]] (no read up). A
    * transaction's own room is read up to its own label; any other place up to `everywhere`.
    */
  def readsFrom(source: Place)(using tx: Tx^): Boolean =
    tx.clearance.reads(Item.InRoom(source), tx.visibility.roomLabel(source))

  /** Whether what this transaction knows may be sent to `service` as a call's arguments: what
    * the deployment trusts `service` with ([[Visibility.trusted]], public when it declares no
    * trust) dominates [[floor]]. What the call returns is checked by [[readsFrom]] of its place.
    */
  def sendsTo(service: Service)(using tx: Tx^): Boolean =
    tx.visibility.trusted(service).dominates(floor(tx))
}
