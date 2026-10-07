package grit.core.store

import grit.core.visibility.{Clearance, Label, Visibility}

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
}
