package grit.core

/** Append-only entry store. Entries are never rewritten or deleted. */
trait EntryStore {

  /** Appends `entry`, rejecting an existing id with `DuplicateId`.
    * Never overwrites.
    */
  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit]

  /** The entry with `id`, or `None` when no such entry exists. */
  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]]

  /** Every entry, ascending by `seq`, ties broken by `id`. */
  def listAll()(using Tx^): Either[StoreError, Vector[Entry]]
}
