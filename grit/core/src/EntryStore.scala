package grit.core

/** Append-only entry store. Entries are never rewritten or deleted. */
trait EntryStore {

  /** Appends `entry`, rejecting an existing id with `DuplicateId`.
    * Never overwrites.
    */
  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit]

  /** The entry with `id`, or `None` when no such entry exists. */
  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]]

  /** Every entry in `conversation`, ascending by `seq`. Empty for an unknown conversation. */
  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]]
}
