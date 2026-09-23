package grit.core

/** A failure a store caller is expected to handle.
  *
  * Absence is not an error — a missing entry comes back as `Right(None)`.
  */
enum StoreError {

  /** An entry with this id already exists. The store never overwrites. */
  case DuplicateId(id: EntryId)

  /** The database rejected or could not complete the statement. */
  case DatabaseError(cause: String)
}
