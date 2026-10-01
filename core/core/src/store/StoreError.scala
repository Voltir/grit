package grit.core.store

import grit.core.id.EntryId

/** A failure a store caller is expected to handle.
  *
  * Absence is not an error — a missing entry comes back as `Right(None)`.
  */
enum StoreError {

  /** An entry with this id already exists. The store never overwrites. */
  case DuplicateId(id: EntryId)

  /** The database rejected or could not complete the statement. */
  case DatabaseError(cause: String)

  /** A stored value this code does not accept, such as settings changed by hand to break
    * their rules; `cause` says which and why.
    */
  case Invalid(cause: String)
}
