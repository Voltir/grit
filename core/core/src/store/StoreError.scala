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

  /** A write past its plugin's `bound` current documents, under terms that refuse past it
    * ([[grit.core.document.DocumentTerms.Full.Refuse]]); nothing was written.
    */
  case Full(bound: Int)

  /** What a person or a log is told of this error. A workflow step records it in its output,
    * so changing a wording changes that step's encoding (ADR 0004).
    */
  def described: String = this match {
    case DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case DatabaseError(cause) => cause
    case Invalid(cause) => cause
    case Full(bound) =>
      s"its plugin already holds the $bound documents it may, so nothing was kept"
  }

  /** This error's case alone, for a record that must not keep what [[described]] can quote of
    * what the store was given.
    */
  def kind: String = this match {
    case DuplicateId(_) => "duplicate id"
    case DatabaseError(_) => "database error"
    case Invalid(_) => "invalid"
    case Full(_) => "full"
  }
}
