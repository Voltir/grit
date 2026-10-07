package grit.core.store

import grit.core.id.EntryId
import grit.core.identity.Account

/** An in-memory [[Principals]] for tests, keeping [[PrincipalsContract]]. It ignores the `Tx`.
  * Who wrote an inbound entry is told to it ([[authored]]), as SqlInbox writes it beside the
  * entry.
  */
final class InMemoryPrincipals extends Principals {

  @caps.unsafe.untrackedCaptures
  private var names = Map.empty[Account, String]

  @caps.unsafe.untrackedCaptures
  private var authors = Map.empty[EntryId, Account]

  /** Records that `by` wrote the inbound entry `entry`. */
  def authored(entry: EntryId, by: Account): Unit = authors = authors.updated(entry, by)

  /** Who [[authored]] `entry`, if anyone. */
  def author(entry: EntryId): Option[Account] = authors.get(entry)

  def name(account: Account, name: String)(using Tx^): Either[StoreError, Unit] =
    Principals.refusal(account, name) match {
      case Some(why) => Left(why)
      case None =>
        names = names.updated(account, name.trim)
        Right(())
    }

  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers] =
    Right(Speakers(entries.flatMap(e => authors.get(e).flatMap(names.get).map(e -> _)).toMap))
}
