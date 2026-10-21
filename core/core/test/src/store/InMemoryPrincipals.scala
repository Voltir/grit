package grit.core.store

import java.time.ZoneId

import grit.core.id.{EntryId, Href}
import grit.core.identity.Account

/** An in-memory [[Principals]] for tests, keeping [[PrincipalsContract]]. It ignores the `Tx`.
  * Who wrote an inbound entry is told to it ([[authored]]), as SqlInbox writes it beside the
  * entry.
  */
final class InMemoryPrincipals extends Principals {

  // Replaced whole by the store's own methods, which the test calls on its one thread.
  @caps.unsafe.untrackedCaptures
  private var names = Map.empty[Account, String]

  @caps.unsafe.untrackedCaptures
  private var zones = Map.empty[Account, ZoneId]

  @caps.unsafe.untrackedCaptures
  private var authors = Map.empty[EntryId, Account]

  @caps.unsafe.untrackedCaptures
  private var links = Map.empty[EntryId, Href]

  /** Records that `by` wrote the inbound entry `entry`, recorded with no link. */
  def authored(entry: EntryId, by: Account): Unit = authored(entry, by, None)

  /** Records that `by` wrote the inbound entry `entry`, recorded with `link`. */
  def authored(entry: EntryId, by: Account, link: Option[Href]): Unit = {
    authors = authors.updated(entry, by)
    link.foreach(l => links = links.updated(entry, l))
  }

  /** Who [[authored]] `entry`, if anyone. */
  def author(entry: EntryId): Option[Account] = authors.get(entry)

  def name(account: Account, name: String)(using Tx^): Either[StoreError, Unit] =
    Principals.refusal(account, name) match {
      case Some(why) => Left(why)
      case None =>
        names = names.updated(account, name.trim)
        Right(())
    }

  def zoned(account: Account, zone: ZoneId)(using Tx^): Either[StoreError, Unit] =
    Principals.refusal(account) match {
      case Some(why) => Left(why)
      case None =>
        zones = zones.updated(account, zone)
        Right(())
    }

  def zoneOf(entry: EntryId)(using Tx^): Either[StoreError, Option[ZoneId]] =
    Right(authors.get(entry).flatMap(zones.get))

  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers] =
    Right(
      Speakers(
        entries.flatMap(e => authors.get(e).flatMap(names.get).map(e -> _)).toMap,
        entries.flatMap(e => links.get(e).map(e -> _)).toMap
      )
    )
}
