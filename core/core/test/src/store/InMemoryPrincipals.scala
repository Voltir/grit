package grit.core.store

import grit.core.id.{EntryId, PrincipalId}

/** An in-memory [[Principals]] for tests, keeping [[PrincipalsContract]]. It ignores the `Tx`.
  * Who wrote an inbound entry is told to it ([[authored]]), as SqlInbox writes it beside the
  * entry.
  */
final class InMemoryPrincipals extends Principals {

  @caps.unsafe.untrackedCaptures
  private var names = Map.empty[PrincipalId, String]

  @caps.unsafe.untrackedCaptures
  private var authors = Map.empty[EntryId, PrincipalId]

  /** Records that `by` wrote the inbound entry `entry`. */
  def authored(entry: EntryId, by: PrincipalId): Unit = authors = authors.updated(entry, by)

  /** Who [[authored]] `entry`, if anyone. */
  def author(entry: EntryId): Option[PrincipalId] = authors.get(entry)

  def enroll(id: PrincipalId, name: String)(using Tx^): Either[StoreError, Unit] =
    Principals.refusal(id, name) match {
      case Some(why) => Left(why)
      case None =>
        names = names.updated(id, name.trim)
        Right(())
    }

  @caps.unsafe.untrackedCaptures
  private var assistants = Map.empty[PrincipalId, String]

  def enrollAssistant(id: PrincipalId, name: String)(using Tx^): Either[StoreError, Unit] =
    Principals.refusal(id, name) match {
      case Some(why) => Left(why)
      case None =>
        assistants = assistants.updated(id, name.trim)
        Right(())
    }

  def name(id: PrincipalId)(using Tx^): Either[StoreError, Option[String]] =
    Right(names.get(id).orElse(assistants.get(id)))

  def speakers(entries: Vector[EntryId])(using Tx^): Either[StoreError, Speakers] =
    Right(Speakers(entries.flatMap(e => authors.get(e).flatMap(names.get).map(e -> _)).toMap))
}
