package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, DocKey, PluginName}
import grit.core.identity.Principal
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Item, Label}

/** In-memory claims, for tests, keeping [[ClaimsContract]]. It floors, places and filters by the
  * `Tx`'s clearance, and otherwise ignores it: nothing is rolled back.
  */
final class InMemoryClaims {
  import InMemoryClaims.Row

  // Only ever replaced by a new immutable vector, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Vector.empty[Row]

  /** `plugin`'s claims, each holder's sessions last working at `worked` of it, as the store's
    * sessions this transaction reads would say.
    */
  def of(plugin: PluginName, worked: ClaimHolder -> Option[Instant]): Claims = new Claims {

    /** `r`'s claim, active as `worked` says. */
    private def read(r: Row): Claim = {
      val c = r.claim
      c.copy(active = worked(c.holder).filter(_.isAfter(c.made)).getOrElse(c.made))
    }

    private def lapsed(r: Row, at: Instant): Boolean = !at.isBefore(read(r).lapses)

    def claim(
        subject: DocKey,
        holder: ClaimHolder,
        role: ClaimRole,
        by: Principal.Person,
        label: Label,
        at: Instant
    )(using tx: Tx^): Either[StoreError, Claimed] = {
      val clearance = Tx.clearance(tx)
      val kept = label.join(clearance.floor)
      def here(r: Row): Boolean = r.plugin == plugin && r.claim.label == kept
      rows = rows.filterNot(r =>
        here(r) && (r.claim.subject == subject || r.claim.holder == holder) && lapsed(r, at)
      )
      val atLabel = rows.filter(here)
      def shown(r: Row): Option[Claim] = Option.when(reads(r))(read(r))
      (atLabel.find(_.claim.subject == subject), atLabel.find(_.claim.holder == holder)) match {
        case (Some(mine), _) if mine.claim.holder == holder && reads(mine) =>
          val renewed = mine.copy(claim = mine.claim.copy(role = role))
          rows = rows.map(r => if (r == mine) renewed else r)
          Right(Claimed.Made(read(renewed)))
        case (Some(first), _) => Right(Claimed.Taken(shown(first)))
        case (None, Some(other)) => Right(Claimed.Holding(shown(other)))
        case (None, None) =>
          val made =
            Row(plugin, Claim(subject, holder, role, by, at, kept, at), clearance.own.map(_.room))
          rows = rows :+ made
          Right(Claimed.Made(read(made)))
      }
    }

    def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]] =
      rows.find(r =>
        r.plugin == plugin && r.claim.subject == subject && r.claim.label == label && reads(r)
      ) match {
        case Some(held) =>
          rows = rows.filterNot(_ == held)
          Right(Some(read(held)))
        case None => Right(None)
      }

    def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] =
      Right(
        rows
          .filter(r => r.plugin == plugin && reads(r) && !lapsed(r, at))
          .map(read)
          .sortBy(c => (DocKey.value(c.subject), c.made))
      )
  }

  private def reads(r: Row)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.claim.label)
}

object InMemoryClaims {
  private final case class Row(plugin: PluginName, claim: Claim, room: Option[Place])
}
