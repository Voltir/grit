package grit.core.claim

import java.time.Instant

import grit.core.id.{DocKey, PluginName}
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

  /** `plugin`'s claims. */
  def of(plugin: PluginName): Claims = new Claims {
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
      val atLabel = rows.filter(r => r.plugin == plugin && r.claim.label == kept)
      def shown(r: Row): Option[Claim] = Option.when(reads(r))(r.claim)
      (atLabel.find(_.claim.subject == subject), atLabel.find(_.claim.holder == holder)) match {
        case (Some(first), _) => Right(Claimed.Taken(shown(first)))
        case (None, Some(other)) => Right(Claimed.Holding(shown(other)))
        case (None, None) =>
          val made = Claim(subject, holder, role, by, at, kept)
          rows = rows :+ Row(plugin, made, clearance.own.map(_.room))
          Right(Claimed.Made(made))
      }
    }

    def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]] =
      rows.find(r =>
        r.plugin == plugin && r.claim.subject == subject && r.claim.label == label && reads(r)
      ) match {
        case Some(held) =>
          rows = rows.filterNot(_ == held)
          Right(Some(held.claim))
        case None => Right(None)
      }
  }

  private def reads(r: Row)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.claim.label)
}

object InMemoryClaims {
  private final case class Row(plugin: PluginName, claim: Claim, room: Option[Place])
}
