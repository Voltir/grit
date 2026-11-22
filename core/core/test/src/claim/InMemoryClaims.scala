package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, DocKey, PluginName, SessionId}
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

  /** `plugin`'s claims, each holder's sessions last working at `worked` of it, and each session
    * working as the holder `sessions` gives it, last at its time, as the store's sessions this
    * transaction reads would say.
    */
  def of(
      plugin: PluginName,
      worked: ClaimHolder -> Option[Instant],
      sessions: SessionId -> Option[(ClaimHolder, Instant)] = _ => None
  ): Claims = new Claims {

    /** `r`'s claim, active as its session's last event says, or while it has none, `worked`. */
    private def read(r: Row): Claim = {
      val c = r.claim
      val last = c.session.fold(worked(c.holder))(s => sessions(s).map(_._2))
      c.copy(active = last.filter(_.isAfter(c.made)).getOrElse(c.made))
    }

    private def lapsed(r: Row, at: Instant): Boolean = !at.isBefore(read(r).lapses)

    def claim(
        subject: DocKey,
        holder: ClaimHolder,
        role: ClaimRole,
        session: Option[SessionId],
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
      val elsewhere = session.flatMap(s => sessions(s).map(_._1).filter(_ != holder).map((s, _)))
      (
        elsewhere,
        atLabel.find(_.claim.subject == subject),
        atLabel.find(_.claim.holder == holder)
      ) match {
        case (Some((s, other)), _, _) => Right(Claimed.Elsewhere(s, other))
        case (None, Some(mine), _)
            if mine.claim.holder == holder && reads(mine) &&
              session.exists(s => mine.claim.session.exists(_ != s)) =>
          Right(Claimed.HeldBy(read(mine)))
        case (None, Some(mine), _) if mine.claim.holder == holder && reads(mine) =>
          val renewed =
            mine.copy(claim =
              mine.claim.copy(role = role, session = mine.claim.session.orElse(session))
            )
          rows = rows.map(r => if (r == mine) renewed else r)
          Right(Claimed.Made(read(renewed)))
        case (None, Some(first), _) => Right(Claimed.Taken(shown(first)))
        case (None, None, Some(other)) => Right(Claimed.Holding(shown(other)))
        case (None, None, None) =>
          val made =
            Row(
              plugin,
              Claim(subject, holder, role, by, at, kept, session, at),
              clearance.own.map(_.room)
            )
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

    def moved(
        claim: Claim,
        holder: ClaimHolder,
        session: SessionId,
        role: ClaimRole,
        at: Instant
    )(using Tx^): Either[StoreError, ClaimMoved] = {
      def here(r: Row): Boolean = r.plugin == plugin && r.claim.label == claim.label
      rows = rows.filterNot(r =>
        here(r) && r.claim.holder == holder && r.claim.subject != claim.subject && lapsed(r, at)
      )
      val mine = rows.find(r =>
        here(r) && r.claim.subject == claim.subject && r.claim.holder == claim.holder &&
          r.claim.made == claim.made && reads(r)
      )
      val other =
        rows.find(r => here(r) && r.claim.holder == holder && r.claim.subject != claim.subject)
      (sessions(session).map(_._1).filter(_ != holder), mine, other) match {
        case (Some(away), _, _) => Right(ClaimMoved.Elsewhere(session, away))
        case (None, None, _) => Right(ClaimMoved.Gone)
        case (None, Some(_), Some(o)) => Right(ClaimMoved.Holding(Option.when(reads(o))(read(o))))
        case (None, Some(m), None) =>
          val now = m.copy(claim =
            m.claim.copy(holder = holder, role = role, session = Some(session), made = at)
          )
          rows = rows.map(r => if (r == m) now else r)
          Right(ClaimMoved.Made(read(now)))
      }
    }

    def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] =
      Right(
        rows
          .filter(r => r.plugin == plugin && reads(r) && !lapsed(r, at))
          .map(read)
          .sortBy(c => (DocKey.value(c.subject), c.made))
      )

    // Nothing here runs at once, so there is nothing to lock.
    def held(claim: Claim)(using Tx^): Either[StoreError, Option[Claim]] =
      Right(
        rows
          .find(r =>
            r.plugin == plugin && r.claim.subject == claim.subject &&
              r.claim.holder == claim.holder && r.claim.made == claim.made &&
              r.claim.label == claim.label && reads(r)
          )
          .map(read)
      )

    // Single-threaded: no other transaction runs to hold it.
    def on(subject: DocKey, at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] =
      Right(
        rows
          .filter(r =>
            r.plugin == plugin && r.claim.subject == subject && reads(r) && !lapsed(r, at)
          )
          .map(read)
          .sortBy(c => (c.made, ClaimHolder.value(c.holder)))
      )

    def lock(subject: DocKey)(using Tx^): Either[StoreError, Unit] = Right(())
  }

  private def reads(r: Row)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.claim.label)
}

object InMemoryClaims {
  private final case class Row(plugin: PluginName, claim: Claim, room: Option[Place])
}
