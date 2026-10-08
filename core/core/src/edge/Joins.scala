package grit.core.edge

import java.time.{Duration, Instant}

import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.StoreError
import grit.core.visibility.RoomAccess

/** Whether an edge's bot is a member of a room, as [[Joins]] has recorded it. */
enum Membership {

  /** A member since `joined`; `backfill` is that join when what was said before it is not yet
    * heard (nor skipped).
    */
  case Member(joined: Instant, backfill: Option[Instant])

  /** Not a member. */
  case Gone
}

/** The rooms an edge's bot is a member of, as the edge reports them (ADR 0033), ordered by when
  * each join or leave happened, not by when it is reported: an edge updates what it holds from
  * what these return, never from the event. A join and a leave at the same instant leave it
  * [[Membership.Gone]].
  */
trait Joins extends caps.SharedCapability {

  /** Records `room`'s access as `access` and, unless a leave as late as `at` or later is
    * recorded, the bot a member as of `at` when it was not one (a repeat while a member changes
    * nothing). A new join invited by `inviter`, when named, whom no trusted realm vouches a full
    * member (as linked now; attest them first, [[Attesting.before]]) is recorded with its
    * backfill skipped; with no inviter named it is not. What is recorded after.
    */
  def joined(
      room: Place,
      access: RoomAccess,
      inviter: Option[Account],
      at: Instant
  ): Either[StoreError, Membership]

  /** Records the bot no longer a member of `room` as of `at`, unless its join is later than
    * `at`; a leave of a room never joined is recorded too, so a join reported after it but
    * made before it changes nothing. What is recorded after.
    */
  def left(room: Place, at: Instant): Either[StoreError, Membership]

  /** Every room within `under` the bot is recorded a member of. */
  def members(under: Place): Either[StoreError, Vector[(Place, Membership.Member)]]

  /** Forgets every room within `under` the bot left before `before` and has not joined since,
    * unless a person set its label or made it quiet (that decision outlives a re-invite); how
    * many. A join of a forgotten room reported after this is recorded however long ago it was
    * made, so an edge forgets only rooms left at least [[Joins.KeptLeft]] ago.
    */
  def forget(under: Place, before: Instant): Either[StoreError, Int]
}

object Joins {

  /** How long a room the bot left is kept before an edge forgets it ([[Joins.forget]]): a day,
    * so a join reported late is still ordered against the leave.
    */
  val KeptLeft: Duration = Duration.ofDays(1)

  /** Whether a new join is recorded with its backfill skipped: when `inviter` is named
    * (`Some`) and whom its account is linked to now (`None` for one never seen) is no person a
    * trusted realm vouches a full member.
    */
  private[grit] def skips(inviter: Option[Option[Principal]]): Boolean =
    inviter.exists(whom => !whom.exists(_.vouched))

  /** What is kept of a room's membership: when the bot last joined and last left, and the join
    * whose backfill is done or skipped. The one definition of how joins and leaves are ordered,
    * for every [[Joins]].
    */
  private[grit] final case class Kept(
      joined: Option[Instant],
      left: Option[Instant],
      backfilled: Option[Instant]
  ) {

    /** The membership this is: a member when it joined after it last left. */
    def membership: Membership = joined match {
      case Some(j) if !left.exists(l => !l.isBefore(j)) =>
        Membership.Member(j, Option.when(!backfilled.contains(j))(j))
      case _ => Membership.Gone
    }

    /** A join at `at`, as [[Joins.joined]] records one, its backfill skipped when `skip`. */
    def join(at: Instant, skip: Boolean): Kept =
      if (left.exists(l => !l.isBefore(at))) this
      else
        membership match {
          case Membership.Member(_, _) => this
          case Membership.Gone =>
            Kept(Some(at), left, if (skip) Some(at) else backfilled)
        }

    /** A leave at `at`, as [[Joins.left]] records one. */
    def leave(at: Instant): Kept =
      if (joined.exists(_.isAfter(at)) || left.exists(_.isAfter(at))) this
      else copy(left = Some(at))

    /** Whether [[Joins.forget]] forgets it, before `before`, when no person `decided` its label
      * or quiet.
      */
    def forgotten(before: Instant, decided: Boolean): Boolean =
      !decided && membership == Membership.Gone && left.exists(_.isBefore(before))
  }

  /** Nothing kept: a room never joined or left. */
  private[grit] val Never: Kept = Kept(None, None, None)
}
