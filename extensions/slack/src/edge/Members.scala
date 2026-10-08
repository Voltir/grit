package grit.slack.edge

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

import grit.core.edge.{Joins, Membership}
import grit.core.identity.Account
import grit.core.place.{Namespace, Place}
import grit.core.store.{Origin, StoreError}
import grit.core.visibility.RoomAccess
import grit.slack.client.ChannelKind
import grit.slack.event.{ChannelId, TeamId}

/** The channels of `team` grit's bot is a member of, as `joins` has recorded them (ADR 0033):
  * what the edge hears. It is changed only by what `joins` returns for a join, a leave or a
  * reconcile, never by an event itself, so a join reported after a later leave changes nothing.
  */
private[slack] final class Members(joins: Joins, team: TeamId) {

  // Written only from what `joins` returns, through the map's own atomic operations. A reader
  // racing a writer hears or skips the one message at a join's or leave's edge, which Slack's
  // own delivery bounds; `joins` is the truth, and the next open rebuilds this from it.
  @caps.unsafe.untrackedCaptures
  private val in = new ConcurrentHashMap[ChannelId, RoomAccess]()

  /** Every room of `team`, `slack:{team}`. */
  private val under: Place = Place.under(Namespace.Slack, Vector(TeamId.value(team)))

  /** Whether grit's bot is a member of `channel`, as last recorded. */
  def contains(channel: ChannelId): Boolean = in.containsKey(channel)

  /** Every channel grit's bot is a member of, by id. */
  def all: Vector[ChannelId] = in.keySet.asScala.toVector.sortBy(ChannelId.value)

  /** Records grit's bot joined `channel`, whose access is `access`, at `at`, invited by
    * `inviter` when named ([[Joins.joined]]); what is recorded after.
    */
  def joined(
      channel: ChannelId,
      access: RoomAccess,
      inviter: Option[Account],
      at: Instant
  ): Either[StoreError, Membership] =
    joins.joined(room(channel), access, inviter, at).map(kept(channel, access, _))

  /** Records grit's bot left `channel` at `at` ([[Joins.left]]); what is recorded after. */
  def left(channel: ChannelId, at: Instant): Either[StoreError, Membership] =
    joins.left(room(channel), at).map { m =>
      m match {
        case Membership.Member(_, _) => ()
        case Membership.Gone => val _ = in.remove(channel)
      }
      m
    }

  /** Records, as of `now`, grit's bot a member of each of `listed` (every channel Slack lists it
    * in, with its access; a join with no inviter named) and gone from every other room of
    * `team` recorded a member, forgets the rooms left at least [[Joins.KeptLeft]] before `now`,
    * and holds what is then recorded; how many rooms were forgotten.
    */
  def reconcile(listed: Vector[(ChannelId, RoomAccess)], now: Instant): Either[StoreError, Int] =
    for {
      recorded <- joins.members(under)
      _ <- listed.foldLeft[Either[StoreError, Unit]](Right(())) { case (done, (c, access)) =>
        done.flatMap(_ => joined(c, access, None, now).map(_ => ()))
      }
      named = listed.map(_._1).toSet
      _ <- recorded.foldLeft[Either[StoreError, Unit]](Right(())) { case (done, (place, _)) =>
        done.flatMap(_ =>
          channelOf(place).filterNot(named.contains) match {
            case Some(c) => left(c, now).map(_ => ())
            case None => Right(())
          }
        )
      }
      forgotten <- joins.forget(under, now.minus(Joins.KeptLeft))
    } yield forgotten

  /** `channel`'s room, `slack:{team}/{channel}`. */
  private def room(channel: ChannelId): Place =
    Origin.channel(TeamId.value(team), ChannelId.value(channel))

  /** The channel `place` is the room of, when it is one of `team`'s. */
  private def channelOf(place: Place): Option[ChannelId] = place.segments match {
    case Vector(_, t, c) if place == Origin.channel(t, c) && t == TeamId.value(team) =>
      Some(ChannelId(c))
    case _ => None
  }

  /** `m`, held as `channel`'s membership with `access`. */
  private def kept(channel: ChannelId, access: RoomAccess, m: Membership): Membership = {
    m match {
      case Membership.Member(_, _) => val _ = in.put(channel, access)
      case Membership.Gone => val _ = in.remove(channel)
    }
    m
  }
}

private[slack] object Members {

  /** The access of a channel of `kind`; `None` for one grit's bot is not in. */
  def access(kind: ChannelKind): Option[RoomAccess] = kind match {
    case ChannelKind.Public => Some(RoomAccess.Open)
    case ChannelKind.Private => Some(RoomAccess.Invited)
    case ChannelKind.Unseen => None
  }
}
