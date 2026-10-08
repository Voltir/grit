package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.util.Using

import grit.core.edge.{Joins, Membership}
import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.{RoomAccess, Subject}

/** [[Joins]] over Postgres: each call one transaction written through `jot`, its rooms' rows in
  * `grit.rooms` (a room never heard gets its place first, [[SqlPlaces.id]]), each row locked
  * while its join or leave is ordered by [[Joins.Kept]]. Memberships carry no label, so it opens
  * for [[Subject.Public]].
  */
private[dbos] final class SqlJoins(jot: Jot) extends Joins {
  import SqlEntryStore.attempt

  def joined(
      room: Place,
      access: RoomAccess,
      inviter: Option[Account],
      at: Instant
  ): Either[StoreError, Membership] =
    jot.write(Subject.Public) {
      for {
        whom <- inviter.fold[Either[StoreError, Option[Option[Principal]]]](Right(None))(a =>
          SqlIdentities.person(a).map(Some(_))
        )
        place <- SqlPlaces.id(room)
        was <- locked(place)
        now = was.join(at, Joins.skips(whom))
        _ <- kept(place, now, Some(access))
      } yield now.membership
    }

  def left(room: Place, at: Instant): Either[StoreError, Membership] =
    jot.write(Subject.Public) {
      for {
        place <- SqlPlaces.id(room)
        was <- locked(place)
        now = was.leave(at)
        _ <- kept(place, now, None)
      } yield now.membership
    }

  def backfilled(room: Place, join: Instant): Either[StoreError, Unit] =
    jot.write(Subject.Public) {
      found(room).flatMap {
        case None => Right(())
        case Some((place, was)) => kept(place, was.backfilledAt(join), None)
      }
    }

  def backfilledSince(under: Place, since: Instant): Either[StoreError, Int] =
    jot.write(Subject.Public) {
      rows("r.backfilled_join IS NOT NULL", lock = false).map(_.count { case Row(_, room, k, _) =>
        room.within(under) && k.backfilledSince(since)
      })
    }

  def members(under: Place): Either[StoreError, Vector[(Place, Membership.Member)]] =
    jot.write(Subject.Public) {
      rows("r.joined_at IS NOT NULL", lock = false).map(_.collect {
        case Row(_, room, k, _) if room.within(under) =>
          k.membership match {
            case m: Membership.Member => Some(room -> m)
            case Membership.Gone => None
          }
      }.flatten.sortBy(_._1.written))
    }

  def forget(under: Place, before: Instant): Either[StoreError, Int] =
    jot.write(Subject.Public) {
      rows("r.left_at IS NOT NULL", lock = true).flatMap { all =>
        val gone =
          all.collect {
            case Row(id, room, k, decided) if room.within(under) && k.forgotten(before, decided) =>
              id
          }
        if (gone.isEmpty) Right(0)
        else
          update(
            """DELETE FROM grit.rooms
              | WHERE place_id IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid)""".stripMargin
          )(_.setString(1, ujson.Arr.from(gone.map(ujson.Str(_))).render()))
      }
    }

  /** A room's row as read: its place's id and the place, its membership, and whether a person
    * set its label or made it quiet.
    */
  private final case class Row(id: String, room: Place, kept: Joins.Kept, decided: Boolean)

  /** Every `grit.rooms` row where `where` holds of it (aliased `r`), locked when `lock`. */
  private def rows(where: String, lock: Boolean)(using tx: Tx^): Either[StoreError, Vector[Row]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""SELECT r.place_id::text, array_to_json(p.path)::text, r.joined_at, r.left_at,
             |       r.backfilled_join, r.label_id IS NOT NULL OR r.quiet AS decided
             |  FROM grit.rooms r JOIN grit.places p ON p.id = r.place_id
             | WHERE $where${if (lock) " FOR UPDATE OF r" else ""}""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val read = Vector.newBuilder[(String, String, Joins.Kept, Boolean)]
          while (rs.next())
            read += ((rs.getString(1), rs.getString(2), keptOf(rs, 3), rs.getBoolean(6)))
          read.result()
        }
      }
    }.flatMap(_.foldLeft[Either[StoreError, Vector[Row]]](Right(Vector.empty)) {
      case (acc, (id, path, k, decided)) =>
        acc.flatMap(done =>
          SqlPlaces
            .read(path)
            .left
            .map(why => StoreError.Invalid(s"a room's place: $why"))
            .map(room => done :+ Row(id, room, k, decided))
        )
    })
  }

  /** `room`'s place id and the membership kept for it, locked until the transaction ends;
    * `None`, making nothing, when no row is kept for it.
    */
  private def found(
      room: Place
  )(using tx: Tx^): Either[StoreError, Option[(String, Joins.Kept)]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT r.place_id::text, r.joined_at, r.left_at, r.backfilled_join
            |  FROM grit.rooms r JOIN grit.places p ON p.id = r.place_id
            | WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
            |   FOR UPDATE OF r""".stripMargin
        )
      ) { ps =>
        ps.setString(1, SqlPlaces.path(room))
        Using.resource(ps.executeQuery())(rs =>
          Option.when(rs.next())((rs.getString(1), keptOf(rs, 2)))
        )
      }
    }
  }

  /** The membership kept for the place `place`, its row made first when there is none, and
    * locked until the transaction ends.
    */
  private def locked(place: String)(using tx: Tx^): Either[StoreError, Joins.Kept] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.rooms (place_id) VALUES (?::uuid) ON CONFLICT (place_id) DO NOTHING"
        )
      ) { ps =>
        ps.setString(1, place)
        ps.executeUpdate()
      }
      Using.resource(
        conn.prepareStatement(
          """SELECT joined_at, left_at, backfilled_join FROM grit.rooms
            | WHERE place_id = ?::uuid FOR UPDATE""".stripMargin
        )
      ) { ps =>
        ps.setString(1, place)
        Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(keptOf(rs, 1)))
      }
    }.flatMap(_.toRight(StoreError.Invalid(s"the room at place $place was not recorded")))
  }

  /** Writes `k` as `place`'s membership, and `access` as its access when given. */
  private def kept(place: String, k: Joins.Kept, access: Option[RoomAccess])(using
      Tx^
  ): Either[StoreError, Unit] =
    update(
      """UPDATE grit.rooms
        |   SET joined_at = ?, left_at = ?, backfilled_join = ?, access = COALESCE(?, access)
        | WHERE place_id = ?::uuid""".stripMargin
    ) { ps =>
      ps.setObject(1, k.joined.map(utc).orNull)
      ps.setObject(2, k.left.map(utc).orNull)
      ps.setObject(3, k.backfilled.map(utc).orNull)
      ps.setString(
        4,
        access.map {
          case RoomAccess.Open => "open"
          case RoomAccess.Invited => "invited"
        }.orNull
      )
      ps.setString(5, place)
    }.map(_ => ())

  private def utc(at: Instant): OffsetDateTime = at.atOffset(ZoneOffset.UTC)

  /** The three membership columns from `first` on, as [[Joins.Kept]]. */
  private def keptOf(rs: ResultSet, first: Int): Joins.Kept = {
    def at(i: Int): Option[Instant] =
      Option(rs.getObject(i, classOf[OffsetDateTime])).map(_.toInstant)
    Joins.Kept(at(first), at(first + 1), at(first + 2))
  }

  /** `sql` run with `bind`'s parameters; the rows it changed. */
  private def update(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        ps.executeUpdate()
      }
    }
  }
}
