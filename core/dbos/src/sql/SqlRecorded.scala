package grit.dbos.sql

import java.sql.Connection

import scala.util.Using

import grit.core.identity.Account
import grit.core.store.StoreError
import grit.core.visibility.{GroupName, Label, Recorded, RoomAccess}

/** What is recorded beside the deployment's declaration ([[Recorded]]): `grit.rooms` and
  * `grit.group_members`, read whole when a transaction opens.
  */
private[dbos] object SqlRecorded {
  import SqlEntryStore.attempt

  /** Every `grit.rooms` row and every `grit.group_members` row, read on `conn` in one round
    * trip. `DatabaseError` when the read fails; `Invalid` when a row names a place, account or
    * group no value could be (a room's label fails closed instead, as [[SqlLabels.read]] does).
    */
  def read(conn: Connection^): Either[StoreError, Recorded] =
    attempt {
      Using.resource(conn.prepareStatement(Query)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Row]
          while (rs.next()) {
            rows += (Option(rs.getString("place")) match {
              case Some(place) =>
                Row.Room(
                  place,
                  Option(rs.getString("access")),
                  Option(rs.getString("label_compartments")).map(_ => SqlLabels.read(rs)),
                  rs.getBoolean("quiet")
                )
              case None => Row.Member(rs.getString("group_name"), rs.getString("account"))
            })
          }
          rows.result()
        }
      }
    }.flatMap(recorded)

  /** A row as read, before its parts are: a room's or a member's. */
  private enum Row {
    case Room(place: String, access: Option[String], label: Option[Label], quiet: Boolean)
    case Member(group: String, account: String)
  }

  private def recorded(rows: Vector[Row]): Either[StoreError, Recorded] =
    rows
      .foldLeft[Either[String, Recorded]](Right(Recorded.Empty)) {
        case (Left(why), _) => Left(why)
        case (Right(r), Row.Room(place, access, label, quiet)) =>
          for {
            at <- SqlPlaces.read(place)
            reported <- access.fold(Right(None))(a => this.access(a).map(Some(_)))
          } yield Recorded(r.rooms.updated(at, Recorded.Kept(reported, label, quiet)), r.added)
        case (Right(r), Row.Member(group, account)) =>
          for {
            g <- GroupName.of(group).left.map(why => s"a group member's group: $why")
            a <- SqlIdentities
              .read(account)
              .left
              .map(why => s"a group member's account: $why")
          } yield Recorded(
            r.rooms,
            r.added.updated(g, r.added.getOrElse(g, Set.empty[Account]) + a)
          )
      }
      .left
      .map(why => StoreError.Invalid(s"what is recorded of rooms and groups: $why"))

  /** `grit.rooms.access` as stored. */
  private def access(stored: String): Either[String, RoomAccess] =
    stored match {
      case "open" => Right(RoomAccess.Open)
      case "invited" => Right(RoomAccess.Invited)
      case other => Left(s"no access $other")
    }

  /** Rooms first, tagged by a place; members after, by none. */
  private val Query: String =
    s"""SELECT array_to_json(p.path)::text AS place, r.access, r.quiet,
       |       ${SqlLabels.columns("l")}, NULL::text AS group_name, NULL::text AS account
       |  FROM grit.rooms r
       |  JOIN grit.places p ON p.id = r.place_id
       |  LEFT JOIN grit.labels l ON l.id = r.label_id
       |UNION ALL
       |SELECT NULL, NULL, NULL, NULL, NULL, m.group_name, m.account
       |  FROM grit.group_members m""".stripMargin

}
