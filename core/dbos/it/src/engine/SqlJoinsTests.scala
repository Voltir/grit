package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Using

import grit.core.edge.{Joins, JoinsContract}
import grit.core.identity.{Account, Standing, Vouched}
import grit.core.model.Grade
import grit.core.place.Place
import grit.core.store.{Tx, Voucher}
import grit.core.visibility.{RoomAccess, Visibility}
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlPlaces, TestPostgres}

/** The joins contract, kept by the engine's joins against a real Postgres. */
object SqlJoinsTests extends JoinsContract {
  import JoinsContract.*

  /** A test's database and the voucher of [[T1]] over it. Each test gets a database of its own,
    * so no test sees another's people or rooms.
    */
  private final case class Fresh(config: DbConfig, voucher: Voucher)

  // Only ever holds an immutable value; the suite's tests run one at a time.
  @caps.unsafe.untrackedCaptures
  private var current: Option[Fresh] = None

  private val made = new AtomicInteger

  private def fresh0: Fresh =
    current.getOrElse(throw new java.lang.AssertionError("no database"))

  protected def fresh(): Joins = {
    val c = TestPostgres.freshDatabase(s"sql_joins_${made.incrementAndGet()}")
    val engine = LiveEngine.open(c, "test")
    try {
      current = Some(Fresh(c, engine.voucher(Set(T1), Set.empty)))
      engine.joins
    } finally engine.close()
  }

  private def under[A](body: (Tx^) ?=> A): A =
    LiveDb.under(fresh0.config, Visibility.Shipped)(body)

  protected def member(j: Joins, account: Account): Unit =
    under(fresh0.voucher.vouch(Vouched(account, Standing.Full(None))))
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), _ => ())

  // The two hooks below write grit.rooms as SqlAdministration.keep does, not through it: its
  // run answers only an administrator the deployment's visibility declares, and this
  // contract's engine runs under Visibility.Shipped, which declares none. As there, turning
  // quiet off or clearing a baseline updates a row already kept and makes none.

  protected def quiet(j: Joins, room: Place, on: Boolean): Unit =
    rooms(
      room,
      if (on)
        """INSERT INTO grit.rooms (place_id, quiet) VALUES (?::uuid, true)
          |ON CONFLICT (place_id) DO UPDATE SET quiet = true""".stripMargin
      else "UPDATE grit.rooms SET quiet = false WHERE place_id = ?::uuid",
      Vector.empty
    )

  protected def baseline(j: Joins, room: Place, to: Option[Grade]): Unit =
    to match {
      case Some(grade) =>
        rooms(
          room,
          """INSERT INTO grit.rooms (place_id, baseline) VALUES (?::uuid, ?)
            |ON CONFLICT (place_id) DO UPDATE SET baseline = EXCLUDED.baseline""".stripMargin,
          Vector(grade.word)
        )
      case None =>
        rooms(room, "UPDATE grit.rooms SET baseline = NULL WHERE place_id = ?::uuid", Vector.empty)
    }

  /** Runs `sql` on `room`'s row, its place id the first parameter and `rest` the others. */
  private def rooms(room: Place, sql: String, rest: Vector[String]): Unit =
    under { (tx: Tx^) ?=>
      SqlPlaces.id(room).flatMap { place =>
        SqlEntryStore.attempt {
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          Using.resource(conn.prepareStatement(sql)) { ps =>
            ps.setString(1, place)
            rest.zipWithIndex.foreach((v, i) => ps.setString(i + 2, v))
            ps.executeUpdate()
          }
        }
      }
    }.fold(e => throw new java.lang.AssertionError(s"writing $room's row: $e"), _ => ())

  protected def access(j: Joins, room: Place): Option[RoomAccess] =
    under { (tx: Tx^) ?=>
      Tx.access(room)
    }
}
