package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Using

import grit.core.edge.{Joins, JoinsContract}
import grit.core.identity.{Account, Standing, Vouched}
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

  protected def quiet(j: Joins, room: Place, on: Boolean): Unit =
    under { (tx: Tx^) ?=>
      SqlPlaces.id(room).flatMap { place =>
        SqlEntryStore.attempt {
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          Using.resource(
            conn.prepareStatement(
              """INSERT INTO grit.rooms (place_id, quiet) VALUES (?::uuid, ?)
                |ON CONFLICT (place_id) DO UPDATE SET quiet = EXCLUDED.quiet""".stripMargin
            )
          ) { ps =>
            ps.setString(1, place)
            ps.setBoolean(2, on)
            ps.executeUpdate()
          }
        }
      }
    }.fold(e => throw new java.lang.AssertionError(s"quieting: $e"), _ => ())

  protected def access(j: Joins, room: Place): Option[RoomAccess] =
    under { (tx: Tx^) ?=>
      Tx.access(room)
    }
}
