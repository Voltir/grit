package grit.dbos.engine

import java.time.OffsetDateTime
import java.util.concurrent.atomic.AtomicInteger

import scala.util.Using

import grit.core.admin.{Administration, AdministrationContract, ChangeJson}
import grit.core.identity.{Account, Standing, Vouched}
import grit.core.place.Place
import grit.core.store.{Tx, Voucher}
import grit.core.visibility.RoomAccess
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlPlaces, TestPostgres}

/** The administration contract, kept by the engine's administration against a real Postgres. */
object SqlAdministrationTests extends AdministrationContract {
  import AdministrationContract.*

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

  protected def fresh(): Administration = {
    val c = TestPostgres.freshDatabase(s"sql_administration_${made.incrementAndGet()}")
    val engine = LiveEngine.open(c, "test", visibility = Declared)
    try {
      current = Some(Fresh(c, engine.voucher(Set(T1), Set.empty)))
      engine.administration
    } finally engine.close()
  }

  private def under[A](body: (Tx^) ?=> A): A = LiveDb.under(fresh0.config, Declared)(body)

  protected def member(a: Administration, account: Account): Unit =
    under(fresh0.voucher.vouch(Vouched(account, Standing.Full(None))))
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), _ => ())

  protected def seen(a: Administration, account: Account): Unit =
    Vouchings.enrolled(account)(using fresh0.config)

  protected def known(a: Administration, account: Account): Boolean =
    under(fresh0.voucher.lastWords(T1)).exists(_.exists(_.account == account))

  protected def reported(a: Administration, room: Place, access: RoomAccess): Unit =
    under { (tx: Tx^) ?=>
      SqlPlaces.id(room).flatMap { place =>
        SqlEntryStore.attempt {
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          Using.resource(
            conn.prepareStatement(
              """INSERT INTO grit.rooms (place_id, access) VALUES (?::uuid, ?)
                |ON CONFLICT (place_id) DO UPDATE SET access = EXCLUDED.access""".stripMargin
            )
          ) { ps =>
            ps.setString(1, place)
            ps.setString(2, if (access == RoomAccess.Open) "open" else "invited")
            ps.executeUpdate()
          }
        }
      }
    }.fold(e => throw new java.lang.AssertionError(s"reporting: $e"), _ => ())

  protected def kept(a: Administration): Vector[Kept] =
    under { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT by, at, change::text FROM grit.visibility_changes ORDER BY at, id"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Kept]
          while (rs.next())
            rows += Kept(
              Account
                .read(rs.getString(1))
                .fold(e => throw new java.lang.AssertionError(e), identity),
              rs.getObject(2, classOf[OffsetDateTime]).toInstant,
              ChangeJson
                .read(ujson.read(rs.getString(3)))
                .fold(e => throw new java.lang.AssertionError(e), identity)
            )
          rows.result()
        }
      }
    }
}
