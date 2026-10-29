package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}

import scala.util.Using

import grit.core.claim.{ClaimRole, Claimed, Claims}
import grit.core.id.{ClaimHolder, DocKey, PluginName, PrincipalIds}
import grit.core.identity.Principal
import grit.core.session.Session
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Label, TestLabels}
import grit.dbos.sql.{LiveDb, SqlClaims, TestPostgres}

import utest.*

/** Claims against a real Postgres, beyond the contract: what they leave in other tables, the
  * sessions that lease them, and the sweep that deletes them.
  */
object ClaimsLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("claims_live")
    LiveEngine.open(c, "test").close()
    c
  }

  /** The rows of `grit.documents` and of `grit.tombstones`. */
  private def versioned(): (Long, Long) =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT (SELECT count(*) FROM grit.documents), (SELECT count(*) FROM grit.tombstones)"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          (rs.getLong(1), rs.getLong(2))
        }
      }
    }

  private def made(c: Claimed): Boolean = c match {
    case Claimed.Made(_) => true
    case _ => false
  }

  private def ok[A](e: Either[Any, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val at = Instant.parse("2026-03-01T00:00:00Z")

  private val by = new Principal.Person(PrincipalIds.stored("local"), Set.empty)

  private val builder = ok(ClaimRole.of("builder"))

  /** `subject`'s claim for `holder` in `c` at `label`, at `when`, by a transaction at
    * `clearance`.
    */
  private def claim(
      c: SqlClaims,
      subject: String,
      holder: String,
      when: Instant,
      label: Label = Label.Public,
      clearance: Clearance = Clearance.of(Label.Public)
  ): Claimed =
    ok(
      LiveDb.transaction(config, clearance)(
        c.claim(ok(DocKey.of(subject)), ok(ClaimHolder.of(holder)), builder, by, label, when)
      )
    )

  /** The rows of `grit.claims` holding `holder`. */
  private def held(holder: String): Long =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SELECT count(*) FROM grit.claims WHERE holder = ?")) {
        ps =>
          ps.setString(1, holder)
          Using.resource(ps.executeQuery()) { rs =>
            rs.next()
            rs.getLong(1)
          }
      }
    }

  /** Whether, within 10 seconds, some session waits on a lock another holds. */
  private def waiting(): Boolean = {
    val deadline = System.nanoTime() + 10_000_000_000L
    def blocked: Boolean = LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.createStatement()) { st =>
        Using.resource(st.executeQuery("SELECT count(*) FROM pg_locks WHERE NOT granted")) { rs =>
          rs.next()
          rs.getLong(1) > 0
        }
      }
    }
    while (!blocked && System.nanoTime() < deadline) Thread.sleep(10)
    blocked
  }

  val tests = Tests {
    test("claim, release and claim again write no document version and no tombstone") {
      val c = new SqlClaims(PluginName.of("claims-unversioned").getOrElse(sys.error("name")))
      val by = new Principal.Person(PrincipalIds.stored("local"), Set.empty)
      val key = DocKey.of("p/a").getOrElse(sys.error("key"))
      val at = Instant.parse("2026-03-01T00:00:00Z")
      val before = versioned()
      val ran = LiveDb.transaction(config, Clearance.of(Label.Public))(
        for {
          holder <- ClaimHolder.of("session").left.map(StoreError.Invalid(_))
          role <- ClaimRole.of("builder").left.map(StoreError.Invalid(_))
          first <- c.claim(key, holder, role, by, Label.Public, at)
          freed <- c.release(key, Label.Public)
          again <- c.claim(key, holder, role, by, Label.Public, at)
        } yield (made(first), freed.nonEmpty, made(again))
      )
      val after = versioned()
      assert(ran == Right((true, true, true)), after == before)
    }

    test("a session the claimer does not read keeps no claim from lapsing for it") {
      val c = new SqlClaims(ok(PluginName.of("claims-hidden")))
      val hour = at.plusSeconds(3600)
      HolderSessions.work(config, by, ok(ClaimHolder.of("hidden-1")), hour, TestLabels.Trial)
      val high = Clearance.of(TestLabels.Trial)
      val trial = TestLabels.Trial
      claim(c, "p/a", "hidden-1", at, trial, high)
      val lapse = at.plusMillis(Claims.Lease.toMillis)
      val kept = claim(c, "p/a", "hidden-2", lapse, trial, high)
      val taken = claim(c, "p/a", "hidden-2", lapse, trial)
      val first = kept match {
        case Claimed.Taken(Some(c)) => Some((ClaimHolder.value(c.holder), c.active))
        case _ => None
      }
      assert(first == Some(("hidden-1", hour)), made(taken))
    }

    test("the sweep deletes a claim once lapsed longer than a session counts as unended") {
      val c = new SqlClaims(ok(PluginName.of("claims-forgotten")))
      claim(c, "p/a", "forgotten-1", at)
      val due = at.plusMillis(Claims.Lease.toMillis + Session.Unended.toMillis)
      ok(LiveDb.transaction(config)(SqlClaims.forget(due.minusMillis(1))))
      val early = held("forgotten-1")
      ok(LiveDb.transaction(config)(SqlClaims.forget(due)))
      assert(early == 1L, held("forgotten-1") == 0L)
    }

    test("of two transactions claiming one subject at once, the second waits and is refused") {
      val c = new SqlClaims(ok(PluginName.of("claims-raced")))
      val made = new CountDownLatch(1)
      val commit = new CountDownLatch(1)
      val pool = Executors.newFixedThreadPool(2)
      try {
        val first = pool.submit(new Callable[Claimed] {
          def call(): Claimed = LiveDb.transaction(config, Clearance.of(Label.Public)) {
            val got = ok(
              c.claim(
                ok(DocKey.of("p/race")),
                ok(ClaimHolder.of("race-1")),
                builder,
                by,
                Label.Public,
                at
              )
            )
            made.countDown()
            val _ = commit.await(10, TimeUnit.SECONDS)
            got
          }
        })
        assert(made.await(10, TimeUnit.SECONDS))
        val second = pool.submit(new Callable[Either[StoreError, Claimed]] {
          def call(): Either[StoreError, Claimed] =
            LiveDb.transaction(config, Clearance.of(Label.Public))(
              c.claim(
                ok(DocKey.of("p/race")),
                ok(ClaimHolder.of("race-2")),
                builder,
                by,
                Label.Public,
                at
              )
            )
        })
        val waited = waiting()
        commit.countDown()
        val one = first.get(10, TimeUnit.SECONDS)
        // The claim read back names its person's accounts; the one made, as it was given.
        val refused = second.get(10, TimeUnit.SECONDS).map {
          case Claimed.Taken(Some(t)) => Claimed.Taken(Some(t.copy(by = by)))
          case other => other
        }
        (waited, refused) ==> ((
          true,
          one match {
            case Claimed.Made(claim) => Right(Claimed.Taken(Some(claim)))
            case other => Right(other)
          }
        ))
      } finally pool.shutdownNow()
    }
  }
}
