package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.claim.{ClaimHolder, ClaimRole, Claimed}
import grit.core.id.{DocKey, PluginName, PrincipalIds}
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.{LiveDb, SqlClaims, TestPostgres}

import utest.*

/** Claims against a real Postgres, beyond the contract: what they leave in other tables. */
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
  }
}
