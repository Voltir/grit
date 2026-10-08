package grit.app.main

import scala.util.Using

import grit.core.identity.{Account, TestAccounts}
import grit.core.store.Tx
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.Offered
import grit.kit.run.Kit

import utest.*

/** The reference deployment's start, as its own engine runs it, against a real Postgres. */
object TrustingLiveTests extends TestSuite {

  /** Each row `sql` reads, as its columns' text. */
  private def rows(config: DbConfig, sql: String): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val width = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to width).map(rs.getString).toVector
          out.result()
        }
      }
    }

  /** `account` attested a full member with `email`, as a start that trusted its realm left it. */
  private def attested(config: DbConfig, account: Account, email: String): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          """WITH home AS (INSERT INTO grit.principals (id, kind)
            |              VALUES (uuidv7()::text, 'person') RETURNING id)
            |INSERT INTO grit.identities (account, home) SELECT ?, id FROM home""".stripMargin
        )
      ) { ps =>
        ps.setString(1, Account.written(account))
        val _ = ps.executeUpdate()
      }
      Using.resource(
        conn.prepareStatement(
          """WITH person AS (INSERT INTO grit.principals (id, kind)
            |                VALUES (uuidv7()::text, 'person') RETURNING id)
            |INSERT INTO grit.emails (email, principal_id) SELECT ?, id FROM person""".stripMargin
        )
      ) { ps =>
        ps.setString(1, email)
        val _ = ps.executeUpdate()
      }
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.attestations (account, email, member, seen_at)
            |VALUES (?, ?, true, now())""".stripMargin
        )
      ) { ps =>
        ps.setString(1, Account.written(account))
        ps.setString(2, email)
        val _ = ps.executeUpdate()
      }
    }

  val tests = Tests {
    test(
      "the reference deployment's start ends the attestations of a realm it no longer trusts"
    ) {
      val config = TestPostgres.freshDatabase("app_trusting")
      LiveEngine.open(config, "test").close()
      val account = TestAccounts.account("slack:T1/U-dropped")
      attested(config, account, "dropped@example.com")
      val deployment = Main
        .deployment(
          Map("GRIT_CLAIMED_DOMAINS" -> "example.com"),
          Offered.Read,
          Vector.empty,
          Vector.empty,
          java.time.ZoneOffset.UTC
        )
        .fold(sys.error, identity)
      val engine = LiveEngine.open(config, "test", visibility = deployment.visibility)
      val started =
        try Kit.trusting(engine, deployment)
        finally engine.close()
      (
        started,
        rows(config, "SELECT account, coalesce(email, ''), member::text FROM grit.attestations")
      ) ==> (Right(()), Vector(Vector(Account.written(account), "", "false")))
    }
  }
}
