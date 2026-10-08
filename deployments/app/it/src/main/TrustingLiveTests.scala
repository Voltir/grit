package grit.app.main

import java.time.Instant

import scala.util.Using

import grit.core.clock.Clock
import grit.core.edge.{CatchUp, EdgeRefusal, EdgeStores, Unheard, Variable}
import grit.core.id.EdgeName
import grit.core.identity.{Account, TestAccounts}
import grit.core.store.Tx
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.Offered
import grit.kit.environment.Secrets
import grit.kit.run.{Kit, Launch}
import grit.slack.edge.SlackEdge
import grit.slack.event.TeamId
import grit.turn.Turn

import utest.*

/** The reference deployment's starts, and which of them end what it no longer trusts, against a
  * real Postgres.
  */
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

  /** The attestation rows, as account, email (empty for none) and member. */
  private def attestations(config: DbConfig): Vector[Vector[String]] =
    rows(config, "SELECT account, coalesce(email, ''), member::text FROM grit.attestations")

  /** `env` with `config`'s database. */
  private def on(config: DbConfig, env: Map[String, String]): Map[String, String] =
    env ++ Map(
      DbConfig.UrlVar -> config.jdbcUrl,
      DbConfig.UserVar -> config.user,
      DbConfig.PasswordVar -> config.password
    )

  /** A catch-up of no source, which hears nothing. */
  private object Nothing extends CatchUp {
    def name: EdgeName = EdgeName("nothing")
    def needs: Vector[Variable] = Vector.empty
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, CatchUp.Open^{stores, clock, log, caps.any}] =
      Right(new CatchUp.Open {
        def since: Instant = clock.now()
        def unheard: Vector[Unheard] = Vector.empty
        def hear(): Either[EdgeRefusal, Unit] = Right(())
        def close(): Unit = ()
      })
  }

  val tests = Tests {
    test("a backfill's start ends the attestations of a realm the deployment no longer trusts") {
      val config = TestPostgres.freshDatabase("app_trusting_backfill")
      LiveEngine.open(config, "test").close()
      val account = TestAccounts.account("slack:T1/U-dropped")
      attested(config, account, "dropped@example.com")
      val env = Map("GRIT_CLAIMED_DOMAINS" -> "example.com")
      val deployment = Main
        .deployment(
          env,
          Offered.Read,
          Vector(SlackEdge.serving(Set.empty)),
          Vector.empty,
          java.time.ZoneOffset.UTC,
          Some(TeamId("T2"))
        )
        .fold(sys.error, identity)
      (
        Kit.catchUp(deployment, Nothing, on(config, env), _ => false, _ => ()),
        attestations(config)
      ) ==> (Right(()), Vector(Vector(Account.written(account), "", "false")))
    }

    test("a start of the chat or a run with arguments ends no attestation, whatever it trusts") {
      val config = TestPostgres.freshDatabase("app_trusting_direct")
      LiveEngine.open(config, "test").close()
      val account = TestAccounts.account("slack:T1/U-kept")
      attested(config, account, "kept@example.com")
      // As the chat declares it: no edge served, so no realm trusted, and no domain claimed.
      val deployment = Main
        .deployment(Map.empty, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
        .fold(sys.error, identity)
      val secrets =
        Secrets.of(on(config, Map.empty), deployment).fold(r => sys.error(r.message), identity)
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = deployment.visibility)
      try { val _ = Main.ownEngine(engine, deployment, secrets, Launch.Run.Served, Clock.system()) }
      finally engine.close()
      attestations(config) ==>
        Vector(Vector(Account.written(account), "kept@example.com", "true"))
    }
  }
}
