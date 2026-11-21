package grit.dbos.engine

import java.time.Instant

import scala.io.Source
import scala.util.Using

import grit.core.claim.{ClaimRole, Claimed}
import grit.core.id.{ClaimHolder, DocKey, PluginName, PrincipalIds, SessionId}
import grit.core.identity.Principal
import grit.core.visibility.Label
import grit.dbos.sql.{DbConfig, LiveDb, SqlClaims, TestPostgres}

import utest.*

/** `claim-session.sql`, the one-off that names the session working a claim, run on a database of
  * the schema before it: this schema's `grit.claims` without its two session columns.
  */
object ClaimSessionMigrationTests extends TestSuite {

  private val at: Instant = Instant.parse("2026-11-20T12:00:00Z")

  private val migration: String =
    Using.resource(
      Option(getClass.getResourceAsStream("/claim-session.sql"))
        .getOrElse(sys.error("no claim-session.sql"))
    )(Source.fromInputStream(_).mkString)

  /** Runs `statements` on their own connection. */
  private def sql(config: DbConfig, statements: String): Unit =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    )(conn => Using.resource(conn.createStatement())(_.execute(statements)))

  /** Each constraint on `grit.claims` in `config`, by name, as Postgres defines it. */
  private def constraints(config: DbConfig): Vector[(String, String)] =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(
        conn
          .createStatement()
          .executeQuery(
            "SELECT conname, pg_get_constraintdef(oid) FROM pg_constraint " +
              "WHERE conrelid = 'grit.claims'::regclass ORDER BY conname"
          )
      )(rs =>
        Iterator
          .continually(rs)
          .takeWhile(_.next())
          .map(r => (r.getString(1), r.getString(2)))
          .toVector
      )
    }

  private def ok[A](e: Either[Any, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val by = new Principal.Person(PrincipalIds.stored("local"), Set.empty)

  private val claims = new SqlClaims(ok(PluginName.of("migrated")))

  /** A fresh database holding a claim on `p/old` for `old-1`, whose grit.claims then names no
    * session, as the schema before it made it.
    */
  private def previous(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    ok(claimed(c, "p/old", "old-1", None))
    sql(
      c,
      """ALTER TABLE grit.claims DROP CONSTRAINT claims_session;
        |ALTER TABLE grit.claims DROP COLUMN session_harness;
        |ALTER TABLE grit.claims DROP COLUMN session_local""".stripMargin
    )
    c
  }

  /** `subject` claimed for `holder` in `config`, worked by `session`, or why not. */
  private def claimed(
      config: DbConfig,
      subject: String,
      holder: String,
      session: Option[SessionId]
  ): Either[String, Claimed] =
    scala.util
      .Try(
        LiveDb.transaction(config)(
          claims.claim(
            ok(DocKey.of(subject)),
            ok(ClaimHolder.of(holder)),
            ok(ClaimRole.of("builder")),
            session,
            by,
            Label.Public,
            at
          )
        )
      )
      .fold(e => Left(e.getMessage), _.left.map(_.toString))

  val tests = Tests {
    test("claim-session.sql brings an older grit.claims to this schema, keeping its claims") {
      val config = previous("claim_session")
      val fresh = TestPostgres.freshDatabase("claim_session_fresh")
      LiveEngine.open(fresh, "test").close()
      claimed(config, "p/a", "new-1", None) match {
        case Left(why) => assert(why.contains("session_"))
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
      sql(config, migration)
      val s = ok(SessionId.read("claude-code:migrated-a"))
      val made = claimed(config, "p/a", "new-1", Some(s))
      // Run again once a claim names its session: the second run changes neither it nor the schema.
      sql(config, migration)
      val sessions = ok(LiveDb.transaction(config)(claims.live(Instant.EPOCH)))
        .map(c => (DocKey.value(c.subject), c.session))
      assert(
        made.toOption.collect { case Claimed.Made(c) => c.session } == Some(Some(s)),
        sessions == Vector(("p/a", Some(s)), ("p/old", None)),
        constraints(config) == constraints(fresh)
      )
    }
  }
}
