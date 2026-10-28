package grit.dbos.engine

import java.time.{Instant, LocalDate}

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.{Try, Using}

import grit.core.document.{DocLabel, DocRef, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{DocKey, DocumentVersionId, PluginName, PrincipalId}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.{DbConfig, LiveDb, SqlDocuments, SqlIdentities, SqlTombstones, TestPostgres}

import utest.*

/** `claims-from-documents.sql`, the one-off that moves the projects plugin's claim documents into
  * `grit.claims`, run on a database of the schema before that table: claim documents written as
  * the plugin wrote them, then this schema applied by an engine's start, then the file.
  */
object ClaimsMigrationTests extends TestSuite {

  private val Projects: PluginName = PluginName.of("projects").getOrElse(sys.error("name"))

  private val Terms: DocumentTerms =
    (for {
      label <- DocLabel.of("projects")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 180.days, 100)
    } yield terms).getOrElse(sys.error("terms"))

  private val at: Instant = Instant.parse("2026-10-27T12:00:00Z")

  private val Public: Clearance = Clearance.of(Label.Public)

  private def ok[A](e: Either[StoreError | String, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val migration: String =
    Using.resource(
      Option(getClass.getResourceAsStream("/claims-from-documents.sql"))
        .getOrElse(sys.error("no claims-from-documents.sql"))
    )(Source.fromInputStream(_).mkString)

  /** A fresh database of the schema before `grit.claims`: this schema with that table dropped. */
  private def previous(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    sql(c, "DROP TABLE grit.claims")
    c
  }

  /** Runs `statements` on their own connection, committing each as it goes. */
  private def sql(config: DbConfig, statements: String): Unit =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    )(conn => Using.resource(conn.createStatement())(_.execute(statements)))

  /** What `query` selects, each row's columns as text. */
  private def rows(config: DbConfig, query: String): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(query)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val n = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to n).toVector.map(rs.getString)
          out.result()
        }
      }
    }

  private def keeper: DocumentKeeper = new SqlDocuments(new SqlTombstones).keeper(Projects, Terms)

  /** The claim document on `artifact` for `slot`, by `by`, as the plugin wrote it. */
  private def claimDocument(
      config: DbConfig,
      artifact: String,
      slot: String,
      by: String
  ): DocumentVersionId =
    LiveDb.transaction(config, Public)(
      for {
        key <- DocKey.of(s"claim/$artifact")
        text <- DocText.of(s"$artifact claimed as builder for $slot by $by")
        on <- DocRef.of("claims", artifact)
        held <- DocRef.of("claimed-for", slot)
        written <- keeper.write(
          key,
          Label.Public,
          Integrity.Authority,
          Place.Everywhere,
          text,
          ujson.Obj("claimed" -> ujson.Obj("by" -> by, "for" -> slot, "role" -> "builder")),
          None,
          at,
          Set(on, held)
        )
      } yield written
    ) match {
      case Right(w) =>
        ok(
          LiveDb.transaction(config, Public)(
            keeper.current(ok(DocKey.of(s"claim/$artifact")), w.kept)
          )
        )
          .map(_.version)
          .getOrElse(sys.error("no claim document"))
      case Left(e) => throw new java.lang.AssertionError(s"$e")
    }

  /** The person `account` is linked to, enrolled; their principal's id. */
  private def enrolled(config: DbConfig, account: String): String = {
    val a = ok(Account.read(account))
    LiveDb.transaction(config)(ok(SqlIdentities.enroll(Set(a))))
    PrincipalId.value(LiveDb.principal(config, a))
  }

  private val claimsTable: String =
    "SELECT subject, holder, role, principal_id, made_at = '2026-10-27T12:00:00Z' FROM grit.claims " +
      "WHERE plugin = 'projects' ORDER BY subject"

  private val claimDocuments: String =
    "SELECT count(*) FROM grit.documents WHERE plugin = 'projects' AND key LIKE 'claim/%'"

  val tests = Tests {
    test(
      "live claims move to grit.claims by any linked account or a bare principal; every claim document, its tombstones and reviews go"
    ) {
      val config = previous("claims_migration")
      val ann = enrolled(config, "slack:T1/U-ann")
      sql(config, "INSERT INTO grit.principals (id, kind) VALUES ('p-bare', 'person')")
      // The first account `by` names is linked to no one.
      claimDocument(config, "p/a", "coder-opus/one", "slack:T0/U-gone, slack:T1/U-ann")
      claimDocument(config, "p/b", "coder-opus/two", "p-bare")
      val freed = claimDocument(config, "p/c", "coder-opus/three", "slack:T1/U-ann")
      val day = LocalDate.of(2026, 10, 27)
      ok(LiveDb.transaction(config, Public)(keeper.forReview(freed, day, at)))
      ok(
        LiveDb.transaction(config, Public)(
          keeper.withdraw(ok(DocKey.of("claim/p/c")), Label.Public, at)
        )
      )
      val artifact = LiveDb.transaction(config, Public)(
        keeper.write(
          ok(DocKey.of("p/a")),
          Label.Public,
          Integrity.Authority,
          Place.Everywhere,
          ok(DocText.of("An artifact.")),
          ujson.Obj(),
          None,
          at
        )
      )
      // This schema, applied as an engine's start applies it, creates grit.claims.
      LiveEngine.open(config, "test").close()
      sql(config, migration)
      assert(
        artifact.isRight,
        rows(config, claimsTable) == Vector(
          Vector("p/a", "coder-opus/one", "builder", ann, "t"),
          Vector("p/b", "coder-opus/two", "builder", "p-bare", "t")
        ),
        rows(config, claimDocuments) == Vector(Vector("0")),
        rows(
          config,
          "SELECT count(*) FROM grit.tombstones WHERE kind = 'document' AND target NOT IN " +
            "(SELECT version::text FROM grit.documents)"
        ) == Vector(Vector("0")),
        rows(config, "SELECT count(*) FROM grit.document_reviews WHERE key LIKE 'claim/%'") ==
          Vector(Vector("0")),
        rows(config, "SELECT key FROM grit.documents WHERE plugin = 'projects'") ==
          Vector(Vector("p/a"))
      )
    }

    test("a live claim whose maker is no principal stops it, changing nothing") {
      val config = previous("claims_migration_unknown")
      claimDocument(config, "p/a", "coder-opus/one", "slack:T0/U-gone")
      LiveEngine.open(config, "test").close()
      val ran = Try(sql(config, migration))
      assert(
        ran.failed.toOption.exists(
          _.getMessage.contains(
            "grit.claims holds 0 projects claims, but 1 claim documents are live"
          )
        ),
        rows(config, claimsTable) == Vector(),
        rows(config, claimDocuments) == Vector(Vector("1"))
      )
    }
  }
}
