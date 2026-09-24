package grit.dbos.sql

import java.sql.DriverManager

import scala.util.Using

import utest.*

/** The test image carries pg_textsearch, preloaded: an extension and a BM25 index can be
  * created in a fresh database, and the index ranks. Guards docker/postgres/Dockerfile.
  */
object PgTextsearchTests extends TestSuite {

  private lazy val config = TestPostgres.freshDatabase("pg_textsearch")

  val tests = Tests {

    test("a bm25 index ranks the matching row first") {
      val ranked =
        Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
          conn =>
            Using.resource(conn.createStatement()) { st =>
              st.execute("CREATE EXTENSION IF NOT EXISTS pg_textsearch")
              st.execute("CREATE TABLE docs (id int PRIMARY KEY, body text NOT NULL)")
              st.execute(
                "INSERT INTO docs VALUES (1, 'the dev database runs in docker compose'), " +
                  "(2, 'the live tests start a throwaway Postgres with Testcontainers')"
              )
              st.execute(
                "CREATE INDEX docs_bm25 ON docs USING bm25 (body) WITH (text_config='english')"
              )
              Using.resource(
                st.executeQuery(
                  "SELECT id FROM docs ORDER BY body <@> to_bm25query('testcontainers', 'docs_bm25') LIMIT 1"
                )
              ) { rs =>
                val ids = Vector.newBuilder[Int]
                while (rs.next()) ids += rs.getInt(1)
                ids.result()
              }
            }
        }
      ranked ==> Vector(2)
    }
  }
}
