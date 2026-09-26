package grit.dbos.sql

import java.sql.DriverManager

import scala.util.Using

import utest.*

/** The diagnostic for docker/postgres/Dockerfile. An image without pg_textsearch fails
  * every suite that opens an engine, in the fixture, at `schema.sql`'s `CREATE EXTENSION`;
  * this suite touches nothing of grit's, so its failure points at the image alone.
  */
object PostgresImageTests extends TestSuite {

  private lazy val config = TestPostgres.freshDatabase("pg_textsearch")

  val tests = Tests {

    test("the image has pg_textsearch preloaded: a bm25 index ranks the matching row first") {
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
