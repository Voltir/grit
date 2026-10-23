package grit.dbos.sql

import java.sql.DriverManager
import java.time.Instant

import scala.io.Source
import scala.util.Using

import utest.*

/** [[SqlKept]]'s statements against a real Postgres. */
object SqlKeptTests extends TestSuite {

  // schema.sql applied as an engine applies it; this package may not open one (grit.dbos.engine's
  // suites import it).
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_kept")
    val schema = Using.resource(getClass.getResourceAsStream("/schema.sql"))(
      Source.fromInputStream(_).mkString
    )
    Using.resource(DriverManager.getConnection(c.jdbcUrl, c.user, c.password)) { conn =>
      Using.resource(conn.createStatement())(_.execute(schema))
    }
    c
  }

  val tests = Tests {

    test("a ? inside a literal in delete's where is not one of its parameters") {
      val deleted = LiveDb.transaction(config) {
        SqlKept.Documents.delete("plugin = ? AND key <> 'who?'", Instant.EPOCH)(_.setString(_, "p"))
      }
      assert(deleted == Right(()))
    }
  }
}
