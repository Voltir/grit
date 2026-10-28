package grit.dbos.sql

import java.sql.DriverManager
import java.time.Instant

import scala.io.Source
import scala.util.Using

import grit.core.id.{DocKey, PluginName}
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Label}

import utest.*

/** How Postgres reads a key's history of documents, against a real Postgres. */
object SqlDocumentHistoryTests extends TestSuite {

  // schema.sql applied as an engine applies it; this package may not open one (grit.dbos.engine's
  // suites import it).
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_document_history")
    val schema = Using.resource(getClass.getResourceAsStream("/schema.sql"))(
      Source.fromInputStream(_).mkString
    )
    Using.resource(DriverManager.getConnection(c.jdbcUrl, c.user, c.password)) { conn =>
      Using.resource(conn.createStatement())(_.execute(schema))
    }
    c
  }

  /** Postgres's plan for `sql`, bound by `bind`, every line, with sequential scans off: so the
    * plan shows which index serves the read, whatever the table's size.
    */
  private def plan(sql: String)(bind: java.sql.PreparedStatement => Unit): String =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SET LOCAL enable_seqscan = off"))(ps => {
        val _ = ps.execute()
      })
      Using.resource(conn.prepareStatement(s"EXPLAIN $sql")) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[String]
          while (rs.next()) out += rs.getString(1)
          out.result().mkString("\n")
        }
      }
    }

  val tests = Tests {

    test("as of an instant is read through idx_documents_history, never a scan of the plugin") {
      val (plugin, key) = (
        PluginName.of("history").fold(e => sys.error(e), identity),
        DocKey.of("p/a").fold(e => sys.error(e), identity)
      )
      val shown = plan(SqlDocuments.AsOf) { ps =>
        SqlClearance.bind(ps, 1, Clearance.of(Label.Public))
        SqlDocuments.bindAsOf(ps, plugin, key, Label.Public, Instant.parse("2026-03-01T00:00:00Z"))
      }
      assert(shown.contains("idx_documents_history"))
    }
  }
}
