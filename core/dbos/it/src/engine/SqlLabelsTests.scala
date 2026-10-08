package grit.dbos.engine

import scala.util.Using

import grit.core.clock.Clock
import grit.core.store.Tx
import grit.core.visibility.{Compartment, Compartments, RoomLabels, Visibility}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** The compartments a database runs under, recorded at each engine start, and the version a
  * label is interned under.
  */
object SqlLabelsTests extends TestSuite {

  private def compartment(name: String): Compartment =
    Compartment.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /** A visibility declaring `names`, and nothing else. */
  private def declaring(names: String*): Visibility =
    Compartments
      .of(names.toVector.map(compartment))
      .left
      .map(c => s"twice: ${Compartment.name(c)}")
      .flatMap(cs =>
        Visibility.of(cs, RoomLabels.Public, Vector.empty, Vector.empty).left.map(_.toString)
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** Every row `sql` reads, its columns joined by `|`. */
  private def rows(config: DbConfig, sql: String): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val n = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[String]
          while (rs.next()) out += (1 to n).map(rs.getString).mkString("|")
          out.result()
        }
      }
    }

  private def versions(config: DbConfig): Vector[String] =
    rows(
      config,
      "SELECT version, compartment FROM grit.compartments ORDER BY version, compartment"
    )

  val tests = Tests {
    test(
      "a first start records its compartments as version 1, the same again writes nothing, and more is the next version"
    ) {
      val config = TestPostgres.freshDatabase("labels_versions")
      LiveEngine.open(config, "test", visibility = declaring("trial")).close()
      LiveEngine.open(config, "test", visibility = declaring("trial")).close()
      LiveEngine.open(config, "test", visibility = declaring("trial", "acme")).close()
      versions(config) ==> Vector(
        "1|trial",
        "1|unmapped",
        "2|acme",
        "2|trial",
        "2|unmapped"
      )
    }

    test(
      "a start dropping a compartment its database ran under does not open, names it, and records nothing"
    ) {
      val config = TestPostgres.freshDatabase("labels_dropped")
      LiveEngine.open(config, "test", visibility = declaring("trial", "acme")).close()
      val starts = rows(config, "SELECT count(*) FROM grit.engine_starts")
      val refused = Engine.open(
        config,
        "test",
        LiveEngine.Identity,
        LiveEngine.Uncapped,
        declaring("acme", "ops"),
        Clock.system()
      ) match {
        case Left(why) => Some(why)
        case Right(engine) =>
          engine.close()
          None
      }
      (refused, versions(config), rows(config, "SELECT count(*) FROM grit.engine_starts")) ==> (
        Some(Unopened.Dropped(compartment("trial"))),
        Vector("1|acme", "1|trial", "1|unmapped"),
        starts
      )
      // Its lock was let go: the next start, declaring what it ran under, opens.
      LiveEngine.open(config, "test", visibility = declaring("trial", "acme")).close()
    }

    test("a label is interned under the newest version of the compartments") {
      val config = TestPostgres.freshDatabase("labels_made_under")
      LiveEngine.open(config, "test", visibility = declaring("trial")).close()
      LiveEngine.open(config, "test", visibility = declaring("trial", "acme")).close()
      val id = rows(config, "SELECT grit.intern_label(ROW(1, '{acme}')::grit.label)")
      rows(config, s"SELECT made_under FROM grit.labels WHERE id = ${id.mkString}") ==> Vector("2")
    }
  }
}
