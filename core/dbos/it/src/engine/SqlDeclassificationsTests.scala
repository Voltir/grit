package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms, Pointer, TestDeclassified, Written}
import grit.core.id.PluginName
import grit.core.place.{Namespace, Place}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Level, TestLabels}
import grit.dbos.sql.{
  LiveDb,
  SqlDeclassifications,
  SqlDocuments,
  SqlStatements,
  SqlTombstones,
  TestPostgres
}

import utest.*

/** The operator's declassification rows as the collector forgets them, against a real Postgres. */
object SqlDeclassificationsTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_declassifications")
    LiveEngine.open(c, "test").close()
    c
  }

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def run(sql: String)(bind: java.sql.PreparedStatement => Unit): Unit =
    LiveDb
      .transaction(config)(SqlStatements.update(sql)(bind))
      .fold(
        e => throw new java.lang.AssertionError(s"$e"),
        _ => ()
      )

  private def json(p: Pointer): String =
    ujson.Obj("arguments" -> p.arguments, "tool" -> ToolName.value(p.tool)).render()

  private def policy(version: Int): Unit =
    run(
      """INSERT INTO grit.declassification_policies
        |       (over, version, classification_id, text, set_at, set_by)
        |VALUES ('compartment:fx', ?, 1, 'remove it', now(), 'local')""".stripMargin
    )(_.setInt(1, version))

  private def application(p: Pointer, version: Int, applied: Boolean): Unit =
    run(
      """INSERT INTO grit.declassification_applications (pointer, version, over, set_at, set_by)
        |VALUES (?::jsonb, ?, ?, now(), 'local')""".stripMargin
    ) { ps =>
      ps.setString(1, json(p))
      ps.setInt(2, version)
      ps.setString(3, if (applied) "compartment:fx" else null)
    }

  private def file(id: String): Pointer =
    got(Pointer.of(ToolName("drive_read_file"), "file" -> ujson.Str(id)))

  /** Each kept row, `p<version>` for a policy, `<file>:<version>` for an application. */
  private def kept: Vector[String] =
    LiveDb
      .transaction(config)(
        SqlStatements.many(
          """SELECT 'p' || version AS row FROM grit.declassification_policies
          |UNION ALL
          |SELECT (pointer -> 'arguments' ->> 'file') || ':' || version
          |  FROM grit.declassification_applications
          | ORDER BY row""".stripMargin
        )(_ => ())(_.getString("row"))
      )
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {

    test(
      "the collector forgets the declassification versions neither current nor named by an audit row"
    ) {
      val (a, b) = (file("fx-a"), file("fx-b"))
      val source = Label.at(Level.Internal, TestLabels.compartment("fx"))
      policy(1)
      application(a, 1, applied = true)
      val keeper = new SqlDocuments(new SqlTombstones).keeper(
        got(PluginName.of("fx-rewrites")),
        got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 30.days, 10))
      )
      LiveDb.transaction(config, Clearance.of(Label.at(Level.Internal))) {
        val applied = keeper
          .declassifying(source, a)
          .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
          .getOrElse(throw new java.lang.AssertionError("no application"))
        val d =
          TestDeclassified.of(
            "gist",
            "fx-cards",
            "card",
            Place.under(Namespace.Task, Vector("fx")),
            source,
            a,
            applied
          )
        keeper.declassified(d, Integrity.Member, Instant.parse("2026-03-01T00:00:00Z")) match {
          case Right(Written.Versioned(_, _, _)) => ()
          case other => throw new java.lang.AssertionError(s"$other")
        }
      }
      policy(2)
      policy(3)
      application(a, 2, applied = true)
      application(b, 1, applied = true)
      application(b, 2, applied = false)
      val forgot = LiveDb
        .transaction(config)(SqlDeclassifications.forget)
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      (forgot, kept) ==> (2, Vector("fx-a:1", "fx-a:2", "fx-b:2", "p1", "p3"))
    }
  }
}
