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

  /** The `version`th policy of `compartment:<over>`: a `draft`, or published to `withdraw` or
    * `keep`.
    */
  private def policy(version: Int, over: String = "fx", status: String = "withdraw"): Unit =
    run(
      """INSERT INTO grit.declassification_policies
        |       (over, version, classification_id, text, status, on_publish, set_at, set_by)
        |VALUES ('compartment:' || ?, ?, 1, 'remove it',
        |        CASE WHEN ? = 'draft' THEN 'draft' ELSE 'published' END, NULLIF(?, 'draft'),
        |        now(), 'local')""".stripMargin
    ) { ps =>
      ps.setString(1, over)
      ps.setInt(2, version)
      ps.setString(3, status)
      ps.setString(4, status)
    }

  /** A bench of `compartment:<over>`'s `version`th policy. */
  private def bench(version: Int, over: String): Unit =
    run(
      """INSERT INTO grit.declassification_benches
        |       (over, policy_version, screening, passed, examples, benched_at)
        |VALUES ('compartment:' || ?, ?, repeat('a', 64), true, '[]', now())""".stripMargin
    ) { ps =>
      ps.setString(1, over)
      ps.setInt(2, version)
    }

  /** The policy versions of `compartment:<over>` benched. */
  private def benched(over: String): Vector[Int] =
    LiveDb
      .transaction(config)(
        SqlStatements.many(
          """SELECT policy_version FROM grit.declassification_benches
            | WHERE over = 'compartment:' || ? ORDER BY policy_version""".stripMargin
        )(_.setString(1, over))(_.getInt("policy_version"))
      )
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  private def application(p: Pointer, version: Int, applied: Boolean, over: String = "fx"): Unit =
    run(
      """INSERT INTO grit.declassification_applications (pointer, version, over, set_at, set_by)
        |VALUES (?::jsonb, ?, ?, now(), 'local')""".stripMargin
    ) { ps =>
      ps.setString(1, json(p))
      ps.setInt(2, version)
      ps.setString(3, if (applied) s"compartment:$over" else null)
    }

  private def file(id: String): Pointer =
    got(Pointer.of(ToolName("drive_read_file"), "file" -> ujson.Str(id)))

  /** Each kept row of `compartment:<over>`, `p<version>` for a policy, `<file>:<version>` for an
    * application to a file whose id starts `<over>-`.
    */
  private def kept(over: String): Vector[String] =
    LiveDb
      .transaction(config)(
        SqlStatements.many(
          """SELECT 'p' || version AS row FROM grit.declassification_policies
          | WHERE over = 'compartment:' || ?
          |UNION ALL
          |SELECT (pointer -> 'arguments' ->> 'file') || ':' || version
          |  FROM grit.declassification_applications
          | WHERE pointer -> 'arguments' ->> 'file' LIKE ? || '-%'
          | ORDER BY row""".stripMargin
        ) { ps =>
          ps.setString(1, over)
          ps.setString(2, over)
        }(_.getString("row"))
      )
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  /** A rewrite of the card the file `card` reads, kept at `compartment:<over>` without it under
    * the application in force for it, which names its policy version in an audit row.
    */
  private def declassified(over: String, card: Pointer): Unit = {
    val source = Label.at(Level.Internal, TestLabels.compartment(over))
    val keeper = new SqlDocuments(new SqlTombstones).keeper(
      got(PluginName.of(s"$over-rewrites")),
      got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 30.days, 10))
    )
    LiveDb.transaction(config, Clearance.of(Label.at(Level.Internal))) {
      val applied = keeper
        .declassifying(source, card)
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
        .map(_.applied)
        .getOrElse(throw new java.lang.AssertionError("no application"))
      val d =
        TestDeclassified.of(
          "gist",
          s"$over-cards",
          "card",
          Place.under(Namespace.Task, Vector(over)),
          source,
          card,
          "r1",
          applied
        )
      keeper.declassified(d, Integrity.Member, Instant.parse("2026-03-01T00:00:00Z")) match {
        case Right(Written.Versioned(_, _, _)) => ()
        case other => throw new java.lang.AssertionError(s"$other")
      }
    }
  }

  private def forgotten: Int =
    LiveDb
      .transaction(config)(SqlDeclassifications.forget)
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {

    test(
      "the collector forgets the declassification versions neither current nor named by an audit row"
    ) {
      val (a, b) = (file("fx-a"), file("fx-b"))
      policy(1)
      application(a, 1, applied = true)
      declassified("fx", a)
      policy(2)
      policy(3)
      application(a, 2, applied = true)
      application(b, 1, applied = true)
      application(b, 2, applied = false)
      (forgotten, kept("fx")) ==> (2, Vector("fx-a:1", "fx-a:2", "fx-b:2", "p1", "p3"))
    }

    test(
      "the collector keeps a standing version and its bench, the last version published to withdraw and a draft above the version in force, and forgets an abandoned draft below it with its bench"
    ) {
      val card = file("fs-a")
      application(card, 1, applied = true, "fs")
      policy(1, "fs")
      policy(2, "fs", "draft")
      bench(2, "fs")
      policy(3, "fs")
      policy(4, "fs", "keep")
      bench(4, "fs")
      declassified("fs", card)
      policy(5, "fs", "keep")
      policy(6, "fs", "draft")
      (forgotten, kept("fs"), benched("fs")) ==> (
        2,
        Vector("fs-a:1", "p3", "p4", "p5", "p6"),
        Vector(4)
      )
    }
  }
}
