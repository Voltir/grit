package grit.dbos.engine

import java.sql.{Connection, DriverManager}
import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms}
import grit.core.id.{DocKey, Harness, PluginName, SessionId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.store.Tx
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.{
  LiveDb,
  SqlDocuments,
  SqlIdentities,
  SqlNotices,
  SqlSessions,
  SqlTombstones,
  TestPostgres
}

import utest.*

/** The notices grit's writers send as their transactions commit, as a listener hears them against
  * a real Postgres. The channels and payloads are pinned: they are what a watch's listener matches.
  */
object SqlNoticesTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_notices")
    LiveEngine.open(c, "test").close()
    c
  }

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private val Terms: DocumentTerms =
    ok(DocumentTerms.of(ok(DocLabel.of("notes")), DocWeight.Unscaled, 2.minutes, 10))

  private val plugin: PluginName = ok(PluginName.of("noticed"))

  private val keeper = new SqlDocuments(new SqlTombstones).keeper(plugin, Terms)

  private val t0 = Instant.parse("2026-10-29T10:00:00Z")

  private def write(key: String, text: String)(using Tx^): Unit = {
    val _ = ok(
      keeper.write(
        ok(DocKey.of(key)),
        Label.Public,
        Integrity.Member,
        Place.under(Namespace.Task, Vector.empty),
        ok(DocText.of(text)),
        ujson.Obj(),
        None,
        t0
      )
    )
  }

  private lazy val ann: Principal.Person =
    LiveDb
      .transaction(config) {
        val account = TestAccounts.account("slack:T1/U-ann")
        SqlIdentities.enroll(Set(account)).flatMap(_ => SqlIdentities.person(account))
      }
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"ann is no person: $other")
    }

  private def record(local: String, kinds: Event.Kind*): Unit = {
    val header = Session.Header(
      ok(Harness.of("claude-code").flatMap(SessionId.of(_, local))),
      None,
      ann,
      Label.Public,
      Place.under(Namespace.Fs, Vector("/work/grit")),
      None
    )
    val events = kinds.toVector.zipWithIndex.map { (k, i) =>
      ok(Event.of(i + 1L, t0.plusSeconds(i.toLong), k, Event.Shape.Empty))
    }
    val _ = LiveDb.transaction(config)(ok(new SqlSessions().record(header, events)))
  }

  private val Sentinel = "grit_test_sentinel"

  /** What a listener on every writer's channel hears while `act` runs, up to a sentinel notice
    * sent after it: notices are delivered in commit order, so nothing `act` committed comes later.
    */
  private def heard(act: => Unit): Vector[SqlNotices.Heard] =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      (conn: Connection) =>
        SqlNotices.listen(conn, Vector(SqlNotices.Documents, SqlNotices.Sessions, Sentinel))
        act
        LiveDb.transaction(config)(ok(SqlNotices.send(Sentinel, "")))
        Iterator
          .continually(SqlNotices.drain(conn, 10.seconds))
          .scanLeft(Vector.empty[SqlNotices.Heard])(_ ++ _)
          .find(_.exists(_.channel == Sentinel))
          .getOrElse(Vector.empty)
          .filterNot(_.channel == Sentinel)
    }

  val tests = Tests {
    test("a transaction versioning two keys notices each once, as plugin/key, on grit_documents") {
      val got = heard {
        LiveDb.transaction(config) {
          write("grit/a", "one")
          write("grit/a", "two")
          write("grit/b", "one")
        }
      }
      assert(
        got == Vector(
          SqlNotices.Heard("grit_documents", "noticed/grit/a"),
          SqlNotices.Heard("grit_documents", "noticed/grit/b")
        )
      )
    }

    test("a write that changes nothing notices nothing") {
      LiveDb.transaction(config)(write("grit/same", "kept"))
      val got = heard(LiveDb.transaction(config)(write("grit/same", "kept")))
      assert(got == Vector.empty)
    }

    test(
      "a recording that logs a commit notices once, empty, on grit_sessions; one without, none"
    ) {
      val got = heard {
        record("with-commits", Event.Kind.Edited, Event.Kind.Commit, Event.Kind.Commit)
        record("without", Event.Kind.Edited, Event.Kind.Ran)
      }
      assert(got == Vector(SqlNotices.Heard("grit_sessions", "")))
    }
  }
}
