package grit.dbos.engine

import java.time.Instant

import grit.core.edge.EdgeSessions
import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.store.StoreError
import grit.core.visibility.{
  Clearance,
  Compartments,
  Label,
  Labelled,
  RoomLabels,
  TestLabels,
  Visibility
}
import grit.dbos.sql.{LiveDb, SqlEdgeSessions, SqlIdentities, SqlSessions, TestPostgres}

import utest.*

/** An edge's sessions recorded by the SQL store against a real Postgres: the person, label and
  * place a session begins with, and a recording kept whole or not at all.
  */
object SqlEdgeSessionsTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_edge_sessions")
    LiveEngine.open(c, "test").close()
    c
  }

  /** The room the edge records in, written at [[TestLabels.Trial]]; every other room public. */
  private val Workshop: Place = Place.under(Namespace.Task, Vector("workshop"))

  private val WorkshopTrial: Visibility =
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector(Workshop -> TestLabels.Trial), Labelled.Mapped(Label.Public))
        .left
        .map(_.toString)
      v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  private def edge: EdgeSessions = new SqlEdgeSessions(LiveDb.jot(config, WorkshopTrial))

  private val store = new SqlSessions

  private val Low: Clearance = Clearance.of(Label.Public)
  private val High: Clearance = Clearance.of(TestLabels.Trial)

  private val t0 = Instant.parse("2026-10-28T10:00:00Z")

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private val Nick: Account = TestAccounts.account("workshop:local/nick")

  private lazy val nick: Principal.Person =
    LiveDb
      .transaction(config) {
        SqlIdentities.enroll(Set(Nick)).flatMap(_ => SqlIdentities.person(Nick))
      }
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"nick is no person: $other")
    }

  private def id(local: String): SessionId =
    ok(Harness.of("claude-code").flatMap(SessionId.of(_, local)))

  private def fs(path: String): Place = Place.under(Namespace.Fs, Vector(path))

  private def event(n: Long): Event =
    ok(Event.of(n, t0.plusSeconds(n), Event.Kind.Edited, Event.Shape.Empty))

  private def read(local: String, clearance: Clearance = High): Option[Session] =
    ok(LiveDb.transaction(config, clearance)(store.read(id(local))))

  val tests = Tests {

    test(
      "each session begins with its account's person, the room's label, its parent, place and holder"
    ) {
      val _ = nick
      val slot = ClaimHolder.of("coder/abc").toOption
      val recorded = edge.record(
        Nick,
        Workshop,
        Vector(
          EdgeSessions.Batch(id("s-1"), None, fs("/work/grit"), None, Vector(event(1), event(2))),
          EdgeSessions
            .Batch(id("s-1/a-1"), Some(id("s-1")), fs("/work/spike"), slot, Vector(event(3)))
        )
      )
      val parent = read("s-1")
      val child = read("s-1/a-1")
      assert(
        recorded == Right(3),
        parent.map(s => (s.by, s.label, s.parent, s.at)) ==
          Some((nick, TestLabels.Trial, None, fs("/work/grit"))),
        child.map(s => (s.by, s.label, s.parent, s.at, s.holder)) ==
          Some((nick, TestLabels.Trial, Some(id("s-1")), fs("/work/spike"), slot)),
        parent.flatMap(_.holder) == None,
        read("s-1", Low) == None
      )
    }

    test("an account no person is linked to records nothing") {
      val stranger = TestAccounts.account("workshop:local/stranger")
      val refused = edge.record(
        stranger,
        Workshop,
        Vector(EdgeSessions.Batch(id("s-2"), None, fs("/work/grit"), None, Vector(event(1))))
      )
      assert(
        refused == Left(StoreError.Invalid("no person is linked to workshop:local/stranger")),
        read("s-2") == None
      )
    }

    test("a batch refused keeps none of the others") {
      val _ = nick
      val elsewhere = fs("/work/grit")
      // A session kept at the trial label, which a recording in a public room does not read.
      ok(
        LiveDb.transaction(config, High)(
          store.record(
            Session.Header(id("s-3"), None, nick, TestLabels.Trial, elsewhere, None),
            Vector(event(1))
          )
        )
      )
      val refused = edge.record(
        Nick,
        Place.under(Namespace.Task, Vector("elsewhere")),
        Vector(
          EdgeSessions.Batch(id("s-4"), None, elsewhere, None, Vector(event(1))),
          EdgeSessions.Batch(id("s-3"), None, elsewhere, None, Vector(event(2)))
        )
      )
      assert(
        refused == Left(
          StoreError.Invalid("session claude-code:s-3 is kept where this transaction does not read")
        ),
        read("s-4") == None
      )
    }

    test("lately reads in the room: a session kept there is read in it, and none elsewhere") {
      val _ = nick
      val edit = ok(
        Event.of(1, t0, Event.Kind.Edited, ok(Event.Shape.of(ujson.Obj("path" -> "A.scala"))))
      )
      ok(
        edge.record(
          Nick,
          Workshop,
          Vector(EdgeSessions.Batch(id("s-5"), None, fs("/work/grit"), None, Vector(edit)))
        )
      )
      val elsewhere = Place.under(Namespace.Task, Vector("elsewhere"))
      assert(
        edge.lately(Workshop, id("s-5")).map(_.map(l => (l.session.id, l.edited))) ==
          Right(Some((id("s-5"), Vector(Session.Lately.Touched("A.scala", None))))),
        edge.lately(elsewhere, id("s-5")) == Right(None),
        edge.lately(Workshop, id("s-none")) == Right(None)
      )
    }
  }
}
