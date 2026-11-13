package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocRef, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.edge.{BriefError, EdgeSessions}
import grit.core.id.{ClaimHolder, DocKey, Harness, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.store.StoreError
import grit.core.visibility.{
  Clearance,
  Compartments,
  Grant,
  Group,
  GroupName,
  Integrity,
  Label,
  Labelled,
  RoomLabels,
  TestLabels,
  Visibility
}
import grit.dbos.sql.{
  LiveDb,
  Opener,
  SqlDocuments,
  SqlEdgeSessions,
  SqlIdentities,
  SqlJot,
  SqlSessions,
  SqlTombstones,
  TestPostgres
}

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

  private def edge: EdgeSessions = under(WorkshopTrial)

  /** The edge's sessions on [[config]]'s database, labelling places as `visibility` does. */
  private def under(visibility: Visibility): EdgeSessions = {
    val opener = new Opener(visibility)
    val ds = LiveDb.dataSource(config)
    new SqlEdgeSessions(new SqlJot(ds, opener), ds, opener)
  }

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

  /** [[WorkshopTrial]], with Nick cleared for [[TestLabels.Trial]]. */
  private val NickTrial: Visibility =
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector(Workshop -> TestLabels.Trial), Labelled.Mapped(Label.Public))
        .left
        .map(_.toString)
      team <- GroupName.of("team").left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(Group(team, Set(Nick))),
          Vector(Grant(team, TestLabels.Trial))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The edge a brief is read through, under [[NickTrial]]. */
  private def briefing: EdgeSessions = under(NickTrial)

  private def holder(text: String): ClaimHolder = ok(ClaimHolder.of(text))

  /** grit's own session `local`. */
  private def own(local: String): SessionId = ok(SessionId.of(Harness.Grit, local))

  private val documents = new SqlDocuments(new SqlTombstones)

  /** The one plugin keeping briefs here, declared enabled on first use. */
  private lazy val briefs: DocumentKeeper = {
    val terms = ok(
      DocumentTerms
        .of(
          ok(DocLabel.of("briefs")),
          DocWeight.Unscaled,
          30.days,
          100,
          0,
          DocumentTerms.Full.Withdraw
        )
    )
    val plugin = ok(PluginName.of("edge-briefs"))
    ok(LiveDb.transaction(config)(documents.declare(Vector(plugin -> terms))))
    documents.keeper(plugin, terms)
  }

  /** Keeps `holder`'s brief under `key`, written `at` in a transaction cleared to `clearance`,
    * whose floor it is kept at.
    */
  private def brief(key: String, holder: ClaimHolder, clearance: Clearance, at: Instant): Unit = {
    val _ = ok(
      LiveDb.transaction(config, clearance)(
        briefs.write(
          ok(DocKey.of(key)),
          Label.Public,
          Integrity.Member,
          fs("/work/grit"),
          ok(DocText.of(s"The brief $key.")),
          ujson.Obj(),
          None,
          at,
          Set(DocRef.brief(holder))
        )
      )
    )
  }

  /** Records, through [[briefing]] in `room`, the session `local` under `parent`, working as
    * `holder`, with an event at each of `seconds` after [[t0]].
    */
  private def worked(
      local: String,
      holder: ClaimHolder,
      room: Place,
      parent: Option[SessionId] = None
  )(seconds: Long*): Unit = {
    val _ = ok(
      briefing.record(
        Nick,
        room,
        Vector(
          EdgeSessions
            .Batch(id(local), parent, fs("/work/grit"), Some(holder), seconds.map(event).toVector)
        )
      )
    )
  }

  /** The keys of `holder`'s brief and newest session as Nick opens it in `room`, besides
    * `besides`.
    */
  private def opening(
      room: Place,
      holder: ClaimHolder,
      besides: Option[SessionId] = None
  ): Either[BriefError, (Option[String], Option[SessionId])] =
    briefing
      .briefed(Nick, room, holder, besides, t0.plusSeconds(1000))
      .map(b => (b.brief.map(d => DocKey.value(d.key)), b.lately.map(_.session.id)))

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

    test("an edge recording grit's own session is refused, keeping none of its batches") {
      val _ = nick
      val grits = ok(SessionId.of(Harness.Grit, "s-6"))
      val refused = edge.record(
        Nick,
        Place.under(Namespace.Task, Vector("elsewhere")),
        Vector(
          EdgeSessions.Batch(id("s-7"), None, fs("/work/grit"), None, Vector(event(1))),
          EdgeSessions.Batch(grits, None, fs("/work/grit"), None, Vector(event(1)))
        )
      )
      assert(
        refused == Left(
          StoreError.Invalid("session grit:s-6 is grit's own: only its turns record it")
        ),
        read("s-7") == None
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

    test(
      "briefed answers a holder's brief and the newest of its sessions by last event, of any harness, never another holder's"
    ) {
      val _ = nick
      val (h, other) = (holder("brief/one"), holder("brief/two"))
      brief("one#brief", h, Low, t0)
      worked("o-early", h, Workshop)(1, 50)
      worked("o-late", h, Workshop)(10, 20)
      worked("o-other", other, Workshop)(100)
      val byEvent = opening(Workshop, h)
      ok(
        LiveDb.transaction(config, Low)(
          store.record(
            Session.Header(own("o-grit"), None, nick, Label.Public, fs("/work/grit"), Some(h)),
            Vector(event(200))
          )
        )
      )
      assert(
        byEvent == Right((Some("one#brief"), Some(id("o-early")))),
        opening(Workshop, h) == Right((Some("one#brief"), Some(own("o-grit"))))
      )
    }

    test("briefed besides the caller's own session answers the newest other, a child among them") {
      val _ = nick
      val h = holder("brief/besides")
      worked("b-parent", h, Workshop)(1)
      worked("b-child", h, Workshop, Some(id("b-parent")))(5)
      assert(
        opening(Workshop, h) == Right((None, Some(id("b-child")))),
        opening(Workshop, h, Some(id("b-child"))) == Right((None, Some(id("b-parent")))),
        opening(Workshop, h, Some(id("b-parent"))) == Right((None, Some(id("b-child"))))
      )
    }

    test("briefed answers none of what a holder lacks") {
      val _ = nick
      val (briefOnly, sessionOnly) = (holder("brief/only"), holder("brief/session-only"))
      brief("only#brief", briefOnly, Low, t0)
      worked("l-session", sessionOnly, Workshop)(1)
      assert(
        opening(Workshop, briefOnly) == Right((Some("only#brief"), None)),
        opening(Workshop, sessionOnly) == Right((None, Some(id("l-session")))),
        opening(Workshop, holder("brief/neither")) == Right((None, None))
      )
    }

    test(
      "briefed reads as the account's person: a brief or newest session it may not read is none, and an older one it reads is answered"
    ) {
      val _ = nick
      val h = holder("brief/labelled")
      val elsewhere = Place.under(Namespace.Task, Vector("elsewhere"))
      brief("low#brief", h, Low, t0)
      brief("high#brief", h, High, t0.plusSeconds(5))
      worked("lab-low", h, elsewhere)(1)
      worked("lab-high", h, Workshop)(10)
      assert(
        opening(Workshop, h) == Right((Some("high#brief"), Some(id("lab-high")))),
        opening(elsewhere, h) == Right((Some("low#brief"), Some(id("lab-low"))))
      )
    }

    test("briefed for an account no person is linked to is NotVouched") {
      val stranger = TestAccounts.account("workshop:local/stranger-briefed")
      briefing.briefed(stranger, Workshop, holder("brief/one"), None, t0) ==>
        Left(BriefError.NotVouched(stranger))
    }
  }
}
