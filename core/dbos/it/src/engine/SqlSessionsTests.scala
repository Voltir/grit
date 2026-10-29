package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, CyclicBarrier, TimeUnit}

import scala.jdk.CollectionConverters.*

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Log, Session, Sessions}
import grit.core.store.StoreError
import grit.core.visibility.{Clearance, Label, TestLabels}
import grit.dbos.sql.{LiveDb, SqlIdentities, SqlSessions, TestPostgres}

import utest.*

/** Sessions kept by the SQL store against a real Postgres: the row a recording begins and moves,
  * its log, and who reads it. Tests share the database, so each names its own session.
  */
object SqlSessionsTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_sessions")
    LiveEngine.open(c, "test").close()
    c
  }

  private val sessions = new SqlSessions

  private val Low: Clearance = Clearance.of(Label.Public)
  private val High: Clearance = Clearance.of(TestLabels.Trial)

  private val t0 = Instant.parse("2026-10-28T10:00:00Z")

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

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

  private def id(local: String): SessionId =
    ok(Harness.of("claude-code").flatMap(SessionId.of(_, local)))

  private val Worktree: Place = Place.under(Namespace.Fs, Vector("/work/grit"))

  private def header(
      local: String,
      label: Label,
      holder: Option[ClaimHolder]
  ): Session.Header =
    Session.Header(id(local), None, ann, label, Worktree, holder)

  private def event(n: Long, kind: Event.Kind = Event.Kind.Edited): Event =
    ok(Event.of(n, t0.plusSeconds(n), kind, ok(Event.Shape.of(ujson.Obj("n" -> n.toInt)))))

  private def record(
      local: String,
      events: Vector[Event],
      label: Label = Label.Public,
      clearance: Clearance = Low,
      holder: Option[ClaimHolder] = None
  ): Either[StoreError, Sessions.Recorded] =
    LiveDb.transaction(config, clearance)(sessions.record(header(local, label, holder), events))

  private def read(local: String, clearance: Clearance = Low): Option[Session] =
    LiveDb.transaction(config, clearance)(ok(sessions.read(id(local))))

  /** The session's log: each line's seq, n and kind, in seq order. */
  private def log(local: String): Vector[(Int, Long, String)] =
    LiveDb.connected(config) { c =>
      val ps = c.prepareStatement(
        """SELECT e.seq, e.n, e.kind FROM grit.session_events e
          |  JOIN grit.sessions s ON s.id = e.session
          | WHERE s.local = ? ORDER BY e.seq""".stripMargin
      )
      ps.setString(1, local)
      val rs = ps.executeQuery()
      val lines = Vector.newBuilder[(Int, Long, String)]
      while (rs.next()) lines += ((rs.getInt(1), rs.getLong(2), rs.getString(3)))
      lines.result()
    }

  private val summary = ok(Session.Summary.of("built the log"))

  private val oneOfEach: Vector[Event] = Vector(
    event(1, Event.Kind.Started),
    event(2, Event.Kind.Read(100)),
    event(3, Event.Kind.Edited),
    event(4, Event.Kind.Ran),
    event(5, Event.Kind.Commit),
    event(6, Event.Kind.Closing(summary)),
    event(7, Event.Kind.Ended)
  )

  val tests = Tests {
    test("a first recording begins the row from its header, logs every kind and counts it") {
      val recorded = record("s-first", oneOfEach.reverse)
      assert(
        recorded == Right(Sessions.Recorded(7, full = false)),
        log("s-first") == oneOfEach.zipWithIndex.map((e, i) => (i + 1, e.n, e.kind.name)),
        read("s-first") == Some(
          Session(
            id("s-first"),
            None,
            ann,
            Label.Public,
            at = Worktree,
            started = t0.plusSeconds(1),
            ended = Some(t0.plusSeconds(7)),
            lastEvent = t0.plusSeconds(7),
            summary = Some(summary),
            counters = ok(Session.Counters.read(4, 1, 1, 100)),
            full = false,
            holder = None
          )
        )
      )
    }

    test("a session works as the holder the first header naming one gives, and keeps it") {
      val first = ok(ClaimHolder.of("coder/first"))
      val second = ok(ClaimHolder.of("coder/second"))
      record("s-holder", Vector(event(1)))
      val none = read("s-holder").flatMap(_.holder)
      record("s-holder", Vector(event(2)), holder = Some(first))
      record("s-holder", Vector(event(3)), holder = Some(second))
      record("s-holder", Vector(event(4)))
      assert(none == None, read("s-holder").flatMap(_.holder) == Some(first))
    }

    test("recording the same events again changes nothing") {
      val events = Vector(event(1), event(2, Event.Kind.Read(10)))
      ok(record("s-again", events))
      val before = read("s-again")
      val again = record("s-again", events :+ event(2, Event.Kind.Read(10)))
      assert(
        again == Right(Sessions.Recorded(0, full = false)),
        read("s-again") == before,
        log("s-again") == Vector((1, 1L, "edited"), (2, 2L, "read"))
      )
    }

    test("recordings of one session at once each take the next seqs, none refused") {
      ok(record("s-race", Vector(event(1))))
      // Each round's recordings start together, so without the row's lock two read one next seq.
      val threads = 4
      val rounds = 10
      val start = new CyclicBarrier(threads)
      val refused = new ConcurrentLinkedQueue[StoreError]()
      val workers = (0 until threads).map { t =>
        new Thread(() => {
          (0 until rounds).foreach { r =>
            start.await(10, TimeUnit.SECONDS)
            record("s-race", Vector(event(2L + r * threads + t))).left.foreach(refused.add(_))
          }
        })
      }
      workers.foreach(_.start())
      workers.foreach(_.join())
      val recorded = (1 + threads * rounds).toLong
      assert(
        refused.asScala.toVector == Vector.empty,
        log("s-race").map(_._1) == (1 to recorded.toInt).toVector,
        log("s-race").map(_._2).sorted == (1L to recorded).toVector,
        read("s-race").map(_.counters) == Some(ok(Session.Counters.read(recorded, recorded, 0, 0)))
      )
    }

    test("past the hard cap one overflow stands for every event after it, uncounted") {
      val offered = (1L to (Log.HardCap + 1).toLong).map(event(_)).toVector
      val filled = record("s-full", offered)
      val later = record("s-full", Vector(event(Log.HardCap + 7L, Event.Kind.Ended)))
      val lines = log("s-full")
      val session = read("s-full")
      assert(
        filled == Right(Sessions.Recorded(Log.HardCap, full = true)),
        later == Right(Sessions.Recorded(0, full = true)),
        lines.size == Log.HardCap,
        lines.lastOption == Some((Log.HardCap, Log.HardCap.toLong, "overflow")),
        session.map(_.counters) == Some(
          ok(Session.Counters.read(Log.HardCap - 1, Log.HardCap - 1, 0, 0))
        ),
        session.map(s => (s.full, s.ended, s.lastEvent)) ==
          Some((true, None, t0.plusSeconds(Log.HardCap + 7L)))
      )
    }

    test("a session kept where a transaction does not read is refused to it, and not read") {
      ok(record("s-high", Vector(event(1)), TestLabels.Trial, High))
      val refused = record("s-high", Vector(event(2)), TestLabels.Trial, Low)
      assert(
        refused == Left(
          StoreError.Invalid(
            "session claude-code:s-high is kept where this transaction does not read"
          )
        ),
        read("s-high", Low) == None,
        read("s-high", High).map(_.lastEvent) == Some(t0.plusSeconds(1))
      )
    }
  }
}
