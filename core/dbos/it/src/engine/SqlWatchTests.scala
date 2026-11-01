package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant
import java.util.concurrent.{Callable, Executors, Future, TimeUnit}

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.document.{DocLabel, DocRef, DocText, DocWeight, DocumentTerms, Written}
import grit.core.edge.{EdgeWatch, WatchError, WatchFor, Watched}
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, Harness, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels, Visibility}
import grit.dbos.sql.{
  LiveDb,
  Opener,
  Pool,
  SqlDocuments,
  SqlIdentities,
  SqlSessions,
  SqlTombstones,
  TestPostgres
}

import utest.*

/** A watch kept by the SQL listener against a real Postgres: what meets it, when, as whom, and
  * how many run at once.
  */
object SqlWatchTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_watch")
    LiveEngine.open(c, "test").close()
    c
  }

  /** `body` given a watch over a pool of its own, both closed after. */
  private def watching[A](body: SqlWatch^ => A): A = {
    val dataSource = Pool.open(config, readOnly = false)
    try {
      val w = new SqlWatch(config, dataSource, new Opener(Visibility.Shipped))
      try body(w)
      finally w.close()
    } finally dataSource.close()
  }

  private val threads = Executors.newCachedThreadPool()

  override def utestAfterAll(): Unit = {
    val _ = threads.shutdownNow()
  }

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private val Terms: DocumentTerms =
    ok(DocumentTerms.of(ok(DocLabel.of("notes")), DocWeight.Unscaled, 2.minutes, 10))

  private val plugin: PluginName = ok(PluginName.of("projects"))

  private val keeper = new SqlDocuments(new SqlTombstones).keeper(plugin, Terms)

  private val t0 = Instant.parse("2026-10-29T10:00:00Z")

  private val Room: Place = Place.under(Namespace.Task, Vector.empty)

  private def state(s: String): DocRef = ok(DocRef.of("state", s))

  /** A version of `key` holding the state `s`, kept at `label`. */
  private def write(key: String, s: String, label: Label = Label.Public): DocumentVersionId =
    LiveDb.transaction(config, Clearance.of(label)) {
      ok(
        keeper.write(
          ok(DocKey.of(key)),
          label,
          Integrity.Member,
          Room,
          ok(DocText.of(s"$key in $s")),
          ujson.Obj(),
          None,
          t0,
          Set(state(s))
        )
      ) match {
        case Written.Versioned(version, _, _) => version
        case other => throw new java.lang.AssertionError(s"$key was not versioned: $other")
      }
    }

  private val annAccount: Account = TestAccounts.account("slack:T1/U-ann")

  private lazy val ann: Principal.Person =
    LiveDb
      .transaction(config) {
        SqlIdentities.enroll(Set(annAccount)).flatMap(_ => SqlIdentities.person(annAccount))
      }
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"ann is no person: $other")
    }

  private def reaching(key: String, states: String*): WatchFor =
    ok(WatchFor.Holding.of(plugin, ok(DocKey.of(key)), states.map(state).toSet))

  /** How long a watch here waits: met, it returns long before; unmet, it lapses within a test's
    * wait for it.
    */
  private val Patience = 20.seconds

  private def until(
      w: SqlWatch^,
      what: WatchFor,
      within: FiniteDuration
  ): Either[WatchError, Watched] = {
    val _ = ann
    w.until(annAccount, Room, what, within)
  }

  /** [[until]] for `within` on a thread of its own, returned once the watch has read, found nothing, and waits
    * to be rung, its listener listening since before it began: only a notice can wake it then.
    */
  private def watched(
      w: SqlWatch^,
      what: WatchFor,
      within: FiniteDuration = Patience
  ): Future[Either[WatchError, Watched]] = {
    if (!w.listening) {
      val _ = until(w, reaching("grit/none", "none"), Duration.Zero)
      eventually(w.listening)
    }
    val before = w.waiting
    val f = threads.submit(new Callable[Either[WatchError, Watched]] {
      def call(): Either[WatchError, Watched] = until(w, what, within)
    })
    eventually(w.waiting > before)
    f
  }

  /** What `f` came to within 10 seconds, half a watch's [[Patience]]: `None` when it had not, so
    * a watch met only by its last read, as it lapses, is not taken for one a notice rang.
    */
  private def outcome(
      f: Future[Either[WatchError, Watched]]
  ): Option[Either[WatchError, Watched]] =
    try Some(f.get(10, TimeUnit.SECONDS))
    catch { case _: java.util.concurrent.TimeoutException => None }

  private def eventually(cond: => Boolean): Unit = {
    val deadline = System.nanoTime() + 10.seconds.toNanos
    while (!cond && System.nanoTime() < deadline) Thread.sleep(10)
    if (!cond) throw new java.lang.AssertionError("not within 10 seconds")
  }

  private def worktree(path: String): Place = Place.under(Namespace.Fs, Vector(path))

  private def session(local: String): SessionId =
    ok(Harness.of("claude-code").flatMap(SessionId.of(_, local)))

  private def shape(path: String, n: Long): Event.Shape =
    ok(Event.Shape.committed(worktree(path), s"sha-$n", None))

  /** A commit in `path` at `at`, logged on the session `local`, at a worktree of its own, working
    * as `holder`.
    */
  private def commit(local: String, holder: String, path: String, at: Instant, n: Long): Unit = {
    val header = Session.Header(
      session(local),
      None,
      ann,
      Label.Public,
      worktree("/work/parent"),
      Some(ok(ClaimHolder.of(holder)))
    )
    val event = ok(Event.of(n, at, Event.Kind.Commit, shape(path, n)))
    val _ = LiveDb.transaction(config, Clearance.of(Label.Public))(
      ok(new SqlSessions().record(header, Vector(event)))
    )
  }

  val tests = Tests {
    test("a watch met already is met at once, with the refs it was met by") {
      val version = write("grit/met", "review")
      val got = watching(w => until(w, reaching("grit/met", "review", "merged"), Patience))
      assert(got == Right(Watched.Held(version, Set(state("review")), t0)))
    }

    test("a watch is met by a write committed after it began") {
      write("grit/later", "build")
      val (version, got) = watching { w =>
        val f = watched(w, reaching("grit/later", "review"))
        val v = write("grit/later", "review")
        (v, outcome(f))
      }
      assert(got == Some(Right(Watched.Held(version, Set(state("review")), t0))))
    }

    test("a write the watcher does not read never meets its watch, read before or rung after") {
      write("grit/hidden", "review", TestLabels.Trial)
      val (before, after) = watching { w =>
        val before = until(w, reaching("grit/hidden", "review"), Duration.Zero)
        val f = watched(w, reaching("grit/hidden", "merged"), 2.seconds)
        val _ = write("grit/hidden", "merged", TestLabels.Trial)
        (before, outcome(f))
      }
      (before, after) ==> ((Right(Watched.Lapsed), Some(Right(Watched.Lapsed))))
    }

    test("a commit meets a watch on its worktree or its session's holder, from since on") {
      val at = t0.plusSeconds(60)
      commit("parent-1", "fixer", "/work/fixer", at, 1L)
      val met = Right(Watched.Committed(session("parent-1"), at, shape("/work/fixer", 1L)))
      val got = watching { w =>
        def from(by: WatchFor.Worker, since: Instant) =
          until(w, WatchFor.Committed(by, since), Duration.Zero)
        Vector(
          from(WatchFor.Worker.At(worktree("/work/fixer")), at),
          from(WatchFor.Worker.As(ok(ClaimHolder.of("fixer"))), at),
          from(WatchFor.Worker.At(worktree("/work/parent")), at),
          from(WatchFor.Worker.As(ok(ClaimHolder.of("other"))), at),
          from(WatchFor.Worker.At(worktree("/work/fixer")), at.plusSeconds(1))
        )
      }
      val lapsed = Right(Watched.Lapsed)
      assert(got == Vector(met, met, lapsed, lapsed, lapsed))
    }

    test("a commit logged after a watch began meets it") {
      val since = t0.plusSeconds(600)
      val got = watching { w =>
        val f = watched(w, WatchFor.Committed(WatchFor.Worker.At(worktree("/work/late")), since))
        commit("late-1", "late", "/work/late", since.plusSeconds(1), 1L)
        outcome(f)
      }
      val met = Watched.Committed(session("late-1"), since.plusSeconds(1), shape("/work/late", 1L))
      assert(got == Some(Right(met)))
    }

    test(
      "the watch past the most under way is refused busy; one write meets every one on its key"
    ) {
      write("grit/crowd", "build")
      val (refused, version, met) = watching { w =>
        val crowd = Vector.fill(EdgeWatch.Most)(watched(w, reaching("grit/crowd", "review")))
        val refused = until(w, reaching("grit/crowd", "review"), Patience)
        val version = write("grit/crowd", "review")
        (refused, version, crowd.map(outcome).distinct)
      }
      assert(
        refused == Left(WatchError.Busy),
        met == Vector(Some(Right(Watched.Held(version, Set(state("review")), t0))))
      )
    }

    test("a write while the listener is down meets a watch once it listens again") {
      write("grit/dropped", "build")
      val (version, got) = watching { w =>
        val f = watched(w, reaching("grit/dropped", "review"))
        Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
          c =>
            Using.resource(c.createStatement())(
              _.execute(
                s"""SELECT pg_terminate_backend(pid) FROM pg_stat_activity
                   | WHERE application_name = '${SqlWatch.ApplicationName}'
                   |   AND datname = current_database()""".stripMargin
              )
            )
        }
        // Written while the listener is down, so only listening again can ring the watch.
        eventually(!w.listening)
        val v = write("grit/dropped", "review")
        (v, outcome(f))
      }
      assert(got == Some(Right(Watched.Held(version, Set(state("review")), t0))))
    }

    test("an account no person is linked to is refused") {
      val nobody = TestAccounts.account("slack:T1/U-nobody")
      val got = watching(_.until(nobody, Room, reaching("grit/met", "review"), Duration.Zero))
      assert(got == Left(WatchError.NotVouched(nobody)))
    }
  }
}
