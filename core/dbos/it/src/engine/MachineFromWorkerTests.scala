package grit.dbos.engine

import java.time.Instant

import scala.io.Source
import scala.util.Using

import grit.core.id.{Harness, SessionId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.visibility.Label
import grit.dbos.sql.{DbConfig, LiveDb, SqlIdentities, SqlSessions, TestPostgres}

import utest.*

/** `machine-from-worker.sql`, the one-off that rewrites stored places from the key `worker` to
  * `machine`, run on a database holding a harness's session as the commands edge wrote it before
  * the rename: its row at a worktree under `worker`, its log's shapes naming it, two of them at
  * the 300-byte bound, a place row, a visibility change, a document, a file and a piece set at
  * it.
  */
object MachineFromWorkerTests extends TestSuite {

  private val migration: String =
    Using.resource(
      Option(getClass.getResourceAsStream("/machine-from-worker.sql"))
        .getOrElse(sys.error("no machine-from-worker.sql"))
    )(Source.fromInputStream(_).mkString)

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private val sessions = new SqlSessions

  private val t0 = Instant.parse("2026-11-14T10:00:00Z")

  /** The worktree the session worked in, as the code after the rename places it. */
  private val At: Place = Place.under(Namespace.Machine, Vector("desktop", "/work/c"))

  /** [[At]] as the code before the rename wrote it. */
  private val Was: String = "worker:" + At.written.stripPrefix("machine:")

  /** [[At]]'s segments as the code before the rename stored them, as an SQL array. */
  private val WasArray: String =
    ("worker" +: At.segments.drop(1)).map(s => s"'$s'").mkString("ARRAY[", ", ", "]")

  private val Id: SessionId = ok(Harness.of("claude-code").flatMap(SessionId.of(_, "s1")))

  private val ShortSha = "1a2b3c4d5e6f"
  private val ShortSubject = "core: a short one"
  private val LongSha = "9f8e7d6c5b4a"
  private val Edited = "core/src/A.scala"

  /** `fields` as one compact JSON object, padded under `pad` to exactly `bytes` when given. */
  private def shape(fields: Vector[(String, String)], pad: Option[(String, Int)] = None): String = {
    def written(fs: Vector[(String, String)]) =
      ujson.write(ujson.Obj.from(fs.map((k, v) => k -> ujson.Str(v))))
    pad.fold(written(fields)) { (key, bytes) =>
      val bare = written(fields :+ (key -> ""))
      val padded = written(fields :+ (key -> ("y" * (bytes - bare.length))))
      assert(padded.getBytes("UTF-8").length == bytes)
      padded
    }
  }

  /** Each logged line as the commands edge wrote it: its seq, kind and shape, under `worker`. */
  private val Lines: Vector[(Int, String, String)] = Vector(
    (2, "commit", shape(Vector("worktree" -> Was, "sha" -> ShortSha, "subject" -> ShortSubject))),
    (3, "commit", shape(Vector("worktree" -> Was, "sha" -> LongSha), Some("subject" -> 300))),
    (4, "edited", shape(Vector("path" -> Edited, "worktree" -> Was))),
    (5, "edited", shape(Vector("worktree" -> Was), Some("path" -> 300)))
  )

  /** A fresh database holding the session and rows of this suite's doc, as written before. */
  private def before(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    val ann = LiveDb
      .transaction(c) {
        val account = TestAccounts.account("slack:T1/U-ann")
        SqlIdentities.enroll(Set(account)).flatMap(_ => SqlIdentities.person(account))
      }
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"ann is no person: $other")
    }
    val started = ok(Event.of(1, t0, Event.Kind.Started, Event.Shape.Empty))
    val _ = LiveDb.transaction(c)(
      sessions.record(Session.Header(Id, None, ann, Label.Public, At, None), Vector(started))
    )
    val lines = Lines
      .map((seq, kind, s) =>
        s"(SELECT id FROM grit.sessions WHERE local = 's1'), $seq, $seq, " +
          s"'${t0.plusSeconds(seq.toLong)}', '$kind', '${s.replace("'", "''")}'"
      )
      .mkString("(", "), (", ")")
    sql(
      c,
      s"""UPDATE grit.sessions SET place[1] = 'worker';
         |INSERT INTO grit.session_events (session, seq, n, at, kind, shape) VALUES $lines;
         |INSERT INTO grit.places (path) VALUES ($WasArray);
         |INSERT INTO grit.visibility_changes (at, by, change)
         |  SELECT now(), account, '{"kind":"quiet","room":"$Was"}'::jsonb
         |    FROM grit.identities ORDER BY account LIMIT 1;
         |INSERT INTO grit.documents (plugin, key, place, written_at, last_placed, integrity_id)
         |  VALUES ('notes', 'a', $WasArray, now(), now(), 1);
         |INSERT INTO grit.files
         |  (sha256, bytes, name, edge, ref, fetched_at, expires_at, place, vouched_id)
         |  VALUES (sha256('a'::bytea), 1, 'a.txt', 'local', 'a.txt', now(), now(), $WasArray, 1);
         |INSERT INTO grit.piece_sets (plugin, ref, name, revision, place, classification_id,
         |    integrity_id, written_at, last_placed, unread)
         |  VALUES ('notes', 'a', 'a', '1', $WasArray, 1, 1, now(), now(), 'unread');""".stripMargin
    )
    c
  }

  /** Runs `statements` on their own connection, committing each as it goes. */
  private def sql(config: DbConfig, statements: String): Unit =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    )(conn => Using.resource(conn.createStatement())(_.execute(statements)))

  /** The file run on a connection of its own, then `end`: the counts it printed, by what. */
  private def run(config: DbConfig, end: String): Map[String, Long] =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(conn.createStatement()) { st =>
        val _ = st.execute(migration)
        val printed = Using.resource(st.executeQuery("SELECT what, n FROM machine_from_worker")) {
          rs =>
            val out = Map.newBuilder[String, Long]
            while (rs.next()) out += (rs.getString(1) -> rs.getLong(2))
            out.result()
        }
        val _ = st.execute(end)
        printed
      }
    }

  /** What a query selects, each row's first column as text. */
  private def column(config: DbConfig, query: String): Vector[String] =
    LiveDb.connected(config) { c =>
      Using.resource(c.createStatement()) { st =>
        Using.resource(st.executeQuery(query)) { rs =>
          val out = Vector.newBuilder[String]
          while (rs.next()) out += rs.getString(1)
          out.result()
        }
      }
    }

  /** Every stored value that holds the place, as text, in a fixed order. */
  private def stored(config: DbConfig): Vector[String] =
    column(config, "SELECT array_to_string(place, '|') FROM grit.sessions") ++
      column(
        config,
        "SELECT array_to_string(path, '|') FROM grit.places WHERE path[2] = 'desktop'"
      ) ++
      Vector("documents", "files", "piece_sets").flatMap(t =>
        column(config, s"SELECT array_to_string(place, '|') FROM grit.$t")
      ) ++
      column(config, "SELECT change ->> 'room' FROM grit.visibility_changes") ++
      column(config, "SELECT shape::text FROM grit.session_events WHERE seq > 1 ORDER BY seq")

  private val FirstRun: Map[String, Long] = Map(
    "grit.places rewritten" -> 1,
    "grit.sessions rewritten" -> 1,
    "grit.documents rewritten" -> 1,
    "grit.files rewritten" -> 1,
    "grit.piece_sets rewritten" -> 1,
    "grit.visibility_changes rewritten" -> 1,
    "grit.session_events rewritten" -> 3,
    "grit.session_events rewritten without their subject" -> 1,
    "grit.session_events deleted, too long to rewrite" -> 1
  )

  val tests = Tests {

    test("a dry run prints what the run would change, and changes nothing") {
      val c = before("machine_from_worker_dry")
      val was = stored(c)
      run(c, "ROLLBACK") ==> FirstRun
      stored(c) ==> was
    }

    test(
      "the run rewrites each place under worker to machine as the code writes it, a commit at the bound without its subject, an edit at the bound deleted"
    ) {
      val c = before("machine_from_worker_run")
      run(c, "COMMIT") ==> FirstRun
      val segments = At.segments.mkString("|")
      stored(c) ==> Vector.fill(5)(segments) ++ Vector(At.written) ++ Vector(
        Event.Shape.committed(At, ShortSha, Some(ShortSubject)),
        Event.Shape.committed(At, LongSha, None),
        Event.Shape.of(ujson.Obj("path" -> Edited, "worktree" -> At.written))
      ).map(s => Event.Shape.value(ok(s)))
      val lately = LiveDb.transaction(c)(ok(sessions.lately(Id)))
      (
        lately.map(_.session.at),
        lately.map(_.commits.map(m => (m.sha, m.subject))),
        lately.map(_.edited)
      ) ==> (
        Some(At),
        Some(Vector(LongSha -> None, ShortSha -> Some(ShortSubject))),
        Some(Vector(Session.Lately.Touched(Edited, Some(At))))
      )
    }

    test("a second run changes nothing, and prints that it changed nothing") {
      val c = before("machine_from_worker_again")
      val _ = run(c, "COMMIT")
      val once = stored(c)
      run(c, "COMMIT") ==> FirstRun.map((what, _) => what -> 0L)
      stored(c) ==> once
    }
  }
}
