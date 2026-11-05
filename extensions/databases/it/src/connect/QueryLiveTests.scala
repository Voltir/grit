package grit.databases.connect

import scala.concurrent.duration.*

import utest.*

/** [[DatabaseReader.query]] against a live Postgres, as an attacker's statements meet it: the
  * grants, the read-only transaction, the role check, the timeout and the clips.
  */
object QueryLiveTests extends TestSuite {

  private lazy val db = AppDatabase("query_live")

  /** The grants of a reader of the leads and the applicants' unpersonal columns, which may also
    * insert into `scratch` (so that only the read-only transaction stops it).
    */
  private val Grants =
    Seq(
      "SELECT ON app.leads",
      "SELECT (id, lead_id, name) ON app.applicants",
      "INSERT ON app.scratch"
    )

  private def statement(text: String): OneStatement =
    OneStatement.of(text).getOrElse(throw new IllegalArgumentException(text))

  /** `who`'s answer to `SELECT current_user, session_user`. */
  private def who(current: String, session: String): Either[QueryRefusal, Option[Rows]] =
    Right(
      Some(
        Rows(
          Vector("current_user", "session_user"),
          Vector(Vector(Some(current), Some(session))),
          None
        )
      )
    )

  private def using[A](reader: DatabaseReader)(body: DatabaseReader => A): A =
    try body(reader)
    finally reader.close()

  val tests = Tests {
    test("a granted column answers its rows as Postgres writes them, NULL as None") {
      using(db.reader(db.login("plain", Grants))) { r =>
        assert(
          r.query(
            statement("SELECT id, external_id FROM app.leads WHERE id IN (2, 3) ORDER BY id")
          ) ==
            Right(
              Some(
                Rows(
                  Vector("id", "external_id"),
                  Vector(Vector(Some("2"), Some("x-2")), Vector(Some("3"), None)),
                  None
                )
              )
            )
        )
      }
    }

    test("a column the role was not granted is refused by Postgres, as not permitted") {
      using(db.reader(db.login("ungranted", Grants))) { r =>
        assert(
          r.query(statement("SELECT ssn FROM app.applicants")) ==
            Left(
              QueryRefusal.NotPermitted(
                Failure(Some("42501"), "permission denied for table applicants")
              )
            )
        )
      }
    }

    test(
      "a write the role is granted is refused by the read-only transaction, and nothing is written"
    ) {
      using(db.reader(db.login("writer", Grants))) { r =>
        assert(
          r.query(statement("INSERT INTO app.scratch VALUES (1)")) == Left(QueryRefusal.Writes),
          db.one("SELECT count(*) FROM app.scratch") == "0"
        )
      }
    }

    // The role check's SELECT takes the transaction's first snapshot, after which it cannot be
    // made writable.
    test("a statement making the transaction writable is refused as a write") {
      using(db.reader(db.login("rw", Grants))) { r =>
        assert(r.query(statement("SET TRANSACTION READ WRITE")) == Left(QueryRefusal.Writes))
      }
    }

    test("a role set by one query is not the next query's: it runs as the declared role") {
      val other = db.role("other_role")
      db.admin(
        s"""CREATE ROLE "$other" NOLOGIN""",
        s"""GRANT SELECT (ssn) ON app.applicants TO "$other""""
      )
      val user = db.login("setter", Grants)
      db.admin(s"""GRANT "$other" TO "$user" WITH INHERIT FALSE, SET TRUE""")
      using(db.reader(user)) { r =>
        assert(
          r.query(statement(s"""SET ROLE "$other"""")) == Right(None),
          r.query(statement("SELECT current_user, session_user")) == who(user, user)
        )
      }
    }

    test("a session authorization set by one query is not the next query's") {
      val user = db.login("superuser", Grants, "SUPERUSER")
      val plain = db.login("plain_target", Grants)
      using(db.reader(user)) { r =>
        assert(
          r.query(statement(s"""SET SESSION AUTHORIZATION "$plain"""")) == Right(None),
          r.query(statement("SELECT current_user, session_user")) == who(user, user)
        )
      }
    }

    test(
      "a reconnect that lands as another role is refused, saying who it was, before the statement"
    ) {
      val other = db.role("landed_role")
      db.admin(s"""CREATE ROLE "$other" NOLOGIN""")
      val user = db.login("reconnects", Grants)
      db.admin(s"""GRANT "$other" TO "$user" WITH INHERIT FALSE, SET TRUE""")
      using(db.reader(user)) { r =>
        db.admin(
          s"""ALTER ROLE "$user" SET role = '$other'""",
          s"SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE usename = '$user'"
        )
        assert(r.query(statement("SELECT 1")) == Left(QueryRefusal.RoleChanged(Seen(other, user))))
      }
    }

    test("a statement past the timeout is stopped, as timed out") {
      using(
        db.reader(
          db.login("sleeper", Grants),
          limits = AppDatabase.limits(timeout = 200.milliseconds)
        )
      ) { r =>
        assert(r.query(statement("SELECT pg_sleep(2)")) == Left(QueryRefusal.TimedOut))
      }
    }

    test("a result past the row limit keeps that many rows, and says so") {
      using(db.reader(db.login("many", Grants), limits = AppDatabase.limits(rows = 3))) { r =>
        assert(
          r.query(statement("SELECT id FROM app.leads ORDER BY id")) ==
            Right(
              Some(
                Rows(
                  Vector("id"),
                  Vector(Vector(Some("1")), Vector(Some("2")), Vector(Some("3"))),
                  Some(Clip.Rows)
                )
              )
            )
        )
      }
    }

    // The name "id" is 2 bytes and each id 1: two rows make 4, a third would make 5.
    test("a result past the byte limit keeps the whole rows that fit, and says so") {
      using(db.reader(db.login("wide", Grants), limits = AppDatabase.limits(bytes = 4))) { r =>
        assert(
          r.query(statement("SELECT id FROM app.leads ORDER BY id")) ==
            Right(
              Some(
                Rows(Vector("id"), Vector(Vector(Some("1")), Vector(Some("2"))), Some(Clip.Bytes))
              )
            )
        )
      }
    }

    // An application may hold a worker's lease as a session advisory lock: one a query takes must
    // not outlive it on grit's connection, or the worker waits for it forever.
    test("an advisory lock a query takes is released when it ends, whichever way it was taken") {
      using(db.reader(db.login("locker", Grants))) { r =>
        val taken =
          Right(Some(Rows(Vector("pg_try_advisory_lock"), Vector(Vector(Some("t"))), None)))
        assert(
          r.query(statement("SELECT pg_advisory_lock(42)")).isRight,
          r.query(statement("SELECT pg_try_advisory_lock(43)")) == taken,
          db.one("SELECT pg_try_advisory_lock(42) AND pg_try_advisory_lock(43)") == "t"
        )
      }
    }

    test("an answer past the driver's buffer is refused as too large, and the next query answers") {
      using(db.reader(db.login("huge", Grants), limits = AppDatabase.limits(bytes = 1024))) { r =>
        assert(
          r.query(statement("SELECT repeat('x', 5000000)")) == Left(QueryRefusal.TooLarge),
          r.query(statement("SELECT 1 AS one")) == Right(
            Some(Rows(Vector("one"), Vector(Vector(Some("1"))), None))
          )
        )
      }
    }
  }
}
