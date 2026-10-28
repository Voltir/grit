package grit.dbos.engine

import grit.core.clock.Clock
import grit.core.store.Db
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.internal.Reader
import grit.dbos.sql.{DbConfig, Pool, SqlStatements, TestPostgres}

import utest.*

/** An engine's, a link's and a reader's own transactions share one bounded pool of sessions,
  * rather than each opening one.
  */
object PoolLiveTests extends TestSuite {

  private val Reads = 20

  /** Each session [[Reads]] reads through `db` ran on: its backend pid and application name. */
  private def sessions(db: Db): Set[(Int, String)] =
    Vector
      .fill(Reads) {
        db.read(Subject.Public)(
          SqlStatements.one("SELECT pg_backend_pid(), current_setting('application_name')")(_ =>
            ()
          )(rs => (rs.getInt(1), rs.getString(2)))
        ) match {
          case Right(Some(session)) => session
          case other => throw new java.lang.AssertionError(s"unread: $other")
        }
      }
      .toSet

  /** A database an engine has opened once, migrating both schemas, and closed. */
  private def migrated(name: String): DbConfig = {
    val config = TestPostgres.freshDatabase(name)
    LiveEngine.open(config, "test").close()
    config
  }

  private def shared(seen: Set[(Int, String)]): Unit = {
    assert(seen.size <= Pool.Size)
    seen.map(_._2) ==> Set(Pool.ApplicationName)
  }

  val tests: Tests = Tests {
    test("an engine's transactions share at most the pool's sessions, named as the pool's") {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("pool_engine"), "test")
      try shared(sessions(engine.db))
      finally engine.close()
    }

    test("a link's transactions share at most the pool's sessions, named as the pool's") {
      val link =
        Link.attach(
          migrated("pool_link"),
          "test",
          LiveEngine.Identity,
          LiveEngine.Uncapped,
          Visibility.Shipped,
          Clock.system()
        )
      try shared(sessions(link.db))
      finally link.close()
    }

    test("a reader's transactions share at most the pool's sessions, named as the pool's") {
      val reader = Reader.open(migrated("pool_reader"))
      try shared(sessions(reader.db))
      finally reader.close()
    }
  }
}
