package grit.dbos.engine

import grit.core.store.StoreError
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** Every engine start is recorded, with the build it ran (`grit.engine_starts`). */
object EngineStartsTests extends TestSuite {

  val tests = Tests {
    test("two engine starts on one database leave two rows, in order, with this build") {
      val config = TestPostgres.freshDatabase("engine_starts")
      LiveEngine.open(config, "first").close()
      LiveEngine.open(config, "second").close()
      val starts = LiveDb.transaction(config)(EngineStarts.all()) match {
        case Right(rows) => rows
        case Left(e: StoreError) => sys.error(s"reading the starts: $e")
      }
      starts.map(s => (s.epoch, s.machine, s.pid, s.build)) ==> Vector(
        ("first", LiveEngine.Identity.machine, LiveEngine.Identity.pid, Build.current),
        ("second", LiveEngine.Identity.machine, LiveEngine.Identity.pid, Build.current)
      )
      val at = starts.map(_.at)
      assert(at.zip(at.drop(1)).forall(_.isBefore(_)))
    }

    test("this suite's classpath carries the resource Mill writes, so the build is known") {
      assertMatch(Build.current) { case Build.Known(_, _) => }
    }
  }
}
