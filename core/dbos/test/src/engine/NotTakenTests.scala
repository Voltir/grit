package grit.dbos.engine

import java.time.Instant

import utest.*

/** What a process refused the engine lock tells a person. */
object NotTakenTests extends TestSuite {

  val tests = Tests {
    test("a refusal names the holder's pid and machine, and how long ago it last beat") {
      val holder = Holder(
        "cave",
        41233,
        "2026-09-27",
        Instant.parse("2026-09-27T14:02:11Z"),
        Instant.parse("2026-09-27T14:05:00Z")
      )
      val said = NotTaken.Held(Some(holder)).message(Instant.parse("2026-09-27T14:05:04Z"))
      assert(
        said.startsWith("grit is already running on this database: pid 41233 on cave, started ")
      )
      assert(
        said.endsWith(
          ", last heartbeat 4s ago. Quit it, or set GRIT_DATABASE_URL to another database."
        )
      )
    }

    test("a holder that has not written its row is still a refusal, without a name") {
      NotTaken.Held(None).message(Instant.EPOCH) ==>
        "grit is already running on this database (it has not said where yet)."
    }
  }
}
