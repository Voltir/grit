package grit.core.edge

import grit.core.id.{Harness, SessionId}
import grit.core.store.StoreError

import utest.*

/** Which sessions an edge may record: every harness's but grit's own. */
object EdgeSessionsTests extends TestSuite {

  private def session(harness: Harness): SessionId =
    SessionId.of(harness, "c1").fold(sys.error, identity)

  val tests = Tests {
    test("an edge may not record grit's own session, and may record any other harness's") {
      val claudeCode = Harness.of("claude-code").fold(sys.error, identity)
      assert(
        EdgeSessions.refused(session(Harness.Grit)) ==
          Some(StoreError.Invalid("session grit:c1 is grit's own: only its turns record it")),
        EdgeSessions.refused(session(claudeCode)).isEmpty
      )
    }
  }
}
