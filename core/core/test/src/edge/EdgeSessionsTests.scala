package grit.core.edge

import java.time.Instant

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.TestAccounts
import grit.core.place.{Namespace, Place}
import grit.core.store.StoreError

import utest.*

/** Which sessions an edge may record: every harness's but grit's own; and what an edge built
  * without the session record answers.
  */
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

    test("an edge built without the session record briefs no holder: Unavailable, saying so") {
      val holder = ClaimHolder.of("coder/abc").fold(sys.error, identity)
      EdgeSessions.Refusing.briefed(
        TestAccounts.account("workshop:local/nick"),
        Place.under(Namespace.Task, Vector("workshop")),
        holder,
        None,
        Instant.parse("2026-11-13T10:00:00Z")
      ) ==> Left(BriefError.Unavailable("this edge was given no session record"))
    }
  }
}
