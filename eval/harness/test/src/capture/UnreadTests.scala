package grit.eval.harness.capture

import grit.core.store.StoreError

import utest.*

/** How the harness says a failed read ([[Unread]]). */
object UnreadTests extends TestSuite {

  val tests = Tests {
    test("a failed read is said by what was read, then unread, then the failure's kind alone") {
      Unread.said("steps of turn-7")(StoreError.DatabaseError("connection reset by 10.0.0.4")) ==>
        "steps of turn-7 unread: database error"
    }
  }
}
