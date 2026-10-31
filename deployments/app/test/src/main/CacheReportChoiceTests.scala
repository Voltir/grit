package grit.app.main

import grit.core.id.ConversationId

import utest.*

/** Which conversation `grit cache-report` reports, read from its arguments alone. */
object CacheReportChoiceTests extends TestSuite {

  val tests = Tests {
    test("an id names its conversation, no id the chat's here, and other arguments no report") {
      (
        Main.cacheReported(Vector("cache-report", "c1")),
        Main.cacheReported(Vector("cache-report")),
        Main.cacheReported(Vector("cache-report", "c1", "c2")),
        Main.cacheReported(Vector("hello"))
      ) ==> (Some(Some(ConversationId("c1"))), Some(None), None, None)
    }
  }
}
