package grit.dbos

import grit.core.Origin
import utest.*

object OriginJsonTests extends TestSuite {

  // The stored form is the conversation's unique key: these pin it, so a
  // change that would orphan existing conversations fails here first.
  val tests = Tests {
    test("tui") {
      SqlConversationStore.originJson(Origin.Tui("s1")) ==>
        ujson.Obj("kind" -> "tui", "session" -> "s1")
    }

    test("slack") {
      SqlConversationStore.originJson(Origin.Slack("T1", "C1", "1700000000.000100")) ==>
        ujson.Obj(
          "kind" -> "slack",
          "team" -> "T1",
          "channel" -> "C1",
          "threadTs" -> "1700000000.000100"
        )
    }

    test("task") {
      SqlConversationStore.originJson(Origin.Task("e2e", "2026-09-23")) ==>
        ujson.Obj("kind" -> "task", "name" -> "e2e", "run" -> "2026-09-23")
    }
  }
}
