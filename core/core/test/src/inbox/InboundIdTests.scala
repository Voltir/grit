package grit.core.inbox

import grit.core.id.{ConversationId, EntryId, SourceId}

import utest.*

object InboundIdTests extends TestSuite {

  private val conversation = ConversationId("0199a1b2-c3d4-7e5f-8a9b-0c1d2e3f4a5b")

  val tests = Tests {
    test("an inbound id reads back as the conversation and source it was made from") {
      Vector(SourceId("1712.345678"), SourceId("tui:7"), SourceId("")).foreach { s =>
        InboundId.source(InboundId.of(conversation, s)) ==> Some((conversation, s))
      }
    }

    test("an inbound id is written in:<conversation>:<source>") {
      EntryId.value(InboundId.of(conversation, SourceId("1712.345678"))) ==>
        "in:0199a1b2-c3d4-7e5f-8a9b-0c1d2e3f4a5b:1712.345678"
    }

    test("an id that is not an inbound one reads as None") {
      Vector("e1", "in:", "in:nocolon", "out:c:s", "c1:e0").foreach { id =>
        InboundId.source(EntryId(id)) ==> None
      }
    }
  }
}
