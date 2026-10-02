package grit.eval.harness.corpus

import grit.core.id.{ConversationId, EntryId, SourceId}
import grit.core.inbox.InboundId
import grit.core.place.Directory
import grit.core.store.Origin

import utest.*

/** A case's identity read from a Slack message's entry, and written and read back. */
object CaseIdTests extends TestSuite {

  private val conversation = ConversationId("0199a1b2-0000-7000-8000-000000000001")
  private val thread = Origin.Slack("T1", "C9", "1727000000.000100")

  val tests = Tests {
    test("a Slack message's inbound entry is its channel and ts") {
      CaseId
        .of(thread, InboundId.of(conversation, SourceId("1727000050.000200")))
        .map(_.written) ==> Some("C9/1727000050.000200")
    }

    test("an entry that is not inbound, or not in Slack, is no case") {
      CaseId.of(thread, EntryId(s"reply:${ConversationId.value(conversation)}:3")) ==> None
      val tui = Origin.Tui(Directory.of("/tmp").fold(sys.error, identity), "s")
      CaseId.of(tui, InboundId.of(conversation, SourceId("1727000050.000200"))) ==> None
    }

    test("a thread's opening is the case its ts names") {
      CaseId.opening(thread).map(_.written) ==> Some("C9/1727000000.000100")
    }

    test("a written case reads back; one that is not two parts split by one slash does not") {
      CaseId.read("C9/1727000050.000200").map(_.written) ==> Right("C9/1727000050.000200")
      CaseId.read("C9/1/2") ==> Left("not a case id: C9/1/2")
      CaseId.read("/1727000050.000200") ==> Left("not a case id: /1727000050.000200")
      CaseId.read("C9") ==> Left("not a case id: C9")
    }

    test("a channel holding a slash makes no case, so every case written reads back") {
      CaseId.opening(Origin.Slack("T1", "C/9", "1727000000.000100")) ==> None
    }
  }
}
