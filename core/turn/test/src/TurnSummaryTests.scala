package grit.turn

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Entry, Payload}

import utest.*

object TurnSummaryTests extends TestSuite {

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  val tests = Tests {
    test("an unprompted turn's summary sees the heard message it answered, and its reply once") {
      def entry(seq: Long, payload: Payload) =
        Entry(
          EntryId(s"e$seq"),
          ConversationId("c"),
          TurnSeq(3),
          None,
          EntrySeq(seq),
          payload,
          Instant.EPOCH
        )
      val said = reply("It moved to Thursday.")
      TurnSummary
        .request(
          Vector(
            entry(1, Payload.Heard("is the freeze still on?")),
            entry(2, Payload.Draft(said)),
            entry(3, Payload.Message(said))
          ),
          None
        )
        .messages ==> Vector(
        Message.User("Overheard: is the freeze still on?\n\nAssistant: It moved to Thursday.")
      )
    }

    test("labelled lines: the summary, and the topic's name and what it covers") {
      TurnSummary.read(reply("Summary: a.\nTopic: Knots\nAbout: which knot.")) ==>
        Some(TurnSummary.Read("a.", Some(("Knots", "which knot."))))
    }

    test("any case, emphasis ignored, a label running on to the next; a name cut to four words") {
      TurnSummary.read(
        reply("**summary:** one\ntwo\n\n*TOPIC*: \"Rust borrow checker errors today\".\nabout: x")
      ) ==> Some(TurnSummary.Read("one\ntwo", Some(("Rust borrow checker errors", "x"))))
    }

    test("no labels: all of it is the summary, as every summary before topics was") {
      TurnSummary.read(reply("  Asked about knots.  ")) ==>
        Some(TurnSummary.Read("Asked about knots.", None))
    }

    test("a summary without both topic lines names no topic; no text reads as nothing") {
      TurnSummary.read(reply("Summary: a.\nTopic: Knots")) ==> Some(TurnSummary.Read("a.", None))
      TurnSummary.read(reply("   ")) ==> None
    }

    test("a reply that drops the labels is still read: all of them, or only Summary's") {
      TurnSummary.read(reply("Asked about knots.\nKnots\nWhich knot holds.")) ==>
        Some(TurnSummary.Read("Asked about knots.", Some("Knots" -> "Which knot holds.")))
      TurnSummary.read(reply("Asked about knots.\nTopic: Knots\nAbout: which knot.")) ==>
        Some(TurnSummary.Read("Asked about knots.", Some("Knots" -> "which knot.")))
      // A middle line with a colon is a transcript, not a name.
      TurnSummary.read(reply("Said so.\nUser: hello\nAssistant: hi")) ==>
        Some(TurnSummary.Read("Said so.\nUser: hello\nAssistant: hi", None))
      // Two bare lines are not the three asked for: the whole text is the summary.
      TurnSummary.read(reply("Asked about knots.\nKnots")) ==>
        Some(TurnSummary.Read("Asked about knots.\nKnots", None))
    }
  }
}
