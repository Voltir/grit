package grit.turn

import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}

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
  }
}
