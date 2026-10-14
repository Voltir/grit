package grit.slack.edge

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.model.Grade
import grit.core.spend.TurnSpend
import grit.core.store.UsageLedger

import utest.*

object ReplyNoteTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)

  private def spent(grade: Option[Grade], upstream: Option[String] = Some("Fireworks")): TurnSpend =
    TurnSpend.of(
      turn,
      Vector(
        UsageLedger.Row(
          turn.replyId,
          "test/model",
          upstream,
          grade,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("0.0042"))),
          Tokens(1)
        )
      )
    )

  val tests = Tests {
    test("a strong answer is noted with its grade and model, and with its spend when chosen") {
      ReplyNote.values.toVector.map(_.of(spent(Some(Grade.Strong)))) ==> Vector(
        None,
        Some("strong · test/model at Fireworks"),
        Some("strong · m$4.2 · test/model at Fireworks")
      )
      ReplyNote.Strength.of(spent(Some(Grade.Strong), upstream = None)) ==>
        Some("strong · test/model")
    }

    test("an answer at standard or below, with no grade, or no answer at all is never noted") {
      val spends = Vector(
        spent(Some(Grade.Standard)),
        spent(Some(Grade.Light)),
        spent(None),
        TurnSpend.of(turn, Vector.empty)
      )
      ReplyNote.values.toVector.flatMap(n => spends.map(n.of)) ==> Vector.fill(12)(None)
    }

    test("a note is named by its word, and nothing else names one") {
      Vector("off", "strength", "strength-cost", "Strength", "cost", "").map(ReplyNote.named) ==>
        Vector(
          Some(ReplyNote.Off),
          Some(ReplyNote.Strength),
          Some(ReplyNote.StrengthAndCost),
          None,
          None,
          None
        )
    }
  }
}
