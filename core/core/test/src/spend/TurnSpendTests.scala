package grit.core.spend

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.Grade
import grit.core.store.UsageLedger

import utest.*

object TurnSpendTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)

  private def row(
      entry: EntryId,
      usd: Option[String],
      upstream: Option[String] = None,
      grade: Option[Grade] = None
  ): UsageLedger.Row =
    UsageLedger.Row(
      entry,
      "test/model",
      upstream,
      grade,
      Usage(Tokens(1), Tokens(1), Tokens.Zero, usd.map(BigDecimal(_))),
      Tokens(1)
    )

  val tests = Tests {
    test(
      "a turn's spend is every row its workflow recorded, its answer the reply's row, else the draft's"
    ) {
      val query = row(EntryId("query"), Some("0.001"))
      val draft = row(turn.draftId, Some("0.002"))
      val reply = row(turn.replyId, Some("0.003"))
      val both = TurnSpend.of(turn, Vector(query, draft, reply))
      val drafted = TurnSpend.of(turn, Vector(query, draft))
      val neither = TurnSpend.of(turn, Vector(query))
      (both.cost, both.answer, drafted.answer, neither.answer) ==>
        (Cost.Exact(BigDecimal("0.006")), Some(reply), Some(draft), None)
    }

    test(
      "its line is the cost in cents, then the model at its upstream; the model alone with none reported; the cost alone with no answer"
    ) {
      val served = row(turn.replyId, Some("0.0042"), upstream = Some("Fireworks"))
      val unreported = row(turn.replyId, None)
      val other = row(EntryId("judge"), Some("0.0124"))
      Vector(
        TurnSpend.of(turn, Vector(served)).line,
        TurnSpend.of(turn, Vector(unreported, other)).line,
        TurnSpend.of(turn, Vector(other)).line
      ) ==> Vector(
        "0.42¢ · test/model at Fireworks",
        "≥ 1.2¢ · test/model",
        "1.2¢"
      )
      TurnSpend.of(turn, Vector(other)).served ==> None
    }

    test("above: only an answer graded above; no answer, or one with no grade, is not") {
      def at(g: Option[Grade]) = TurnSpend.of(turn, Vector(row(turn.replyId, None, grade = g)))
      Vector(
        at(Some(Grade.Strong)).above(Grade.Standard),
        at(Some(Grade.Standard)).above(Grade.Standard),
        at(Some(Grade.Standard)).above(Grade.Light),
        at(None).above(Grade.Light),
        TurnSpend.of(turn, Vector.empty).above(Grade.Light)
      ) ==> Vector(true, false, true, false, false)
    }
  }
}
