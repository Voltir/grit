package grit.act.moves

import grit.core.act.MoveError
import grit.core.act.MovesFixtures.Judging
import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, TurnRef}
import grit.core.message.{SpendCap, Tokens, Usage}
import grit.dbos.sql.TestTx

import utest.*

/** [[DurableMoves]]' asks under a spend cap, which no scripted run has: the conversation its
  * acting's turn is in is the one whose spend is read.
  */
object DurableSpendCapTests extends TestSuite {
  import MovesContract.{Seen, asked, limits}

  private val cap = SpendCap.of(BigDecimal("0.02")).fold(e => sys.error(e), identity)

  /** One call's usage, costing `usd`. */
  private def costing(usd: String): Usage =
    Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(usd)))

  /** Another turn of [[MovesWorld.Turn]]'s conversation. */
  private val another = MovesWorld.Turn.copy(turnSeq = MovesWorld.Turn.turnSeq.next)

  /** A turn of another conversation. */
  private val elsewhere = TurnRef(ConversationId("elsewhere"), MovesWorld.Turn.turnSeq)

  val tests = Tests {
    test(
      "an ask once its conversation's spend reached its spend cap is Capped for that reason, and no model is asked"
    ) {
      val w = new MovesWorld(broken = false, MovesContract.Floor, spendCap = Some(cap))
      w.ledger.called("another turn", another, costing("0.02"))(using TestTx.fake)
      val got = w.run(limits(1, 0), Vector(), new Judging())(m => Seen(Vector(asked(m, "a"))))
      // The reason the ask records, which a person may read: its spend cap's, not the day's.
      val reasons = w.durable.history(MovesWorld.Turn.workflowId).collect {
        case InMemoryDurable.Step("ask:a", InMemoryDurable.Outcome.Output(v)) =>
          MovesJournal.askMade.decode(v).map {
            case AskMade.Refused(_, why, _, _) => why
            case other => other.toString
          }
      }
      (got, w.ledger.rows.size, reasons) ==> (
        Seen(Vector(Left(MoveError.Capped))),
        1,
        Vector(
          Right(
            "The conversation's spend reached its spend cap, or some of it was not priced, so no model was asked."
          )
        )
      )
    }
    test(
      "an ask while its conversation's spend is under its spend cap is asked, whatever another conversation spent"
    ) {
      val w = new MovesWorld(broken = false, MovesContract.Floor, spendCap = Some(cap))
      w.ledger.now = w.clock.at
      w.ledger.called("another turn", another, costing("0.01"))(using TestTx.fake)
      w.ledger.called("elsewhere", elsewhere, costing("0.05"))(using TestTx.fake)
      val got = w.run(limits(1, 0), Vector(), new Judging())(m => Seen(Vector(asked(m, "a"))))
      (got.moves.map(_.isRight), w.ledger.rows.size) ==> (Vector(true), 3)
    }
  }
}
