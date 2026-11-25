package grit.act.moves

import grit.core.act.MoveError
import grit.core.act.MovesFixtures.Judging
import grit.core.durable.InMemoryDurable
import grit.core.message.{Tokens, Usage}
import grit.core.spend.SpendCap
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

  val tests = Tests {
    test(
      "an ask once its conversation's spend reached its spend cap is Capped for that reason, and no model is asked"
    ) {
      val w = new MovesWorld(broken = false, MovesContract.Floor, spendCap = Some(cap))
      w.ledger.called("earlier", MovesWorld.Turn, costing("0.02"))(using TestTx.fake)
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
  }
}
