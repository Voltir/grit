package grit.turn

import java.time.Instant

import grit.core.durable.StepRecord
import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.UsageLedger

import utest.*
import TurnFixtures.conversation

object TurnCacheTests extends TestSuite {

  private val turn = TurnRef(conversation, TurnSeq(2))

  private def usage(input: Long, read: Long, write: Long): Usage =
    Usage(Tokens(input), Tokens(5), Tokens(read), Some(BigDecimal("0.001")), Tokens(write))

  private def row(entry: EntryId, model: String, u: Usage): UsageLedger.Row =
    UsageLedger.Row(entry, model, None, None, u, Tokens(0))

  private val at0 = Instant.parse("2026-10-30T10:00:00Z")
  private val at1 = Instant.parse("2026-10-30T10:00:07Z")

  // A turn that called tools in round 0 and answered in round 1, then summarised: the steps
  // as it recorded them, under the carry-reasons patch, and its workflow's ledger rows.
  private val steps: Vector[StepRecord] = Vector(
    StepRecord(StepRecord.marker(TurnAssembly.Carrying), None, None),
    StepRecord(StepRecord.marker(TurnAssembly.CarryReasons), None, None),
    // The recorded form, pinned: `carried`'s output under the patch.
    StepRecord(Turn.Step.Carried, Some("""{"ok":{"afresh":"prefix-differs"}}"""), Some(at0)),
    StepRecord("ask:0", Some("{}"), Some(at0)),
    StepRecord("ask:1", Some("{}"), Some(at1)),
    StepRecord(Turn.Step.Summarise, Some("{}"), Some(at1))
  )
  private val rows: Vector[UsageLedger.Row] = Vector(
    row(TurnTools.callId(turn, TurnLoop.Round.at(0)), "m", usage(100, 0, 90)),
    row(turn.replyId, "m", usage(200, 180, 10)),
    row(TurnSummary.id(turn), "s", usage(50, 0, 0)),
    row(EntryId("not-this-turns"), "x", usage(1, 0, 0))
  )

  val tests = Tests {
    test(
      "a turn's own calls are its rounds and its reply, in round order, each started with its ask; any other call of the turn is an aside, and a row of no role of the turn's is left out"
    ) {
      TurnCache.of(turn, steps, rows) ==> Right(
        TurnCache(
          turn,
          Some(TurnBasis.Afresh(TurnBasis.Why.PrefixDiffers)),
          Vector(
            TurnCache.Own(0, "m", usage(100, 0, 90), Some(at0)),
            TurnCache.Own(1, "m", usage(200, 180, 10), Some(at1))
          ),
          Vector(TurnCache.Aside(TurnRecord.Role.Summary, "s", usage(50, 0, 0)))
        )
      )
    }

    test("a turn's share is its own calls' cache reads over their input, its asides left out") {
      TurnCache.of(turn, steps, rows).map(_.share) ==> Right(Some(180.0 / 300))
    }
  }
}
