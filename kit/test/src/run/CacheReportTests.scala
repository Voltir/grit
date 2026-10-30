package grit.kit.run

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.turn.{TurnBasis, TurnCache, TurnRecord}

import utest.*

object CacheReportTests extends TestSuite {

  private val conversation = ConversationId("c1")

  private def usage(input: Long, read: Long, write: Long, cost: Option[String]): Usage =
    Usage(Tokens(input), Tokens(5), Tokens(read), cost.map(BigDecimal(_)), Tokens(write))

  private val at = Instant.parse("2026-10-30T10:00:00Z")

  // A first turn drawn afresh, then one carried 3 min 5 s after the first's last call began.
  private val first: TurnCache = TurnCache(
    TurnRef(conversation, TurnSeq(1)),
    Some(TurnBasis.Afresh(TurnBasis.Why.NoWindow)),
    Vector(TurnCache.Own(0, "m", usage(100, 0, 90, Some("0.001")), Some(at))),
    Vector.empty
  )
  private val second: TurnCache = TurnCache(
    TurnRef(conversation, TurnSeq(2)),
    Some(TurnBasis.Against(grit.core.id.EntryId("window:c1:1"))),
    Vector(
      TurnCache.Own(0, "m", usage(200, 180, 10, Some("0.0002")), Some(at.plusSeconds(185))),
      TurnCache.Own(1, "m", usage(300, 290, 10, Some("0.0003")), Some(at.plusSeconds(190)))
    ),
    Vector(TurnCache.Aside(TurnRecord.Role.Summary, "s", usage(50, 0, 0, None)))
  )

  val tests = Tests {
    test(
      "the report shows each turn's carry or why not, its share and the gap since the last turn's last call, each call's tokens and cost, and how many carried turns reach the target"
    ) {
      CacheReport.lines(conversation, Vector(first, second)) ==> Vector(
        "conversation c1",
        "turn 1 · afresh: no-window · cache reads 0% of 100 input",
        "  round 0 · first · m · in 100 · read 0 · write 90 · 0.1¢",
        "turn 2 · carried · cache reads 94% of 500 input · 3m 5s after turn 1's last call",
        "  round 0 · first · m · in 200 · read 180 · write 10 · 0.02¢",
        "  round 1 · later · m · in 300 · read 290 · write 10 · 0.03¢",
        "  summary · s · in 50 · read 0 · write 0 · cost unknown",
        "carried turns: 1 · at 80% or more: 1"
      )
    }
  }
}
