package grit.core.triage

import java.time.Instant

import grit.core.id.ShadowName
import grit.core.spend.DailyCap

import utest.*

object ShadowingTests extends TestSuite {

  private def capped(usd: String): Shadowing =
    Shadowing(
      ShadowName.of("words").getOrElse(throw new java.lang.AssertionError("a name")),
      Instant.parse("2026-10-01T00:00:00Z"),
      DailyCap.of(usd).getOrElse(throw new java.lang.AssertionError(usd))
    )

  private def usd(s: String): BigDecimal = BigDecimal(s)

  val tests = Tests {
    test("a batch that would cross the cap is cut to what the rest of the cap covers") {
      // $0.01 a day, $0.0091 spent: $0.0009 left covers 3 calls at $0.0003, not 4.
      capped("0.01").batch(usd("0.0091"), Vector(usd("0.0002"), usd("0.0004"))) ==> 3
    }

    test("before any call, a call is estimated at FirstCallUsd") {
      // $0.001 left at $0.0002 a call: 5.
      capped("0.001").batch(usd("0"), Vector.empty) ==> 5
    }

    test("a batch is at most MaxBatch, and none once the cap is spent or passed") {
      capped("10").batch(usd("0"), Vector(usd("0.0001"))) ==> Shadowing.MaxBatch
      capped("0.01").batch(usd("0.01"), Vector(usd("0.0001"))) ==> 0
      capped("0.01").batch(usd("0.02"), Vector(usd("0.0001"))) ==> 0
    }

    test("calls that cost nothing leave the batch at MaxBatch") {
      capped("0.01").batch(usd("0"), Vector(usd("0"), usd("0"))) ==> Shadowing.MaxBatch
    }
  }
}
