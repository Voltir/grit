package grit.core.model

import grit.core.message.{Tokens, Usage}

import utest.*

/** The expected figures are worked by hand, USD per million tokens, not from the code. */
object PricingTests extends TestSuite {

  private def rates(input: String, write: String, read: String, output: String): Rates =
    Rates
      .of(BigDecimal(input), BigDecimal(write), BigDecimal(read), BigDecimal(output))
      .fold(why => throw new java.lang.AssertionError(why), identity)

  private val short = rates("0.10", "0.125", "0.01", "0.50")
  private val long = rates("0.50", "0.625", "0.05", "2.50")

  private def stepped(past: Long, first: Rates, next: Rates): Either[String, Pricing] =
    Pricing.of(first, Vector(Pricing.Tier(Tokens(past), next)))

  private val haiku =
    stepped(100_000, short, long).fold(why => throw new java.lang.AssertionError(why), identity)

  private def usage(input: Long, read: Long, write: Long, output: Long): Usage =
    Usage(Tokens(input), Tokens(output), Tokens(read), None, Tokens(write))

  val tests = Tests {
    test(
      "a prompt is billed at the first tier to 100,000 tokens and at the next past them, reads and writes counted"
    ) {
      // At 100,000: 90,000 read at 0.01, 4,000 written at 0.125, 6,000 uncached at 0.10, 2,000
      // out at 0.50. At 100,001, one more uncached, every rate the long tier's.
      (
        haiku.cost(usage(100_000, 90_000, 4_000, 2_000)),
        haiku.cost(usage(100_001, 90_000, 4_000, 2_000))
      ) ==> (BigDecimal("0.0030"), BigDecimal("0.0150005"))
    }

    test("a call whose reads and writes exceed its input bills no uncached input") {
      haiku.cost(usage(1_000, 900, 200, 0)) ==> BigDecimal("0.0000340")
    }

    test("thinking is billed at its call's tier's output rate; none when not reported") {
      val thought = usage(10_000, 0, 0, 1_000).copy(thinking = Some(Tokens(400)))
      (
        haiku.thinking(thought),
        haiku.thinking(thought.copy(input = Tokens(120_000))),
        haiku.thinking(thought.copy(thinking = None))
      ) ==> (Some(BigDecimal("0.0002")), Some(BigDecimal("0.001")), None)
    }

    test("pricing refuses a step that does not lengthen, or whose rates do not rise") {
      val lower = rates("0.50", "0.625", "0.005", "2.50")
      (
        stepped(0, short, long),
        Pricing.of(
          short,
          Vector(
            Pricing.Tier(Tokens(100), long),
            Pricing.Tier(Tokens(100), rates("1", "1", "1", "5"))
          )
        ),
        stepped(100, short, lower),
        stepped(100, short, short)
      ) ==> (
        Left("step 0 starts at 0, not past 0"),
        Left("step 1 starts at 100, not past 100"),
        Left("step 0's rates do not rise above the tier before it"),
        Left("step 0's rates do not rise above the tier before it")
      )
    }

    test("rates refuse a rate below 0, naming it") {
      Rates.of(BigDecimal(1), BigDecimal(1), BigDecimal("-0.01"), BigDecimal(1)) ==>
        Left("the read rate -0.01 is below 0")
    }

    test("the first tier holds to where the first step begins; one tier holds at any length") {
      (
        haiku.flatTo,
        Pricing.flat(short).flatTo,
        haiku.oneTier(Tokens(100_000)),
        haiku.oneTier(Tokens(100_001)),
        Pricing.flat(short).oneTier(Tokens(10_000_000))
      ) ==> (Some(Tokens(100_000)), None, true, false, true)
    }

    test("an output token costs a millionth of its tier's output rate") {
      (short.outputPerToken, long.outputPerToken) ==>
        (BigDecimal("0.0000005"), BigDecimal("0.0000025"))
    }

    test("a usage priced by a pricing costs what it bills, and nothing known under none") {
      // 1,000 uncached at 0.10 and 1,000 out at 0.50.
      val used = usage(1_000, 0, 0, 1_000)
      (Pricing.priced(used, Some(haiku)).costUsd, Pricing.priced(used, None)) ==>
        (Some(BigDecimal("0.0006")), used.copy(costUsd = None))
    }
  }
}
