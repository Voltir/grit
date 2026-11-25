package grit.eval.harness.sent

import utest.*

/** What a recorded turn is priced at ([[TurnRates]]). */
object TurnRatesTests extends TestSuite {

  private val glm = "z-ai/glm-5.3-flash"

  /** [[TurnSents.sent]]'s turn, unpinned, its ledger rows naming `models` in turn. */
  private def unpinned(models: String*): TurnSent = {
    val sent = TurnSents.sent(TurnSents.request(TurnSents.asked))
    sent.copy(spent = sent.spent.zipWithIndex.map { case ((entry, spent), i) =>
      entry -> spent.copy(model = models.lift(i).orElse(models.lastOption).getOrElse("m"))
    })
  }

  val tests = Tests {
    test("an unpinned turn of one fitted model is priced at its fitted rates") {
      TurnRates.of(unpinned(glm)).map { r =>
        val rates = r.pricing.at(grit.core.message.Tokens.Zero)
        (r.source, rates.input, rates.read, rates.output)
      } ==> Some(
        (TurnRates.Source.Fitted(glm), BigDecimal("0.15"), BigDecimal("0.03"), BigDecimal("0.50"))
      )
    }
    test("an unpinned turn of two models is unpriced") {
      TurnRates.of(unpinned("other", glm)) ==> None
    }
  }
}
