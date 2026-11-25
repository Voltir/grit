package grit.eval.harness.sent

import grit.core.model.{Pricing, Rates}

/** The rates a recorded turn's calls are priced at, and where they come from. */
object TurnRates {

  /** Where a turn's rates come from. */
  enum Source extends caps.Pure {

    /** The pin its calls were made under ([[TurnSent.pricing]]). */
    case Pin

    /** [[Fitted]], for `model`, whose pin is unpriced. */
    case Fitted(model: String)
  }

  /** A turn's `pricing`, from `source`. */
  final case class Rated(pricing: Pricing, source: Source) extends caps.Pure

  /** Rates fitted to a model's recorded ledger rows, by its model id, for a model the catalog
    * does not price. GLM 5.3 flash at Z.ai: bench trial 01a21426's 44 rows fit $0.15 a million
    * uncached, $0.03 read and $0.50 output with no residual; it reported no cache writes, so
    * writing costs as uncached.
    */
  val Fitted: Map[String, Pricing] =
    Rates
      .of(BigDecimal("0.15"), BigDecimal("0.15"), BigDecimal("0.03"), BigDecimal("0.50"))
      .toOption
      .map(r => "z-ai/glm-5.3-flash" -> Pricing.flat(r))
      .toMap

  /** `sent`'s rates: its pin's, else [[Fitted]]'s for the one model every ledger row of it
    * names; `None` when neither prices it.
    */
  def of(sent: TurnSent): Option[Rated] =
    sent.pricing
      .map(Rated(_, Source.Pin))
      .orElse(sent.spent.map(_._2.model).distinct match {
        case Vector(model) => Fitted.get(model).map(Rated(_, Source.Fitted(model)))
        case _ => None
      })

  /** How `rated` reads in a line: whose rates they are, `unpriced` with none. */
  def described(rated: Option[Rated]): String = rated.fold("unpriced") {
    case Rated(_, Source.Pin) => "priced by its pin"
    case Rated(p, Source.Fitted(model)) =>
      val r = p.at(grit.core.message.Tokens.Zero)
      s"priced at rates fitted to $model's ledger rows ($$${r.input}/M uncached, " +
        s"$$${r.read}/M read, $$${r.output}/M output)"
  }
}
