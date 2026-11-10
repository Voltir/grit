package grit.core.model

import grit.core.message.{Tokens, Usage}

/** One tier's prices, USD per million tokens: uncached input, input written to the prompt
  * cache (at its 5-minute rate), input read from it, and output.
  */
// A private constructor makes `apply` and `copy` private too (Scala 3), so every Rates is one
// `of` admitted.
final case class Rates private (
    input: BigDecimal,
    write: BigDecimal,
    read: BigDecimal,
    output: BigDecimal
) derives CanEqual

object Rates {

  /** These rates; `Left` naming the first below 0. */
  def of(
      input: BigDecimal,
      write: BigDecimal,
      read: BigDecimal,
      output: BigDecimal
  ): Either[String, Rates] =
    Vector("input" -> input, "write" -> write, "read" -> read, "output" -> output)
      .collectFirst { case (name, rate) if rate < 0 => s"the $name rate $rate is below 0" }
      .toLeft(Rates(input, write, read, output))
}

/** What a pair bills one call, by its prompt's length (a request's input, `Usage.input`, cache
  * reads and writes counted): `first` up to the first step, and each step's rates for a prompt
  * longer than its `past`.
  */
// As for Rates, the private constructor keeps `apply` and `copy` private: every Pricing is one
// `of` admitted, its steps lengthening and rising.
final case class Pricing private (first: Rates, steps: Vector[Pricing.Tier]) derives CanEqual {

  /** The rates a call whose prompt is `prompt` tokens is billed at. */
  def at(prompt: Tokens): Rates =
    steps.filter(s => Tokens.value(prompt) > Tokens.value(s.past)).lastOption.fold(first)(_.rates)

  /** What `usage` cost, in USD, at the rates of its prompt's tier. Its uncached input is what is
    * left of `input` after its cache reads and writes, never below 0.
    */
  def cost(usage: Usage): BigDecimal = {
    val r = at(usage.input)
    val read = Tokens.value(usage.cacheRead)
    val write = Tokens.value(usage.cacheWrite)
    val uncached = math.max(0L, Tokens.value(usage.input) - read - write)
    (r.input * uncached + r.write * write + r.read * read +
      r.output * Tokens.value(usage.output)) / Pricing.Million
  }

  /** What `usage`'s thinking cost, in USD, at the output rate of its prompt's tier; `None` when
    * its thinking is not reported.
    */
  def thinking(usage: Usage): Option[BigDecimal] =
    usage.thinking.map(t => at(usage.input).output * Tokens.value(t) / Pricing.Million)

  /** The longest prompt billed at `first`, where the price first rises; `None` for one tier. */
  def flatTo: Option[Tokens] = steps.headOption.map(_.past)

  /** Whether every set of prompts whose lengths sum to at most `totalPrompt` is billed at
    * `first`.
    */
  def oneTier(totalPrompt: Tokens): Boolean =
    flatTo.forall(f => Tokens.value(totalPrompt) <= Tokens.value(f))
}

object Pricing {

  private val Million = BigDecimal(1000000)

  /** Rates for a prompt longer than `past`. */
  final case class Tier(past: Tokens, rates: Rates) derives CanEqual

  /** `first`, then `steps`; `Left` when a step's `past` is not above the one before it's (above
    * 0 for the first), or its rates do not rise: none below the tier before it's, and one above.
    */
  def of(first: Rates, steps: Vector[Tier]): Either[String, Pricing] =
    steps.zipWithIndex
      .foldLeft[Either[String, (Tokens, Rates)]](Right((Tokens.Zero, first))) {
        case (Right((past, before)), (step, i)) =>
          if (Tokens.value(step.past) <= Tokens.value(past))
            Left(s"step $i starts at ${Tokens.value(step.past)}, not past ${Tokens.value(past)}")
          else if (!rises(before, step.rates))
            Left(s"step $i's rates do not rise above the tier before it")
          else Right((step.past, step.rates))
        case (refused, _) => refused
      }
      .map(_ => Pricing(first, steps))

  /** `rates` at any length. */
  def flat(rates: Rates): Pricing = Pricing(rates, Vector.empty)

  private def rises(before: Rates, after: Rates): Boolean = {
    val pairs = Vector(
      before.input -> after.input,
      before.write -> after.write,
      before.read -> after.read,
      before.output -> after.output
    )
    pairs.forall((b, a) => a >= b) && pairs.exists((b, a) => a > b)
  }
}
