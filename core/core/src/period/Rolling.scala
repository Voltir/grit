package grit.core.period

import scala.concurrent.duration.*

import grit.core.message.Tokens

/** How a room's period rolls (ADR 0060): it closes once what was said in it reaches `chars`
  * characters, or once it has been open `most`; `shown` is the most a window's room sections
  * cost, in the estimated tokens of the assembler that draws them.
  */
final case class Rolling private (chars: Int, most: FiniteDuration, shown: Tokens)

object Rolling {

  /** The rolling, or why not: `chars` must be at least 1, `most` positive, and `shown` not
    * negative.
    */
  def of(chars: Int, most: FiniteDuration, shown: Tokens): Either[String, Rolling] =
    for {
      _ <- Either.cond(chars >= 1, (), "chars must be at least 1")
      _ <- Either.cond(most > Duration.Zero, (), "most must be positive")
      _ <- Either.cond(Tokens.value(shown) >= 0, (), "shown must not be negative")
    } yield new Rolling(chars, most, shown)

  /** 30,000 characters, 7 days, and 6,000 tokens shown. The characters stay under what a
    * window's tail holds, so a room's open period is drawn whole at a boundary.
    */
  val Default: Rolling = new Rolling(30000, 7.days, Tokens(6000))
}
