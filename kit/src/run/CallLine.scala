package grit.kit.run

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.ModelRef

/** How grit writes one model call's consumption for a person to read: in the cache report, and
  * after each call a run with arguments or `grit work` makes.
  */
object CallLine {

  /** `in <input> · read <cache read> · write <cache write> · out <output> · <cost>`, the cost in
    * cents ([[Cost.cents]]) or `cost unknown` when the call was not priced.
    */
  def spent(u: Usage): String =
    s"in ${Tokens.value(u.input)} · read ${Tokens.value(u.cacheRead)} · write ${Tokens
        .value(u.cacheWrite)} · out ${Tokens.value(u.output)} · ${u.costUsd
        .fold("cost unknown")(Cost.cents)}"

  /** `[provider] <pair> answered · ` and [[spent]]: the line printed after a call `ref`'s model
    * answered.
    */
  def answered(ref: ModelRef, u: Usage): String = s"[provider] $ref answered · ${spent(u)}"

  /** What a line [[answered]] wrote says: its pair's model id and its tokens, with no cost,
    * since the line's cents are rounded for reading; `None` for any other line.
    */
  def read(line: String): Option[(model: String, usage: Usage)] =
    line match {
      case Answered(ref, input, cacheRead, cacheWrite, output) =>
        for {
          i <- input.toLongOption
          r <- cacheRead.toLongOption
          w <- cacheWrite.toLongOption
          o <- output.toLongOption
        } yield (
          ref.takeWhile(_ != ' '),
          Usage(Tokens(i), Tokens(o), Tokens(r), None, Tokens(w))
        )
      case _ => None
    }

  /** [[answered]]'s line: its pair, then its input, cache reads, cache writes and output. */
  private val Answered =
    raw"\[provider\] (.+) answered · in (\d+) · read (\d+) · write (\d+) · out (\d+) · .*".r
}
