package grit.kit.run

import grit.core.edge.Unheard
import grit.lifecycle.triage.TriageQuestion
import grit.models.JevConfig

/** What hearing `messages` in `threads` adds to the day's spend, at most, in dollars: each
  * message triaged (`triage`), and every thread written a closing (`closings`, at
  * [[Estimate.PerClosing]] each).
  */
final case class Estimate(messages: Int, threads: Int, triage: BigDecimal, closings: BigDecimal) {

  /** The bound: `triage` and `closings`. */
  def total: BigDecimal = triage + closings

  def +(other: Estimate): Estimate =
    Estimate(
      messages + other.messages,
      threads + other.threads,
      triage + other.triage,
      closings + other.closings
    )
}

object Estimate {

  val Zero: Estimate = Estimate(0, 0, BigDecimal(0), BigDecimal(0))

  /** A closing at most: a gate and a closing of a heard period on today's models. */
  val PerClosing: BigDecimal = BigDecimal("0.0015")

  /** The characters a triage call sends beyond the message and its thread: its questions. */
  val TriageOverhead: Int = 2_000

  /** What hearing `unheard` would add: each message one Jev call of [[TriageOverhead]], its
    * text and the text before it in its thread (the last [[TriageQuestion.ThreadChars]] of
    * it), a token every 4 characters, at [[JevConfig.UsdPerMillionInput]]; and a closing for
    * each thread.
    */
  def of(unheard: Unheard): Estimate = {
    val chars = unheard.threads.map { thread =>
      thread
        .foldLeft((0L, 0L)) { case ((sum, before), length) =>
          val shown = math.min(before, TriageQuestion.ThreadChars.toLong)
          (sum + TriageOverhead + length + shown, before + length)
        }
        ._1
    }.sum
    val threads = unheard.threads.count(_.nonEmpty)
    Estimate(
      unheard.messages,
      threads,
      BigDecimal(chars) / 4 * JevConfig.UsdPerMillionInput / 1_000_000,
      PerClosing * threads
    )
  }
}
