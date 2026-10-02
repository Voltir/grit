package grit.core.triage

import java.time.Instant

import grit.core.id.ShadowName
import grit.core.spend.DailyCap

/** A declared shadow variant as the sweep enqueues it: every heard message triaged at or
  * after `since`, the oldest first, as many each UTC day as `dailyUsd` covers ([[batch]]).
  */
final case class Shadowing(name: ShadowName, since: Instant, dailyUsd: DailyCap) {

  /** How many messages one sweep enqueues, none of the variant's being queued or running:
    * as many as what is left of the day's cap after `spent` covers at the mean of `recent`
    * (the costs of its latest calls, at most [[Shadowing.Recent]] of them;
    * [[Shadowing.FirstCallUsd]] each when there are none), and at most
    * [[Shadowing.MaxBatch]]. The day's spend may so pass the cap by that batch's estimating
    * error, never by more.
    */
  def batch(spent: BigDecimal, recent: Vector[BigDecimal]): Int = {
    val latest = recent.take(Shadowing.Recent)
    val estimate =
      if (latest.isEmpty) Shadowing.FirstCallUsd else latest.sum / BigDecimal(latest.size)
    val left = dailyUsd.usd - spent
    if (left <= 0) 0
    else if (estimate <= 0) Shadowing.MaxBatch
    else (left / estimate).setScale(0, BigDecimal.RoundingMode.FLOOR).toInt.min(Shadowing.MaxBatch)
  }
}

object Shadowing {

  /** The most messages one sweep enqueues for one variant: 20. */
  val MaxBatch: Int = 20

  /** How many of a variant's latest calls its next call's cost is estimated from: 20. */
  val Recent: Int = 20

  /** What a call is estimated to cost before a variant has made any: $0.0002, about five
    * times a measured triage call.
    */
  val FirstCallUsd: BigDecimal = BigDecimal("0.0002")
}
