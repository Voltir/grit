package grit.core.spend

import java.time.{Instant, ZoneId}

import grit.core.message.Cost

/** How grit meters its spend: the zone its days begin in, and the cap on each, if any. */
final case class Budget(zone: ZoneId, cap: Option[DailyCap]) {

  /** The day `now` falls on. */
  def today(now: Instant): Day = Day.at(now, zone)

  /** Whether a new message may start a turn, or a job's run ask a model, once `spent` has been
    * recorded today: always,
    * with no cap; otherwise while the priced part of `spent` is below the cap. A call its
    * provider does not price counts as nothing, so under a provider that prices none the
    * cap is never reached. It is checked before a message is recorded, and before each ask of
    * an acting whose allowance is `Daily` (a job's run, [[grit.core.act.Allowance.Daily]]): the
    * turns already running each spend up to their own bound (`GRIT_TOOL_ROUNDS` model calls, each
    * of about `GRIT_WINDOW_TOKENS` in and at most the turn's output budget, `GRIT_MAX_TOKENS`,
    * out) and a closing summary per period they end, so a day can end above its cap by that
    * much.
    */
  def admits(spent: Spend): Boolean = cap.forall { c =>
    val priced = spent.cost match {
      case Cost.Exact(usd) => usd
      case Cost.AtLeast(usd) => usd
    }
    priced < c.usd
  }
}

object Budget {

  /** What a person is told when their message is not taken because the day's cap was
    * reached. It names no cost and no cap.
    */
  val Refusal: String = "grit can't take new messages right now. Try again later."
}
