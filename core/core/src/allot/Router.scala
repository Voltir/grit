package grit.core.allot

import grit.core.allot.Allotment.{Calls, Reasoning}
import grit.core.message.Cost
import grit.core.model.{Effort, Grade}
import grit.core.period.Probability
import grit.core.spend.{DailyCap, Spend}

/** The effort a turn's root calls for, from its task features and the day's headroom (ADR 0031:
  * it chooses only among the pins safety cleared). It names grades, never models.
  */
object Router {

  /** 0.6, calibrated: the least p(hard) a root is hard at; lower buys recall with strong calls
    * on easy roots.
    */
  val HardAt: Probability = Probability.clamped(0.6)

  /** 0.8: the share of the daily cap spent at which headroom is `Low`, leaving a fifth of the
    * day for the turns that would otherwise overshoot it on strong pins.
    */
  val LowAt: BigDecimal = BigDecimal("0.8")

  /** `Standard`: the greatest grade a turn is routed to at `Low` headroom. */
  val LowCeiling: Grade = Grade.Standard

  /** `Medium`: the most reasoning a turn's calls ask at `Low` headroom, where its calls are at
    * most the deployment's too.
    */
  val LowEffort: Effort = Effort.Medium

  /** 2: a hard root's calls, times the deployment's, for the sources and steps it needs. */
  val HardCalls: Allotment.Calls.Scaled = new Allotment.Calls.Scaled(2)

  /** `High`: the effort a hard root asks of a reasoning pin. */
  val HardEffort: Effort = Effort.High

  /** `Low`: the effort a chat root asks of a reasoning pin. */
  val ChatEffort: Effort = Effort.Low

  /** `Low` when the priced part of `spent` is at least [[LowAt]] of `cap`; `Ample` otherwise,
    * and with no cap. A call its provider does not price counts as nothing, as for
    * [[grit.core.spend.Budget.admits]].
    */
  def headroom(spent: Spend, cap: Option[DailyCap]): Headroom = {
    val priced = spent.cost match {
      case Cost.Exact(usd) => usd
      case Cost.AtLeast(usd) => usd
    }
    if (cap.exists(c => priced >= c.usd * LowAt)) Headroom.Low else Headroom.Ample
  }

  /** The greatest grade a turn at `headroom` is routed to; `None`: no ceiling. */
  def ceiling(headroom: Headroom): Option[Grade] = headroom match {
    case Headroom.Ample => None
    case Headroom.Low => Some(LowCeiling)
  }

  /** What a root answered `features` is allotted at `headroom` (`None`: not asked or not
    * answered): a hard root that is not chat wants `Strong`, [[HardEffort]] and [[HardCalls]];
    * a chat root `Standard`, [[ChatEffort]] and the fewest calls; any other answered root
    * `Standard` and the pin's own effort and the deployment's calls; an unasked one no grade,
    * the pin's own and the deployment's. At `Low`, for every root, asked or not, the effort is
    * at most [[LowEffort]] and the calls at most the deployment's.
    */
  def allot(features: Option[Features], headroom: Headroom): Allotment = {
    val asked = features match {
      case None => Allotment.Untouched
      case Some(f) if f.kind == TaskKind.Chat =>
        Allotment(Some(Grade.Standard), Reasoning.At(ChatEffort), Calls.Fewest)
      case Some(f) if f.hard >= HardAt =>
        Allotment(Some(Grade.Strong), Reasoning.At(HardEffort), HardCalls)
      case Some(_) => Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    }
    headroom match {
      case Headroom.Ample => asked
      case Headroom.Low =>
        asked.copy(reasoning = capped(asked.reasoning), calls = capped(asked.calls))
    }
  }

  /** `reasoning` asking at most [[LowEffort]]. */
  private def capped(reasoning: Reasoning): Reasoning = reasoning match {
    case Reasoning.Own => Reasoning.AtMost(LowEffort)
    case Reasoning.At(e) => Reasoning.At(lower(e))
    case Reasoning.AtMost(e) => Reasoning.AtMost(lower(e))
  }

  /** `calls` at most the deployment's. */
  private def capped(calls: Calls): Calls = calls match {
    case Calls.Scaled(_) => Calls.Default
    case Calls.Default | Calls.Fewest => calls
  }

  private def lower(e: Effort): Effort = if (e.ordinal > LowEffort.ordinal) LowEffort else e
}
