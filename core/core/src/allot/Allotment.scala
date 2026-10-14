package grit.core.allot

import grit.core.model.{Effort, Grade}

/** How much of the day's cap was left as a turn was routed ([[Router.headroom]]). */
enum Headroom derives CanEqual {
  case Ample, Low
}

/** What the effort stage lays over a turn: the grade it wants (`None`: no grade wanted; the
  * caller routes as it does unasked), the reasoning effort its calls ask, and how many calls its
  * loop may make.
  */
final case class Allotment(
    grade: Option[Grade],
    reasoning: Allotment.Reasoning,
    calls: Allotment.Calls
) derives CanEqual

object Allotment {

  /** Nothing laid over: what a turn routed before allotments ran. */
  val Untouched: Allotment = Allotment(None, Reasoning.Own, Calls.Default)

  /** The effort the turn's calls ask, over the effort its pin asks. */
  enum Reasoning derives CanEqual {

    /** The pin's own. */
    case Own

    /** `effort`, when the pin asks one; none when it asks none, since a pair is known to
      * reason only by its pin asking an effort.
      */
    case At(effort: Effort)

    /** The pin's own, lowered to `effort` when above it. */
    case AtMost(effort: Effort)

    /** The effort a pin asking `own` asks under this. */
    def over(own: Option[Effort]): Option[Effort] = this match {
      case Own => own
      case At(effort) => own.map(_ => effort)
      case AtMost(effort) => own.map(o => if (o.ordinal > effort.ordinal) effort else o)
    }
  }

  /** The turn loop's model calls, against the deployment's. */
  sealed trait Calls derives CanEqual

  object Calls {

    /** The deployment's. */
    case object Default extends Calls

    /** `times` the deployment's, at least once by construction: [[Scaled.of]], or [[Router]]. */
    final case class Scaled private[allot] (times: Int) extends Calls

    object Scaled {

      /** `times` the deployment's calls; `None` under 1. */
      def of(times: Int): Option[Scaled] = Option.when(times >= 1)(new Scaled(times))
    }

    /** The fewest a turn may make. */
    case object Fewest extends Calls
  }
}
