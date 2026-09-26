package grit.core.period

/** Why a period closed. */
enum CloseReason {

  /** The classifier judged nobody waiting on anything, with probability `confidence`, once it had been quiet
    * for the settle window ([[Verdict]]).
    */
  case Resolved(confidence: Probability)

  /** Nothing happened for the idle window. */
  case Lapsed
}
