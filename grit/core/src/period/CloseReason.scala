package grit.core.period

/** Why a period closed. */
enum CloseReason {

  /** Someone said the work was done, and nothing happened in the grace window after. */
  case Resolved

  /** Nothing happened for the idle window. */
  case Lapsed
}
