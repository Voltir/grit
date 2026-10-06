package grit.core.period

/** Why a period closed. */
enum CloseReason {

  /** The classifier judged nobody waiting on anything, with probability `confidence`, once it had been quiet
    * for the settle window ([[Verdict]]).
    */
  case Resolved(confidence: Probability)

  /** Nothing happened for the idle window. */
  case Lapsed

  /** Nothing in it was said to grit, and triage kept nothing
    * ([[grit.core.triage.Earning]]): closed on its deadline with a closing written without a
    * model.
    */
  case Unearned

  /** The period of a job's run (a conversation whose origin names a slot, `Slot.of`): never
    * asked whether anyone is waiting, and closed on its deadline with a closing written without
    * a model, its prose the run's opening and its outcome the run's reply verbatim ("No reply."
    * without one).
    */
  case Ran
}
