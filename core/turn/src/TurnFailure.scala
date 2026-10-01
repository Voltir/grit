package grit.turn

/** Why a turn ended without a reply, or without a summary. Recorded as a step's output and replayed, so each
  * case carries text rather than the error it came from.
  */
enum TurnFailure {

  /** No window: the store could not be read, or the window named entries it lacks. */
  case Assembly(reason: String)

  /** The provider produced no response, or none with the text it was asked for. */
  case Model(reason: String)

  /** The store could not be read or written. */
  case Store(reason: String)
}
