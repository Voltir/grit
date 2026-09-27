package grit.core.context

/** The label each line grit writes into a window begins with, which a turn's base prompt
  * teaches the model to read.
  */
enum Label(val tag: String) {

  /** The record of the conversation up to its last close. */
  case Record extends Label("[record]")

  /** A section of another of the person's conversations. */
  case Afar extends Label("[afar]")

  /** Turns left out between the messages on either side of it. */
  case Gap extends Label("[gap]")
}
