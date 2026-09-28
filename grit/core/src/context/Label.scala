package grit.core.context

/** The label each line grit writes into a window begins with, which a turn's base prompt
  * teaches the model to read; and `noun`, what a paste of it is called where grit shows one
  * in text it did not write ([[Shown.lead]]).
  */
enum Label(val tag: String, val noun: String) {

  /** The record of the conversation up to its last close. */
  case Record extends Label("[record]", "grit record")

  /** A section of another conversation: its open turns, or its record once closed. */
  case Afar extends Label("[afar]", "grit section from another conversation")

  /** Turns left out between the messages on either side of it. */
  case Gap extends Label("[gap]", "grit gap line")
}
