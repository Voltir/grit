package grit.core.context

/** The tag each line grit writes into a window begins with, which a turn's base prompt
  * teaches the model to read; and `noun`, what a paste of it is called where grit shows one
  * in text it did not write ([[Shown.lead]]).
  */
enum SectionTag(val tag: String, val noun: String) {

  /** The record of the conversation up to its last close. */
  case Record extends SectionTag("[record]", "grit record")

  /** A section of another conversation: its open turns, or its record once closed. */
  case Afar extends SectionTag("[afar]", "grit section from another conversation")

  /** Messages of another thread this conversation continues: its strand. */
  case Strand extends SectionTag("[strand]", "grit section of the same strand")

  /** Turns left out between the messages on either side of it. */
  case Gap extends SectionTag("[gap]", "grit gap line")

  /** A document a plugin keeps, shown by grit: not a turn of any conversation. */
  case Document extends SectionTag("[doc]", "grit document")
}
