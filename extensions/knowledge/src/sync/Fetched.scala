package grit.knowledge.sync

import grit.core.host.Clipped
import grit.knowledge.card.UnitVersion

/** A file as a read of it answers it. */
enum Fetched extends caps.Pure {

  /** Its text, read at `version`: the version of the card written from it. */
  case Text(text: String, version: UnitVersion)

  /** No text with its version, and `why`, as a run's reply says it. */
  case Unversioned(why: String)
}

object Fetched {

  /** 8192: the most bytes of a file's text a summary is asked about. */
  val HeadBytes: Int = 8192

  /** `text`'s first [[HeadBytes]] bytes in UTF-8, never splitting a character: what of a file
    * its summary is asked of.
    */
  def head(text: String): String = Clipped.prefix(text, HeadBytes)
}
