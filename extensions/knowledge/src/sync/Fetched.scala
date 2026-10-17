package grit.knowledge.sync

import scala.annotation.tailrec

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
  def head(text: String): String = {
    @tailrec def go(i: Int, bytes: Int): Int =
      if (i >= text.length) i
      else {
        val cp = text.codePointAt(i)
        val size = if (cp < 0x80) 1 else if (cp < 0x800) 2 else if (cp < 0x10000) 3 else 4
        if (bytes + size > HeadBytes) i else go(i + Character.charCount(cp), bytes + size)
      }
    text.substring(0, go(0, 0))
  }
}
