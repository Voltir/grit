package grit.knowledge.github

import grit.knowledge.card.UnitVersion

/** A file as `get_file_contents` answers it. */
enum Fetched extends caps.Pure {

  /** Its text, read at `sha`, its blob's SHA: the version of the card written from it. */
  case Text(text: String, sha: UnitVersion)

  /** No text with its SHA: a binary or empty file, one too large to send, one read at another
    * ref than asked, or no file's answer.
    */
  case Unversioned
}

object Fetched {

  /** What `answer`, the text of a `get_file_contents` call of a file, holds: its first line
    * `successfully downloaded text file (SHA: {sha})` and, after it, the file's text, as
    * [[Fetched.Text]]; anything else as [[Fetched.Unversioned]]. A text over the answer's limit
    * is its head, followed by the answer's note of what was cut.
    */
  def of(answer: String): Fetched =
    answer.split("\n", 2).toVector match {
      case Vector(First(sha), text) =>
        UnitVersion.of(sha).fold(_ => Fetched.Unversioned, Fetched.Text(text, _))
      case _ => Fetched.Unversioned
    }

  private val First = """successfully downloaded text file \(SHA: (\S+)\)""".r
}
