package grit.knowledge.github

import grit.knowledge.card.UnitVersion
import grit.knowledge.sync.Fetched

/** A file as `get_file_contents` answers it. */
object Contents {

  /** What `answer`, the text of a `get_file_contents` call of a file, holds: its first line
    * `successfully downloaded text file (SHA: {sha})` and, after it, the file's text (empty
    * when nothing follows), as [[Fetched.Text]] at its blob's SHA. A text over the answer's
    * limit is its head, followed by the answer's note of what was cut. Anything else is
    * [[Fetched.Unversioned]], saying `a binary file`, `too large to send`, `read at another ref
    * than the source's` (the ref missing, so GitHub read its default branch), `its SHA, {sha},
    * is no git SHA`, or `not a file's answer`.
    */
  def of(answer: String): Fetched = {
    val (first, text) = answer.split("\n", 2).toVector match {
      case Vector(line, rest) => (line, rest)
      case lines => (lines.headOption.getOrElse(""), "")
    }
    first match {
      case Downloaded(sha) =>
        UnitVersion
          .of(sha)
          .fold(_ => Fetched.Unversioned(s"its SHA, $sha, is no git SHA"), Fetched.Text(text, _))
      case _ if first.startsWith(TextPrefix) =>
        Fetched.Unversioned("read at another ref than the source's")
      case _ if first.startsWith("successfully downloaded binary file") =>
        Fetched.Unversioned("a binary file")
      case _ if first.contains("is too large to display") =>
        Fetched.Unversioned("too large to send")
      case _ => Fetched.Unversioned("not a file's answer")
    }
  }

  private val TextPrefix = "successfully downloaded text file (SHA: "

  private val Downloaded = """successfully downloaded text file \(SHA: (\S+)\)""".r
}
