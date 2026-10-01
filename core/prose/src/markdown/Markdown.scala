package grit.prose.markdown

import grit.prose.form.Doc

/** Markdown in, prose out: the subset a model writes by habit ([[Blocks]], [[Inlines]]).
  * Total: any string is some prose, and nothing here throws.
  */
object Markdown {

  /** A whole piece of markdown, as prose. An emphasis or code span left unclosed is
    * literal text; a code fence left unclosed runs to the end.
    */
  def parse(text: String): Doc = Doc(Blocks.parse(lines(text), open = false))

  /** The start of markdown still arriving -- a reply as it streams -- as prose that the
    * rest will only extend: an unclosed span runs to the end, a delimiter or a line that
    * may yet turn into a block marker is held back until it has, and an unclosed fence is
    * code. So each prefix renders close to the last, rather than flipping between
    * literal markers and styled text as tokens land.
    */
  def parsePrefix(text: String): Doc = Doc(Blocks.parse(lines(held(text)), open = true))

  /** `text` split into lines, carriage returns dropped. */
  private def lines(text: String): Vector[String] =
    text.replace("\r", "").split("\n", -1).toVector

  /** A last line that could still become a block marker -- `#`, `-`, `1.`, a fence being
    * typed -- whose meaning the next token decides.
    */
  private val Pending = """^(?:#{1,6}|[-*+_]{1,2}|\d{1,9}[.)]?|`{1,2}|~{1,2}|`{3,}.*|~{3,}.*)$""".r

  /** `text` without what the next token may still change the meaning of: a last line that
    * is [[Pending]], a table row still arriving, and a table whose delimiter row has not
    * arrived (until it does, its header would read as a paragraph).
    */
  private def held(text: String): String = {
    val all = text.split("\n", -1).toVector
    val settled = all.dropRight(1)
    val partial = all.lastOption.getOrElse("")
    var s = settled.length
    while (s > 0 && settled(s - 1).trim.startsWith("|")) s -= 1
    val table = settled.drop(s)
    val unsettledTable = table.nonEmpty && !table.lift(1).exists(Blocks.Delimiter.matches)
    val last = partial.dropWhile(c => c == ' ' || c == '\t' || c == '>')
    val holdLast = partial.trim.startsWith("|") || (last.nonEmpty && Pending.matches(last))
    if (unsettledTable) (settled.take(s) :+ "").mkString("\n")
    else if (holdLast) (settled :+ "").mkString("\n")
    else text
  }
}
