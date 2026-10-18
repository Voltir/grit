package grit.eval.harness.label

/** What a correct reply to a question holds: one of each of `facts`' alternatives, and none of
  * `never`. A string is held where the reply's text has it, ignoring case, runs of whitespace,
  * backticks and asterisks, curly quotes and the dash's kind, a letter or digit at either of its
  * ends not continued by another in the reply (`30 days` is not held in `130 days`).
  */
final case class Gold(facts: ::[Gold.Fact], never: Vector[String]) {

  /** How much of this gold `reply` holds. */
  def matched(reply: String): Gold.Matched = {
    val text = Gold.plain(reply)
    def edge(c: Char) = c.isLetterOrDigit
    def held(s: String) = {
      val p = Gold.plain(s)
      val (first, last) = (p.headOption.exists(edge), p.lastOption.exists(edge))
      p.nonEmpty && Iterator
        .iterate(text.indexOf(p))(i => text.indexOf(p, i + 1))
        .takeWhile(_ >= 0)
        .exists { i =>
          val end = i + p.length
          !(first && text.lift(i - 1).exists(edge)) && !(last && text.lift(end).exists(edge))
        }
    }
    Gold.Matched(facts.count(_.any.exists(held)), facts.size, never.count(held))
  }
}

object Gold {

  /** `s` as matching reads it: lower case, backticks and asterisks gone, quotes straight, every
    * dash a hyphen, each run of whitespace one space.
    */
  private def plain(s: String): String =
    java.text.Normalizer
      .normalize(s, java.text.Normalizer.Form.NFKC)
      .toLowerCase
      .replaceAll("[`*]", "")
      .replaceAll("[‘’ʼ]", "'")
      .replaceAll("[“”]", "\"")
      .replaceAll("[‐-―−]", "-")
      .replaceAll("\\s+", " ")
      .trim

  /** A fact held when any of `any` is. */
  final case class Fact(any: ::[String])

  /** Of a gold's `of` facts, how many a reply held (`found`), and how many of its never-strings
    * it held (`never`).
    */
  final case class Matched private[label] (found: Int, of: Int, never: Int) {

    /** Every fact held, and no never-string. */
    def correct: Boolean = found == of && never == 0
  }
}
