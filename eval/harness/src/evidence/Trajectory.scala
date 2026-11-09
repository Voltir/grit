package grit.eval.harness.evidence

import grit.eval.harness.sent.SentMarkdown

/** A trajectory as a proposer reads it. */
object Trajectory {

  /** `kept`, a trajectory as the bench keeps it (its run's lines, then each turn
    * [[SentMarkdown.render]]ed whole), with each tool result's text past `chars` characters cut
    * there and marked as [[SentMarkdown.within]] marks it; every other line as it is.
    * [[Observable.matched]] reads the kept text, never this.
    */
  def within(kept: String, chars: Int): String = {
    val lines = kept.split("\n", -1).toVector
    val out = Vector.newBuilder[String]
    var i = 0
    while (i < lines.size) {
      val line = lines(i)
      out += line
      i += 1
      val opened =
        if (Result.matches(line)) lines.lift(i).collect { case Opener(fence) => fence } else None
      opened.foreach { fence =>
        val close = lines.indexOf(fence, i + 1)
        if (close > i) {
          out ++= SentMarkdown.within(lines.slice(i + 1, close).mkString("\n"), chars, None)
          i = close + 1
        }
      }
    }
    out.result().mkString("\n")
  }

  /** A tool result's heading line, as [[SentMarkdown]] writes it. */
  private val Result = """\*\*\d+ · tool result\*\* for `.*""".r

  /** The line opening a tool result's `text` fence, its fence captured. */
  private val Opener = """(`{3,})text""".r
}
