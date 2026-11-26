package grit.tools

import java.nio.charset.StandardCharsets.UTF_8

import scala.util.matching.Regex

import grit.core.host.{Clipped, LineNumbers, RelPath, RunOutput, Workspace}
import grit.prose.words.Words

/** A run's output as `run`, `tail` and `stop` answer with it: whole when it is at most
  * [[WholeBytes]], else its size and file, the lines above its tail that look like failures, and
  * its tail.
  */
private[tools] object RunAnswer {

  /** 8 KB: the most of a run's output an answer shows whole. An answer is sent again with every
    * later call of its turn.
    */
  val WholeBytes: Int = 8 * 1024

  /** 40 lines within 4 KB: the most of a long output's end shown. */
  val TailLines = 40
  val TailBytes: Int = 4 * 1024

  /** 3 KB: the most of the flagged lines above the tail shown, numbered, with the [[Following]]
    * lines after each; [[Failing]]'s first, then [[Noted]]'s in what is left. The rest are
    * counted.
    */
  val FlaggedBytes: Int = 3 * 1024

  /** 3: the lines shown after each flagged one, where a compiler writes its message below the
    * line naming its position.
    */
  val Following = 3

  /** A line that looks like an error or a failing test, matched once [[Clipped.plain]]:
    * `[error]` (Mill, sbt, scalac); `FAIL`, `FAILED` or `FAILURE` (scripts/check's `FAIL`, Mill's
    * summary, JUnit, pytest, go); `X` first after any `…] ` prefix (utest's failing test, as Mill
    * prints it); `error:` or `error[…]:` (gcc, clang, javac, rustc); `assertion failed` or
    * `Assertion failed`; a Scala 3 diagnostic's `-- [E…]`; `panic:` (go).
    */
  val Failing: Regex =
    ("""\[error\]|\bFAIL(?:ED|URE)?\b|^(?:\S+\]\s+)?\s*X\s|\berror(?:\[\w+\])?:|""" +
      """[Aa]ssertion failed|^\s*-- \[E\d+\]|\bpanic:""").r

  /** A line that looks like a warning or an exception, matched as [[Failing]] is: `[warn]`, or a
    * word ending in `Error:` or `Exception:`.
    */
  val Noted: Regex = """\[warn\]|\b\w*(?:Error|Exception)\b:""".r

  /** `output` after `heading`, such as `Output`: `heading:` then its text when it is at most
    * [[WholeBytes]]. Else `heading:` its size and file, the flagged lines above its tail, each
    * numbered as [[LineNumbers]] numbers its line in the file, then its tail, those lines
    * [[Clipped.plain]] and each cut as [[Workspace.cut]] cuts a search's line. With nothing
    * flagged, the answer says so, and when the command `failed`, what was looked for.
    */
  def of(heading: String, output: RunOutput, failed: Boolean): String =
    if (bytes(output.text) <= WholeBytes) s"$heading:\n${output.text}"
    else {
      val shown = Clipped.lines(output.text).map(l => Workspace.cut(Clipped.plain(l)))
      val total = shown.size
      val kb = (bytes(output.text) + 1023) / 1024
      val head =
        s"$heading: ${Words.counted(total, "line")}, $kb KB, too long to show whole. All of it " +
          s"is in `${RelPath.value(output.file)}`, for `read` and `search`."
      // How many lines from the end fit in the tail: at least one, since each is cut.
      val kept = shown.reverseIterator
        .scanLeft(0)((used, l) => used + bytes(l) + 1)
        .drop(1)
        .takeWhile(_ <= TailBytes + 1)
        .size
        .min(TailLines)
        .max(1)
      val from = total - kept + 1
      val tail =
        (if (kept == 1) s"Its last line, $total:" else s"Its last lines, $from to $total:") +
          "\n" + shown.takeRight(kept).mkString("\n")
      if (from == 1) s"$head\n\n$tail"
      else s"$head\n\n${flagged(shown.take(from - 1), failed)}\n\n$tail"
    }

  /** The flagged section over `above`, the lines from line 1 above the tail. */
  private def flagged(above: Vector[String], failed: Boolean): String = {
    val last = above.size
    def numbered(i: Int): String = LineNumbers.show(i + 1, Vector(above(i))).mkString
    val failing = above.indices.filter(i => Failing.findFirstIn(above(i)).nonEmpty)
    val noted = above.indices.filter(i =>
      Failing.findFirstIn(above(i)).isEmpty && Noted.findFirstIn(above(i)).nonEmpty
    )
    // Each match with the lines after it, taken whole while the budget lasts: failing ones first.
    val (chosen, _) = (failing ++ noted).foldLeft((Set.empty[Int], 0)) { case ((in, used), i) =>
      val fresh = (i to (i + Following).min(last - 1)).filterNot(in)
      val cost = fresh.map(j => bytes(numbered(j)) + 1).sum
      if (used + cost <= FlaggedBytes) (in ++ fresh, used + cost) else (in, used)
    }
    val unshown = (failing ++ noted).count(i => !chosen(i))
    if (chosen.isEmpty && unshown == 0)
      if (failed)
        s"None of lines 1 to $last has a usual mark of a failure (`[error]`, `FAILED`, `error:`, " +
          "an exception's name); `search` the file for what this command prints on a failure."
      else s"None of lines 1 to $last looks like an error, a warning or a failing test."
    else if (chosen.isEmpty)
      s"Of lines 1 to $last, those that look like errors, warnings or failing tests are too " +
        "long to show here; `search` the file for them."
    else {
      val sorted = chosen.toVector.sorted
      // `…` on its own line marks the lines left out between two shown.
      val lines = sorted.zipWithIndex
        .map((i, k) => (if (sorted.lift(k - 1).exists(_ != i - 1)) "…\n" else "") + numbered(i))
        .mkString("\n")
      val more =
        if (unshown == 0) ""
        else
          s"\nNot shown: ${Words.counted(unshown, "more such line")}; `search` the file for them."
      s"Of lines 1 to $last, those that look like errors, warnings or failing tests, each " +
        s"after its line number in that file, and the $Following after each:\n$lines$more"
    }
  }

  private def bytes(s: String): Int = s.getBytes(UTF_8).length
}
