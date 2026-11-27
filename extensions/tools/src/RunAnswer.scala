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
    val (chosen, unshown) = picked(above.zipWithIndex.map((l, i) => (i + 1, l)), FlaggedBytes)
    if (chosen.isEmpty && unshown == 0)
      if (failed)
        s"None of lines 1 to $last has a usual mark of a failure (`[error]`, `FAILED`, `error:`, " +
          "an exception's name); `search` the file for what this command prints on a failure."
      else s"None of lines 1 to $last looks like an error, a warning or a failing test."
    else if (chosen.isEmpty)
      s"Of lines 1 to $last, those that look like errors, warnings or failing tests are too " +
        "long to show here; `search` the file for them."
    else {
      val lines = listed(chosen)
      val more =
        if (unshown == 0) ""
        else
          s"\nNot shown: ${Words.counted(unshown, "more such line")}; `search` the file for them."
      s"Of lines 1 to $last, those that look like errors, warnings or failing tests, each " +
        s"after its line number in that file, and the $Following after each:\n$lines$more"
    }
  }

  /** Of `lines`, an output's lines each after its line number in it, in order, those the
    * flagged rule picks: each that looks like a failure ([[Failing]]), then each that looks like
    * a warning ([[Noted]]), with those of the [[Following]] after it that are among `lines`,
    * taken whole while `budget` bytes last, each as [[listed]] numbers it; and how many flagged
    * lines were left out.
    */
  def picked(lines: Vector[(Int, String)], budget: Int): (Vector[(Int, String)], Int) = {
    val failing = lines.indices.filter(i => Failing.findFirstIn(lines(i)._2).nonEmpty)
    val noted = lines.indices.filter(i =>
      Failing.findFirstIn(lines(i)._2).isEmpty && Noted.findFirstIn(lines(i)._2).nonEmpty
    )
    // Each match with the lines after it, taken whole while the budget lasts: failing ones first.
    val (chosen, _) = (failing ++ noted).foldLeft((Set.empty[Int], 0)) { case ((in, used), i) =>
      val (n, _) = lines(i)
      val fresh = lines.indices.drop(i).takeWhile(j => lines(j)._1 <= n + Following).filterNot(in)
      val cost = fresh.map(j => bytes(numbered(lines(j))) + 1).sum
      if (used + cost <= budget) (in ++ fresh, used + cost) else (in, used)
    }
    (chosen.toVector.sorted.map(lines), (failing ++ noted).count(i => !chosen(i)))
  }

  /** `lines`, in order, each after its line number as [[LineNumbers]] numbers it, `…` on a line of
    * its own wherever lines between two are left out.
    */
  def listed(lines: Vector[(Int, String)]): String =
    lines.zipWithIndex
      .map { case (l @ (n, _), k) =>
        (if (lines.lift(k - 1).exists(_._1 != n - 1)) "…\n" else "") + numbered(l)
      }
      .mkString("\n")

  /** The lines of its output an answer [[of]] wrote shows, each with its line number in that
    * output: every line of one shown whole, else its flagged lines and its tail. `answer` starts
    * with its heading's line. Lines `of` adds of its own are not among them, so what `of` left
    * out ("Not shown: …") stays out.
    */
  def shown(answer: String): Vector[(Int, String)] = {
    val lines = Clipped.lines(answer)
    lines.headOption match {
      case Some(first) if first.endsWith(":") =>
        lines.drop(1).zipWithIndex.map((l, i) => (i + 1, l))
      case _ =>
        val (above, tail) = lines.drop(1).span(l => TailHead.findFirstIn(l).isEmpty)
        val flagged = above.flatMap(l =>
          Numbered.findFirstMatchIn(l).flatMap(m => m.group(1).toIntOption.map((_, m.group(2))))
        )
        val fromTail = tail.headOption
          .flatMap(h => TailHead.findFirstMatchIn(h))
          .flatMap(m => m.group(1).toIntOption)
          .fold(Vector.empty[(Int, String)])(from =>
            tail.drop(1).zipWithIndex.map((l, i) => (from + i, l))
          )
        flagged ++ fromTail
    }
  }

  /** The line a long output's tail follows: "Its last line, {n}:" or "Its last lines, {a} to {b}:". */
  private val TailHead = """^Its last lines?, (\d+)(?: to \d+)?:$""".r

  /** A flagged line as [[LineNumbers]] numbers it. */
  private val Numbered = """^\s*(\d+)\t(.*)$""".r

  private def numbered(line: (Int, String)): String =
    LineNumbers.show(line._1, Vector(line._2)).mkString

  private def bytes(s: String): Int = s.getBytes(UTF_8).length
}
