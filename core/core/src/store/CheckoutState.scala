package grit.core.store

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.host.Clipped

/** What grit read of a task's checkout after the turn before its continuation (ADR 0058), shown
  * after the continuation's note as material; `None` for a piece not read.
  */
final case class CheckoutState(
    diff: Option[CheckoutState.Diff],
    failed: Option[CheckoutState.Failed]
) extends caps.Pure {

  /** Whether nothing was read. */
  def isEmpty: Boolean = diff.isEmpty && failed.isEmpty
}

object CheckoutState {

  /** Nothing read: a continuation shown as its reason alone. */
  val Unread: CheckoutState = CheckoutState(None, None)

  /** How the branch differs from `base`, origin's default branch as git names it
    * (`origin/main`), its uncommitted changes included: `stat` git's diff stat, empty when no
    * tracked file differs; `untracked` the files git neither tracks nor ignores, `more` how many
    * past them were left out.
    */
  final case class Diff private (base: String, stat: String, untracked: Vector[String], more: Int)
      extends caps.Pure

  object Diff {

    /** `base`, `stat` and `untracked` cut to their bounds: `base` to [[BaseBytes]]; `stat` to its
      * first [[StatLines]] lines within [[StatBytes]], a last line ` ...` standing for those cut;
      * `untracked` to its first [[UntrackedFiles]] within [[UntrackedBytes]], `more` (a count
      * already left out, at least 0) raised by those cut.
      */
    def of(base: String, stat: String, untracked: Vector[String], more: Int = 0): Diff = {
      val lines = Clipped.lines(stat.stripTrailing())
      val whole = Clipped.head(lines.take(StatLines).mkString("\n"), _ => None, StatBytes).text
      val shown =
        if (Clipped.lines(whole).size == lines.size) whole
        else {
          val room = StatBytes - Elided.length - 1
          val kept = Clipped.head(lines.take(StatLines - 1).mkString("\n"), _ => None, room).text
          (Clipped.lines(kept) :+ Elided).mkString("\n")
        }
      // The first names that fit, a newline after each: none after the first that does not.
      val (_, names) =
        untracked.take(UntrackedFiles).foldLeft((0, Vector.empty[String])) { case ((used, in), n) =>
          val cost = used + n.getBytes(UTF_8).length + 1
          if (cost <= UntrackedBytes) (cost, in :+ n)
          else (UntrackedBytes + 1, in)
        }
      Diff(
        Clipped.prefix(base, BaseBytes),
        shown,
        names,
        more.max(0) + untracked.size - names.size
      )
    }
  }

  /** The last command the turn before ran that failed with a line that looks like a failure:
    * `exit` its exit code, `None` when it was killed at its timeout; `lines` those its answer
    * flags, each after its line number.
    */
  final case class Failed private (command: String, exit: Option[Int], lines: String)
      extends caps.Pure

  object Failed {

    /** `command` cut to [[WentOn.CommandBytes]] and `lines` to their first [[FailedBytes]] in
      * whole lines; an `exit` of 0 kept as `None`.
      */
    def of(command: String, exit: Option[Int], lines: String): Failed =
      Failed(
        Clipped.prefix(command, WentOn.CommandBytes),
        exit.filterNot(_ == 0),
        Clipped.head(lines.stripTrailing(), _ => None, FailedBytes).text
      )
  }

  /** The line a cut diff stat ends with, as git marks the files `--stat-count` leaves out. */
  val Elided: String = " ..."

  /** 200: the most bytes of UTF-8 a base's name keeps. */
  val BaseBytes: Int = 200

  /** 30: the most files a diff stat names, as git is asked to cut it. */
  val StatFiles: Int = 30

  /** The most lines a diff stat keeps: [[StatFiles]], git's ` ...` and its totals line. */
  val StatLines: Int = StatFiles + 2

  /** 3 KB: the most of a diff stat kept. */
  val StatBytes: Int = 3 * 1024

  /** 20: the most untracked files named. */
  val UntrackedFiles: Int = 20

  /** 1 KB: the most of the untracked files' names kept, a newline after each. */
  val UntrackedBytes: Int = 1024

  /** 3 KB: the most of a failed command's flagged lines kept. */
  val FailedBytes: Int = 3 * 1024
}
