package grit.core.host

import java.io.OutputStream

import scala.concurrent.duration.FiniteDuration

/** A program and its arguments, run as given: no shell reads them. */
final case class Argv private (program: String, args: List[String]) extends caps.Pure {

  /** As a person would type it at a shell, each word holding a space, a quote or a character a
    * shell reads specially, or nothing, single-quoted: for showing, never for running.
    */
  def written: String = (program :: args).map(Argv.quoted).mkString(" ")
}

object Argv {

  /** `program` with `args`, or why not: an empty program, or a NUL in any of them. */
  def of(program: String, args: String*): Either[String, Argv] = {
    val all = program +: args
    if (program.isEmpty) Left("a program's name is empty")
    else if (all.exists(_.contains('\u0000'))) Left("a program's name or argument holds a NUL")
    else Right(new Argv(program, args.toList))
  }

  /** The characters a shell reads in a word as something other than itself. */
  private val Special: Set[Char] = " \t\n'\"\\$`|&;<>()*?[]{}~#!".toSet

  private def quoted(word: String): String =
    if (word.nonEmpty && !word.exists(Special.contains)) word
    else "'" + word.replace("'", "'\\''") + "'"
}

/** Starting programs under a root, as this machine's user, with an environment from which every
  * variable named like a secret is gone. A secret reaches a program only through [[Handing]].
  */
trait Processes extends caps.SharedCapability {

  /** Runs `argv` in `dir` with nothing on its input, and waits for it: its exit code and the tail
    * of its output and errors together. `Left`: `NotFound` or `NotADirectory` for `dir`;
    * `Outside` when `dir` leads out of the root; `Failed` when the program cannot start;
    * `TimedOut` past `timeout`, after killing it and every process it started still its
    * descendant (one that detached itself survives).
    */
  def run(argv: Argv, dir: RelPath, timeout: FiniteDuration): Either[HostError, Ran]

  /** `body` of `argv` started in `dir`. A program blocks once its output is full and unread. When
    * `body` returns or throws, a program still running is killed as [[run]] kills one. `Left`,
    * `body` not run: `dir` as for [[run]], or `Failed` when it cannot start.
    */
  def started[A](argv: Argv, dir: RelPath)(
      body: Started^ => A
  ): Either[HostError, A]
}

/** A program started by [[Processes.started]], for as long as its scope lasts. */
trait Started extends caps.SharedCapability {

  /** What the program reads; closing it ends its input. */
  def input: OutputStream^{this}

  /** The program's output line by line, split at each `\n`, `\r\n` or `\r`, each
    * line redacted as its host redacts what programs show, a value spanning lines line by line;
    * `hasNext` waits for the next line or the end. Its errors are kept apart, for [[await]].
    */
  def lines: Iterator[String]^{this}

  /** The program's next line, as [[lines]] reads them, waiting at most `within` for it:
    * [[Started.Next.Quiet]] when none came in time, [[Started.Next.Ended]] once its output has
    * ended. It reads the one output [[lines]] reads: a line either gives, the other does not.
    */
  def next(within: FiniteDuration): Started.Next

  /** Waits for the program to end: its exit code and the tail of its errors. `TimedOut` past
    * `timeout`, after killing it as [[kill]] does.
    */
  def await(timeout: FiniteDuration): Either[HostError, Ran]

  /** Kills the program and every process it started still its descendant, and waits for it to
    * end; nothing when it has ended.
    */
  def kill(): Unit
}

object Started {

  /** What [[Started.next]] found. */
  enum Next derives CanEqual {

    /** The program's next line. */
    case Line(text: String)

    /** No line came within the wait; one may still come. */
    case Quiet

    /** The program's output ended. */
    case Ended
  }
}
