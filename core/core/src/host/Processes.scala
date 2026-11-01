package grit.core.host

import java.io.{InputStream, OutputStream}

import scala.concurrent.duration.FiniteDuration

/** A program and its arguments, run as given: no shell reads them. */
final case class Argv private (program: String, args: List[String]) extends caps.Pure

object Argv {

  /** `program` with `args`, or why not: an empty program, or a NUL in any of them. */
  def of(program: String, args: String*): Either[String, Argv] = {
    val all = program +: args
    if (program.isEmpty) Left("a program's name is empty")
    else if (all.exists(_.contains('\u0000'))) Left("a program's name or argument holds a NUL")
    else Right(new Argv(program, args.toList))
  }
}

/** Starting programs under a root, as this machine's user, with an environment from which every
  * variable named like a secret is gone, unless its caller adds it ([[started]]). A secret
  * reaches a program through [[Handing]].
  */
trait Processes extends caps.SharedCapability {

  /** Runs `argv` in `dir` with nothing on its input, and waits for it: its exit code and the tail
    * of its output and errors together. `Left`: `NotFound` or `NotADirectory` for `dir`;
    * `Outside` when `dir` leads out of the root; `Failed` when the program cannot start;
    * `TimedOut` past `timeout`, after killing it and every process it started still its
    * descendant (one that detached itself survives).
    */
  def run(argv: Argv, dir: RelPath, timeout: FiniteDuration): Either[HostError, Ran]

  /** `body` of `argv` started in `dir`, with `added` set in its environment as given. A program
    * blocks once its output is full and unread. When `body` returns or
    * throws, a program still running is killed as [[run]] kills one. `Left`, `body` not run:
    * `dir` as for [[run]], or `Failed` when it cannot start.
    */
  def started[A](argv: Argv, dir: RelPath, added: Map[String, String])(
      body: Started^ => A
  ): Either[HostError, A]
}

/** A program started by [[Processes.started]], for as long as its scope lasts. */
trait Started extends caps.SharedCapability {

  /** What the program reads; closing it ends its input. */
  def input: OutputStream^{this}

  /** What the program writes to its output; its errors are kept apart, for [[await]]. */
  def output: InputStream^{this}

  /** Waits for the program to end: its exit code and the tail of its errors. `TimedOut` past
    * `timeout`, after killing it as [[kill]] does.
    */
  def await(timeout: FiniteDuration): Either[HostError, Ran]

  /** Kills the program and every process it started still its descendant, and waits for it to
    * end; nothing when it has ended.
    */
  def kill(): Unit
}
