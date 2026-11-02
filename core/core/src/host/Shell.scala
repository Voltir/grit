package grit.core.host

import scala.concurrent.duration.FiniteDuration

/** Running commands in the checkout: the narrowed view of [[Processes]] a coding tool is given,
  * one command line at the checkout's root, with no argv and no input.
  */
trait Shell extends caps.SharedCapability {

  /** Runs `command` with `sh -c` in the checkout's root, with nothing on its input, and
    * waits for it. Its output and errors are interleaved as written, clipped from the tail
    * ([[Clipped.tail]]), the hint saying how many lines were left out. A non-zero exit is
    * not a failure: it is in [[Ran]]. Fails with [[HostError.TimedOut]] when it runs past
    * `timeout`, and [[HostError.Failed]] when it cannot start.
    */
  def run(command: String, timeout: FiniteDuration): Either[HostError, Ran]
}

object Shell {

  /** 10 minutes: the longest one call of a coding tool waits on a command, and the longest a
    * command [[Shell.run]] runs may run. A command [[Runs]] started may go on after the call,
    * up to [[Runs.Longest]].
    */
  val Longest: FiniteDuration = FiniteDuration(10, java.util.concurrent.TimeUnit.MINUTES)
}

/** A command that ran to its end: its exit code and its output. */
final case class Ran(exit: Int, output: Clipped)
