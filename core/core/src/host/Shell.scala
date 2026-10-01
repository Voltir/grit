package grit.core.host

import scala.concurrent.duration.FiniteDuration

/** Running commands in the checkout. */
trait Shell extends caps.SharedCapability {

  /** Runs `command` with `sh -c` in the checkout's root, with nothing on its input, and
    * waits for it. Its output and errors are interleaved as written, clipped from the tail
    * ([[Clipped.tail]]), the hint saying how many lines were left out. A non-zero exit is
    * not a failure: it is in [[Ran]]. Fails with [[HostError.TimedOut]] when it runs past
    * `timeout`, and [[HostError.Failed]] when it cannot start.
    */
  def run(command: String, timeout: FiniteDuration): Either[HostError, Ran]
}

/** A command that ran to its end: its exit code and its output. */
final case class Ran(exit: Int, output: Clipped)
