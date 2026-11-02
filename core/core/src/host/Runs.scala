package grit.core.host

import scala.concurrent.duration.*

import grit.core.id.Sha256

/** A command [[Runs]] started: 8 lowercase hex digits. One from before a restart names nothing,
  * because the new process's `Runs` knows only what it started.
  */
opaque type RunId = String

object RunId {

  /** `raw` as an id, or why not: not 8 lowercase hex digits. */
  def parse(raw: String): Either[String, RunId] =
    if (raw.length == 8 && raw.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')))
      Right(raw)
    else Left(s"`$raw` is not a run's id: ids are 8 letters and digits from 0-9 and a-f.")

  /** The id made from a [[grit.core.clock.Fresh]] nonce: the first 8 hex digits of its
    * SHA-256.
    */
  def of(nonce: String): RunId = Sha256.text(nonce).hex.take(8)

  extension (id: RunId) {

    /** Its 8 hex digits. */
    def written: String = id
  }

  given CanEqual[RunId, RunId] = CanEqual.derived
}

/** How a run stands. Each output is its last lines, clipped as [[Clipped.tail]] clips. */
enum RunStatus {

  /** Still running after `elapsed`; it is killed at `timeout`. */
  case Going(elapsed: FiniteDuration, timeout: FiniteDuration, output: Clipped)

  /** Ended by itself. */
  case Ended(ran: Ran)

  /** Killed, with everything it started, at its timeout `after`. */
  case TimedOut(after: FiniteDuration, output: Clipped)

  /** Killed by [[Runs.stop]] after `after`. */
  case Stopped(after: FiniteDuration, output: Clipped)
}

/** Commands started at the checkout's root that outlive the call starting them, each known by
  * its [[RunId]] to the `Runs` that started it, until [[Runs.Kept]] newer ones have ended or its
  * process ends.
  */
trait Runs extends caps.SharedCapability {

  /** `argv` started with nothing on its input, and killed with everything it started at
    * `timeout` or [[Runs.Longest]], the less, or once its output passes
    * [[Workspace.MaxFileBytes]]; its id and how it stands once it ends or `wait` passes,
    * whichever is first. `Left`: `Busy` when [[Runs.Most]] are going, and nothing starts;
    * `TooLarge` when its output passed the most, and it is forgotten; `Failed` when it cannot
    * start.
    */
  def start(
      argv: Argv,
      timeout: FiniteDuration,
      wait: FiniteDuration
  ): Either[HostError, (id: RunId, status: RunStatus)]

  /** The command `id` runs and how it stands, once it ends or `wait` passes, whichever is first.
    * `Left`: `Gone` when no run `id` is kept; `TooLarge` when its output passed
    * [[Workspace.MaxFileBytes]], and it is forgotten; `Failed` when its output cannot be read.
    */
  def tail(id: RunId, wait: FiniteDuration): Either[HostError, (argv: Argv, status: RunStatus)]

  /** `id` killed with everything it started, and the command it ran and how it stands:
    * `Stopped`, or as it ended when it already had. `Left` as [[tail]].
    */
  def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)]
}

object Runs {

  /** 4: the most runs going at once in one checkout. */
  val Most = 4

  /** 8: the most ended runs kept; past that, the one started first is forgotten. */
  val Kept = 8

  /** 30 seconds: how long `run` waits for a command before answering with its id. */
  val Wait: FiniteDuration = 30.seconds

  /** 60 minutes: the longest a run may go before it is killed. */
  val Longest: FiniteDuration = 60.minutes
}
