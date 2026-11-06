package grit.core.store

import scala.concurrent.duration.*

import grit.core.host.Clipped
import grit.core.visibility.Integrity

/** Why grit went on with a task in a new turn (ADR 0058). */
sealed trait WentOn extends caps.Pure

object WentOn {

  /** The turn before ended at its limit without declaring its task done. */
  case object AtLimit extends WentOn

  /** The turn before ended with a reply and no request to go on, and the task's acceptance
    * command `command` then failed: `exit` its exit code, `None` when it ran past
    * [[CheckLongest]] and was stopped; `tail` the end of its output, as [[failed]] cuts it.
    */
  final case class CheckFailed private[WentOn] (command: Command, exit: Option[Int], tail: String)
      extends WentOn

  object CheckFailed {

    /** A failed check as stored: refused when `exit` is 0, or `tail` is not what [[failed]]
      * keeps of it (over [[TailLines]] lines or [[TailBytes]] bytes, or holding an escape or a
      * control character).
      */
    def read(command: Command, exit: Option[Int], tail: String): Either[String, CheckFailed] =
      if (exit.contains(0)) Left("a failed check's exit code is not 0")
      else if (kept(tail) != tail) Left("a failed check's output is over its bound or not clean")
      else Right(CheckFailed(command, exit, tail))
  }

  /** A task's acceptance command, as its driver names it, at most [[CommandBytes]] bytes of
    * UTF-8 and not blank: run by the driver with `sh -c` in the task's directory.
    */
  final case class Command private (text: String) extends caps.Pure

  object Command {

    /** `text` as a command, or why it is none: blank, or over [[CommandBytes]] bytes. */
    def of(text: String): Either[String, Command] =
      if (text.isBlank) Left("the check command is empty")
      else if (text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length > CommandBytes)
        Left(s"the check command is over $CommandBytes bytes")
      else Right(Command(text))
  }

  /** How far a continuation for `reason` is believed, its task's first message believed at
    * `task`: a failed check's output is the checkout's, believed no higher than
    * [[Integrity.Member]], as a coding tool's result is.
    */
  def believed(reason: WentOn, task: Integrity): Integrity = reason match {
    case AtLimit => task
    case _: CheckFailed => task.meet(Integrity.Member)
  }

  /** The most bytes of UTF-8 an acceptance command holds. */
  val CommandBytes: Int = 1024

  /** The most lines a failed check's output keeps. */
  val TailLines: Int = 60

  /** The most bytes of UTF-8 a failed check's output keeps, newlines counted. */
  val TailBytes: Int = 4 * 1024

  /** How long a check runs before it is stopped and counted as failed. */
  val CheckLongest: FiniteDuration = 30.minutes

  /** `command` failed with `exit` (`None`: stopped past [[CheckLongest]]), its output
    * `output` kept as its end: ANSI escapes and control characters other than newline and tab
    * removed (a carriage return read as a newline) and trailing whitespace dropped, then cut
    * to its last [[TailLines]] lines
    * and [[TailBytes]] bytes. A check that exits 0 passes; an `exit` of 0 given here is kept
    * as `None`.
    */
  def failed(command: Command, exit: Option[Int], output: String): CheckFailed =
    CheckFailed(command, exit.filterNot(_ == 0), kept(output))

  private val Escape =
    "\u001b(?:\\[[0-?]*[ -/]*[@-~]|\\][^\u0007\u001b]*(?:\u0007|\u001b\\\\)|[@-Z\\\\-_])"

  private def kept(output: String): String = {
    val clean = output
      .replaceAll(Escape, "")
      .replace("\r\n", "\n")
      .replace('\r', '\n')
      .replaceAll("[\\p{Cc}&&[^\n\t]]", "")
      .stripTrailing()
    Clipped.tail(clean, _ => None, TailBytes, TailLines).text
  }
}
