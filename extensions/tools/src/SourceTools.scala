package grit.tools

import grit.core.classify.Ask
import grit.core.host.Sources

/** The tools that read `sources` by name, which an allowlisted `run` names on the line it
  * answers a shell read of those files with ([[Coding.allowlisted]]): `reading` for a read of a
  * file, `searching` for a search, each as that line names it. The field a deployment's stack
  * holds of its language.
  */
final case class SourceTools private (sources: Sources, reading: String, searching: String)
    extends caps.Pure {

  /** The line a read's answer begins with, after [[Sources.Lead]]. */
  def line(read: Sources.Read): String = read match {
    case Sources.Read.Reading => s"${Sources.Lead}$reading."
    case Sources.Read.Searching => s"${Sources.Lead}$searching."
  }

  /** Whether `command` looks up these sources' code by name, as the machine guard's classifier
    * is asked it of a command it let start ([[MachineGuard.ask]]).
    */
  def question: Ask[MachineGuard.State, Double] = {
    val language = sources.language
    val files = sources.suffixes.mkString(" or ")
    Ask.yesNo[MachineGuard.State](
      s"Running `command` in the directory `checkout` looks up $language code by name: it " +
        s"prints a $language source file or part of one to see a definition, or searches " +
        s"$language sources for where a type, method or value is defined or used.",
      Some(
        s"It reads a $files file, or searches $files files for an identifier such as a class, " +
          "trait, method or value name."
      ),
      Some(
        "It searches for a message, a string literal, a comment or a pattern that is no " +
          s"identifier, or searches files that are not $language sources."
      )
    )
  }
}

object SourceTools {

  /** `sources` read by `reading` and searched by `searching`; or why not: either blank. */
  def of(sources: Sources, reading: String, searching: String): Either[String, SourceTools] =
    if (reading.isBlank || searching.isBlank)
      Left("source tools name what reads their sources and what searches them")
    else Right(new SourceTools(sources, reading, searching))

  /** The probability at which the classifier's yes gives the line: 0.7, Jev's confident-yes
    * line, since a line given wrongly is carried in every later call of the turn. Provisional,
    * as [[MachineGuard.Refuses]] is.
    */
  val Points: Double = 0.7
}
