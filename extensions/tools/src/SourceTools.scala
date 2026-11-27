package grit.tools

import grit.core.classify.Ask
import grit.core.host.Argv

/** The tools that read one language's source files by name, which an allowlisted `run` names on
  * the line it answers a shell read of those files with ([[Coding.allowlisted]]): `reading` for
  * a read of a file, `searching` for a search, each as that line names it. The field a
  * deployment's stack holds of its language.
  */
final case class SourceTools private (
    language: String,
    suffixes: Vector[String],
    reading: String,
    searching: String
) extends caps.Pure {

  /** `argv` as a shell read of these sources: `Reading`, a `cat`, `head`, `tail` or `sed -n`
    * naming one of their files; `Searching`, a `grep` naming one or searching a directory
    * recursively with no `--include` but of theirs; `None` otherwise.
    */
  def read(argv: Argv): Option[SourceTools.Read] = {
    val words = argv.args.toVector
    def source(w: String): Boolean = suffixes.exists(w.endsWith)
    argv.program match {
      case "cat" | "head" | "tail" if words.exists(w => !w.startsWith("-") && source(w)) =>
        Some(SourceTools.Read.Reading)
      case "sed" if words.headOption.contains("-n") && words.drop(2).exists(source) =>
        Some(SourceTools.Read.Reading)
      case "grep" =>
        val paths = ShellReads.pathWords(argv).filterNot(_.startsWith("-"))
        val includes = words.collect { case w if w.startsWith("--include=") => w.drop(10) }
        val recursive = words.exists(w =>
          w == "--recursive" || w == "--dereference-recursive" ||
            (w.startsWith("-") && !w.startsWith("--") && w.exists(c => c == 'r' || c == 'R'))
        )
        Option.when(
          paths.exists(source) ||
            (recursive && (includes.isEmpty || includes.exists(source)))
        )(SourceTools.Read.Searching)
      case _ => None
    }
  }

  /** Whether the line is given when no classifier answers: a read of a file always; a search
    * when its pattern is an identifier (dotted, after a definition's keyword or between `\b`s)
    * and no file it names has a suffix not of these sources.
    */
  def ruled(argv: Argv, read: SourceTools.Read): Boolean = read match {
    case SourceTools.Read.Reading => true
    case SourceTools.Read.Searching =>
      val named = ShellReads.pathWords(argv).filterNot(_.startsWith("-"))
      ShellReads.grepPatterns(argv).forall(SourceTools.Identifier.matches) &&
      ShellReads.grepPatterns(argv).nonEmpty &&
      named.forall(w =>
        suffixes.exists(w.endsWith) || !w.split('/').lastOption.exists(_.contains('.'))
      )
  }

  /** The line a read's answer begins with. */
  def line(read: SourceTools.Read): String = read match {
    case SourceTools.Read.Reading => s"${SourceTools.Lead}$reading."
    case SourceTools.Read.Searching => s"${SourceTools.Lead}$searching."
  }

  /** Whether `command` looks up this language's code by name, as the machine guard's classifier
    * is asked it of a command it cleared ([[MachineGuard.ask]]).
    */
  def question: Ask[MachineGuard.State, Double] = {
    val files = suffixes.mkString(" or ")
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

  /** Which shell read of a language's sources a command is. */
  enum Read extends caps.Pure {
    case Reading, Searching
  }

  /** `language`'s sources, the files ending in one of `suffixes`, read by `reading` and searched
    * by `searching`; or why not: no suffix, one not starting with `.`, or `language`,
    * `reading` or `searching` blank.
    */
  def of(
      language: String,
      suffixes: Vector[String],
      reading: String,
      searching: String
  ): Either[String, SourceTools] =
    if (language.isBlank || reading.isBlank || searching.isBlank)
      Left("source tools name their language, and what reads and what searches it")
    else if (suffixes.isEmpty || suffixes.exists(s => !s.startsWith(".") || s.length < 2))
      Left(
        "a source's suffixes each start with `.`: " +
          (if (suffixes.isEmpty) "none" else suffixes.mkString(", "))
      )
    else Right(new SourceTools(language, suffixes, reading, searching))

  /** How the line a read's answer begins with begins: what the bench's readout counts. */
  val Lead: String = "Answered in fewer tokens by "

  /** The probability at which the classifier's yes gives the line: 0.7, Jev's confident-yes
    * line, since a line given wrongly is carried in every later call of the turn. Provisional,
    * as [[MachineGuard.Refuses]] is.
    */
  val Points: Double = 0.7

  private val Identifier =
    raw"(?:\\b)?(?:(?:def|class|trait|object|enum|type|val|given)\s+)?[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*(?:\\b)?".r
}
