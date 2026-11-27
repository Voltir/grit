package grit.core.host

/** One language's source files, the files ending in one of `suffixes`: which shell reads read
  * them, and which of those a rule finds better answered by a tool that reads them by name.
  */
final case class Sources private (language: String, suffixes: Vector[String]) extends caps.Pure {

  /** `argv` as a shell read of these sources: `Reading`, a `cat`, `head`, `tail` or `sed -n`
    * naming one of their files; `Searching`, a `grep` naming one or searching a directory
    * recursively with no `--include` but of theirs; `None` otherwise.
    */
  def read(argv: Argv): Option[Sources.Read] = {
    val words = argv.args.toVector
    def source(w: String): Boolean = suffixes.exists(w.endsWith)
    argv.program match {
      case "cat" | "head" | "tail" if words.exists(w => !w.startsWith("-") && source(w)) =>
        Some(Sources.Read.Reading)
      case "sed" if words.headOption.contains("-n") && words.drop(2).exists(source) =>
        Some(Sources.Read.Reading)
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
        )(Sources.Read.Searching)
      case _ => None
    }
  }

  /** Whether the rule finds `argv`, a `read`, better answered by name: a read of a file always;
    * a search when its pattern is an identifier (dotted, after a definition's keyword or between
    * `\b`s) and no file it names has a suffix not of these sources.
    */
  def ruled(argv: Argv, read: Sources.Read): Boolean = read match {
    case Sources.Read.Reading => true
    case Sources.Read.Searching =>
      val patterns = ShellReads.grepPatterns(argv)
      val named = ShellReads.pathWords(argv).filterNot(_.startsWith("-"))
      patterns.nonEmpty && patterns.forall(Sources.Identifier.matches) &&
      named.forall(w =>
        suffixes.exists(w.endsWith) || !w.split('/').lastOption.exists(_.contains('.'))
      )
  }
}

object Sources {

  /** Which shell read of a language's sources a command is. */
  enum Read extends caps.Pure {
    case Reading, Searching
  }

  /** `language`'s sources, the files ending in one of `suffixes`; or why not: a blank
    * `language`, no suffix, or one not starting with `.`.
    */
  def of(language: String, suffixes: Vector[String]): Either[String, Sources] =
    if (language.isBlank) Left("sources name their language")
    else if (suffixes.isEmpty || suffixes.exists(s => !s.startsWith(".") || s.length < 2))
      Left(
        "a source's suffixes each start with `.`: " +
          (if (suffixes.isEmpty) "none" else suffixes.mkString(", "))
      )
    else Right(new Sources(language, suffixes))

  /** Scala's, the files ending `.scala`. */
  val Scala: Sources = new Sources("Scala", Vector(".scala"))

  /** How the line naming the tools that read a shell read's sources by name begins: what a
    * bench's readout counts it by.
    */
  val Lead: String = "Answered in fewer tokens by "

  private val Identifier =
    raw"(?:\\b)?(?:(?:def|class|trait|object|enum|type|val|given)\s+)?[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*(?:\\b)?".r
}
