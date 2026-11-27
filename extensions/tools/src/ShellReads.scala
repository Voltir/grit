package grit.tools

import grit.core.host.{Allowlist, Argv}

/** The shell's reads as the machine guard holds them to the checkout (ADR 0069): which commands
  * are reads, which of a read's words may name a path, and the `find` and `sed` that would
  * write or run a program.
  */
private[tools] object ShellReads {

  /** The reads: git's `status`, `diff`, `log` and `show`, and `ls`, `cat`, `head`, `tail`, `wc`,
    * `grep`, `diff`, `find` and `sed -n`, each with any further arguments.
    */
  val Rules: Vector[Allowlist.Rule] =
    Vector(
      Vector("git", "status"),
      Vector("git", "diff"),
      Vector("git", "log"),
      Vector("git", "show"),
      Vector("ls"),
      Vector("cat"),
      Vector("head"),
      Vector("tail"),
      Vector("wc"),
      Vector("grep"),
      Vector("diff"),
      Vector("find"),
      Vector("sed", "-n")
    ).flatMap(w => Argv.of(w.head, w.tail*).toOption.map(Allowlist.Rule.Leading(_)))

  /** Whether `argv` is one of [[Rules]]'. */
  def reads(argv: Argv): Boolean = Rules.exists(_.permits(argv))

  /** The words of `argv`, a read, that may name a path: all but `grep`'s pattern, `sed`'s
    * script, the patterns of `find`'s name tests and of git's `--grep`, `--author`,
    * `--committer`, `--format`, `--pretty`, `-S` and `-G`.
    */
  def pathWords(argv: Argv): Vector[String] = {
    val words = argv.args.toVector
    argv.program match {
      case "grep" => grepPaths(words)
      case "sed" => words.drop(2)
      case "find" => after(words, FindPatterns)
      case "git" =>
        words.zipWithIndex.collect {
          case (w, i)
              if !GitPatterns.exists(p => w.startsWith(p + "=")) &&
                !(w.startsWith("-S") || w.startsWith("-G")) &&
                !words.lift(i - 1).exists(p => GitPatterns(p) || p == "-S" || p == "-G") =>
            w
        }
      case _ => words
    }
  }

  /** Why `argv` may not start, when it is a `find` with an action that writes or runs a
    * program, or a `sed` but `sed -n` printing a line range or a pattern's lines; `None`
    * otherwise. Said after the guard's lead.
    */
  def writing(argv: Argv): Option[String] = argv.program match {
    case "find" if argv.args.exists(w => FindActions(w) || w.startsWith("-fprint")) =>
      Some(
        "runs `find` only to list what it finds: with no " +
          s"${FindShown.map(a => s"`$a`").mkString(", ")} or `-fls`. Change files with `edit` " +
          "and `write`."
      )
    case "sed" if !printing(argv.args) =>
      Some(
        "runs `sed` only as `sed -n <lines>p <files>`, printing a line range (`10,40p`, `5p`, " +
          "`$p`) or the lines a pattern matches (`/x/p`, `/x/,/y/p`). Change files with `edit` " +
          "and `write`."
      )
    case _ => None
  }

  private val FindShown: Vector[String] =
    Vector("-exec", "-execdir", "-ok", "-okdir", "-delete", "-fprint", "-fprint0", "-fprintf")

  /** GNU find's actions that write a file or run a program, `-fprint…` besides. */
  private val FindActions: Set[String] =
    Set("-exec", "-execdir", "-ok", "-okdir", "-delete", "-fls")

  private val FindPatterns: Set[String] =
    Set(
      "-name",
      "-iname",
      "-path",
      "-ipath",
      "-wholename",
      "-iwholename",
      "-regex",
      "-iregex",
      "-lname",
      "-ilname"
    )

  private val GitPatterns: Set[String] =
    Set("--grep", "--author", "--committer", "--format", "--pretty")

  /** A sed address: a line, `$`, or a `/pattern/` holding no unescaped `/`. */
  private val Address = """(?:\d+|\$|/(?:[^/\\]|\\.)*/)"""

  private val Printed = s"$Address(?:,$Address)?p".r

  /** `-n`, then a script printing an address or a range of two, then the files. */
  private def printing(args: List[String]): Boolean = args match {
    case "-n" :: Printed() :: _ => true
    case _ => false
  }

  /** `words` without the word after each of `options`. */
  private def after(words: Vector[String], options: Set[String]): Vector[String] =
    words.zipWithIndex.collect { case (w, i) if !words.lift(i - 1).exists(options) => w }

  /** grep's short options taking a value, glued or as the next word. */
  private val GrepValued: Set[Char] = Set('e', 'f', 'm', 'A', 'B', 'C', 'd', 'D')

  private val GrepLongValued: Set[String] = Set(
    "--regexp",
    "--file",
    "--max-count",
    "--after-context",
    "--before-context",
    "--context",
    "--include",
    "--exclude",
    "--exclude-dir",
    "--exclude-from",
    "--directories",
    "--devices",
    "--label",
    "--binary-files"
  )

  /** The patterns `argv`, a `grep`, searches for: each `-e`'s or `--regexp`'s value, else its
    * first operand.
    */
  def grepPatterns(argv: Argv): Vector[String] =
    grepWords(argv.args.toVector).collect { case (_, Some(p)) => p }

  /** grep's words but its patterns. */
  private def grepPaths(words: Vector[String]): Vector[String] =
    grepWords(words).collect { case (w, None) => w }

  /** Each of grep's words, with the pattern it gives when it gives one. */
  private def grepWords(words: Vector[String]): Vector[(String, Option[String])] = {
    def walk(
        rest: List[String],
        options: Boolean,
        patterned: Boolean
    ): List[(String, Option[String])] =
      rest match {
        case Nil => Nil
        case "--" :: more if options => walk(more, false, patterned)
        case w :: more if options && w.startsWith("--") =>
          val name = w.takeWhile(_ != '=')
          val regexp = name == "--regexp"
          if (w.contains('='))
            (w, Option.when(regexp)(w.drop(name.length + 1))) :: walk(more, true, patterned)
          else if (!GrepLongValued(name)) (w, None) :: walk(more, true, patterned)
          else
            more match {
              case v :: further =>
                (w, None) :: (v, Option.when(regexp)(v)) :: walk(further, true, patterned)
              case Nil => List((w, None))
            }
        case w :: more if options && w.length > 1 && w.startsWith("-") =>
          val at = w.indexWhere(GrepValued, 1)
          if (at < 0) (w, None) :: walk(more, true, patterned)
          else {
            val pattern = w(at) == 'e'
            if (at < w.length - 1)
              (w, Option.when(pattern)(w.drop(at + 1))) :: walk(more, true, patterned)
            else
              more match {
                case v :: further =>
                  (w, None) :: (v, Option.when(pattern)(v)) :: walk(further, true, patterned)
                case Nil => List((w, None))
              }
          }
        case w :: more => (w, Option.when(!patterned)(w)) :: walk(more, options, true)
      }
    val patterned = words.exists(w =>
      w == "--regexp" || w.startsWith("--regexp=") || w == "--file" || w.startsWith("--file=") ||
        (w.startsWith("-") && !w.startsWith("--") && w.drop(1).exists(c => c == 'e' || c == 'f'))
    )
    walk(words.toList, true, patterned).toVector
  }
}
