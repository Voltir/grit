package grit.tools

import grit.core.host.{Allowlist, Argv}

/** How an allowlisted `run` refuses a command its list does not admit: what would work in its
  * place, each form it names one its list admits or a tool of the coding set.
  */
private[tools] object RunRefusals {

  /** What follows the list's refusal of `argv` ([[grit.core.host.HostError.Unlisted]]), and
    * the subject its repeats are counted by: the script it names, git and its subcommand, or
    * the program.
    */
  def unlisted(argv: Argv, allowlist: Allowlist): (subject: String, said: String) = {
    def admits(words: Seq[String]): Boolean =
      words.headOption.exists(p => Argv.of(p, words.drop(1)*).exists(allowlist.permits))
    // `; `<form> …` runs here too` for the first of `forms` the list admits that is not `program`.
    def also(program: String, forms: String*): String =
      forms
        .find(f => f != program && admits(f.split(' ').toVector :+ "x"))
        .fold("")(f => s"; `$f …` runs here too")
    val program = argv.program
    val args = argv.args.toVector
    val spaced = program.split(' ').toVector.filter(_.nonEmpty)
    def scriptOf(word: String): Option[String] =
      word
        .split(' ')
        .headOption
        .filter(w => w.contains('/') || w.endsWith(".sh") || w.endsWith(".py"))
    if (Tools(program))
      (program, s"`$program` is a tool of its own, not a program: call `$program` itself.")
    // A plain program sent with its arguments ran split ([[Coding]]'s run): what reaches
    // here carries shell text.
    else if (spaced.size > 1 && admits(spaced ++ args))
      (
        program,
        "No shell runs here: `program` is one program's name, and pipes, `&&`, `;`, quotes " +
          s"and redirects cannot run. Give `${spaced.head}` as `program`, its arguments in " +
          "`args`, one program per call."
      )
    else if (args.headOption.exists(a => admits(s"$program/$a" +: args.drop(1))))
      (program, s"A program is one word with its path: give `$program/${args.head}` as `program`.")
    else if (Interpreters(program)) {
      val rest = args.dropWhile(_.startsWith("-"))
      if (rest.nonEmpty && admits(rest))
        (
          rest.head,
          s"Run it as a program of its own: `program` `${rest.head}`, `args` " +
            s"${rest.drop(1).map(a => s"`$a`").mkString("[", ", ", "]")}. No shell or " +
            "interpreter runs here: one program per call, no pipes."
        )
      else
        args.flatMap(scriptOf).headOption match {
          case Some(script) => (script, scripted(script))
          case None =>
            (
              program,
              "No shell or interpreter runs here: `run` starts one listed program per call, " +
                "with its arguments: no pipes, no scripts, no `-c` lines. For what it was for, " +
                "`search` finds lines in files, `read` reads a file, `list` lists a directory, " +
                "and `edit` and `write` change files."
            )
        }
    } else if (scriptOf(program).isDefined) (program, scripted(program))
    else if (program == "git") {
      val sub = args.headOption.getOrElse("")
      val restore = admits(Vector("git", "restore", "x"))
      val checkout = admits(Vector("git", "checkout", "--", "x"))
      val stash = admits(Vector("git", "stash"))
      val older =
        if (restore)
          " For a file as an older commit had it: `git restore --source <commit> <path>`."
        else ""
      val said = sub match {
        case "push" =>
          "Pushing is left to a person here: say in your reply that the branch is ready."
        case "reset" if restore || checkout || stash =>
          val undo = Vector(
            Option.when(restore)("`git restore <path>`"),
            Option.when(checkout)("`git checkout -- <path>`")
          ).flatten
          "`git reset` does not run here." +
            (if (undo.isEmpty) "" else s" To undo changes to a file: ${undo.mkString(" or ")}.") +
            (if (stash) " To set work aside: `git stash`." else "") + older
        case "checkout" if checkout =>
          "`git checkout` runs here only as `git checkout -- <path>`, to restore files: this " +
            s"checkout stays on its branch.$older"
        case _ =>
          val git = allowlist.rules.filter(r => ruleProgram(r) == "git").map(r => s"`${r.written}`")
          if (git.isEmpty) "No git command runs here."
          else s"The git commands that run here: ${git.mkString(", ")}."
      }
      (s"git $sub".trim, said)
    } else if (Searches(program))
      (program, s"`search` finds the lines matching a pattern in files${also(program, "grep")}.")
    else if (Lists(program))
      (program, s"`list` lists a directory, as deep as you ask${also(program, "find", "ls")}.")
    else if (Reads(program))
      (program, s"`read` reads a file, from any line${also(program, "cat", "head", "sed -n")}.")
    else
      (
        program,
        s"Only these programs run here: ${programs(allowlist).mkString(", ")}, each as the run " +
          "tool's description lists it."
      )
  }

  /** The sentence a refusal of `subject` ends with when it is the `n`th this turn, `n` past 1. */
  def repeated(subject: String, n: Int): String =
    s" This is refusal $n of `$subject` this turn: no form of it runs here, so stop trying it " +
      "and go on with what does."

  /** The sentence a machine guard's refusal ends with when it refused that command before this
    * turn.
    */
  val Again: String = " This exact command was already refused this turn."

  private def scripted(script: String): String =
    s"`run` starts only the listed programs; no form of starting `$script` is one, through " +
      "`bash`, `sh` or any other program. Do not retry it. If your task needs what it does, say " +
      "so in your reply and go on without it."

  private def ruleProgram(r: Allowlist.Rule): String = r match {
    case Allowlist.Rule.Exactly(a) => a.program
    case Allowlist.Rule.Leading(a) => a.program
    case Allowlist.Rule.Following(a, _) => a.program
  }

  private def programs(allowlist: Allowlist): Vector[String] =
    allowlist.rules.map(ruleProgram).distinct.map(p => s"`$p`")

  /** The coding set's tools, which a model may name as a program. */
  private val Tools: Set[String] = Set("read", "search", "list", "write", "edit", "run", "stop")

  private val Interpreters: Set[String] = Set(
    "bash",
    "sh",
    "zsh",
    "dash",
    "env",
    "python",
    "python3",
    "node",
    "perl",
    "ruby",
    "java",
    "xargs",
    "sudo",
    "nohup",
    "timeout",
    "exec"
  )

  private val Searches: Set[String] = Set("grep", "rg", "ag", "ack", "egrep", "fgrep")
  private val Lists: Set[String] = Set("find", "ls", "tree", "fd")
  private val Reads: Set[String] =
    Set("cat", "head", "tail", "sed", "awk", "less", "more", "bat", "nl")
}
