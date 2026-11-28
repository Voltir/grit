package grit.tools

import grit.core.host.{Argv, HostError, RelPath, Uncommitted}

/** What an allowlisted `run` checks before a `git commit`, and the refusals the model reads when
  * the commit would leave out a file, or the check cannot be made. None counts as a repeat
  * ([[Refusals]]): each is one the model can fix.
  */
private[tools] object CommitCheck {

  /** The most paths a refusal lists of each kind before saying how many more there are. */
  val MostListed = 20

  /** Whether `argv`, a `git commit`, stages every tracked file's changes before committing:
    * `-a` or `--all`, alone or in a cluster of short flags before any that takes a value
    * (`-am`, not `-ma`). A flag's value is never read as a flag.
    */
  def all(argv: Argv): Boolean = {
    def after(args: List[String]): Boolean = args match {
      case Nil | "--" :: _ => false
      case "--all" :: _ => true
      case long :: rest if Valued(long) => after(rest.drop(1))
      case long :: rest if long.startsWith("--") => after(rest)
      case short :: rest if short.length > 1 && short.startsWith("-") =>
        val flags = short.drop(1)
        val valued = flags.indexWhere(ValuedShort.contains(_))
        val before = if (valued < 0) flags else flags.take(valued)
        before.contains('a') || after(if (valued == flags.length - 1) rest.drop(1) else rest)
      case _ :: rest => after(rest)
    }
    argv.args match {
      case "commit" :: rest => after(rest)
      case "-C" :: _ :: "commit" :: rest => after(rest)
      case _ => false
    }
  }

  // The short flags of `git commit` that take a value, attached or as the next argument.
  private val ValuedShort = Set('m', 'F', 'C', 'c', 't')

  // The long flags of `git commit` that take a value as the next argument when none is attached.
  private val Valued = Set(
    "--message",
    "--file",
    "--author",
    "--date",
    "--reuse-message",
    "--reedit-message",
    "--fixup",
    "--squash",
    "--template",
    "--cleanup",
    "--trailer",
    "--pathspec-from-file"
  )

  /** What a commit would leave out of `files`, by `argv`: with [[all]], only the untracked. */
  def leftOut(argv: Argv, files: Uncommitted.Files): Uncommitted.Files =
    if (all(argv)) files.copy(unstaged = Vector.empty) else files

  /** The refusal of a commit that would leave out `files`; `None` when it leaves out nothing. */
  def refusal(files: Uncommitted.Files): Option[String] = {
    def listed(heading: String, paths: Vector[RelPath]): Vector[String] =
      if (paths.isEmpty) Vector.empty
      else
        heading +: (paths.take(MostListed).map(p => s"- ${RelPath.value(p)}") ++
          Option.when(paths.size > MostListed)(s"- … and ${paths.size - MostListed} more"))
    val lines = listed("Untracked, neither committed nor ignored:", files.untracked) ++
      listed("Changed, not staged:", files.unstaged)
    Option.when(lines.nonEmpty)(
      s"$Refused it would leave out files the checkout holds. ${lines.mkString("\n")}" +
        "\n\nAdd each with `git add`, or remove it, or ignore it in `.gitignore`; then commit " +
        "again."
    )
  }

  /** The refusal of a commit whose check of `what` failed with `error`: it fails closed. */
  def unchecked(what: String, error: HostError): String =
    s"$Refused $what could not be checked. ${error.message}"

  /** The working tree, as [[unchecked]] names it. */
  val Tree = "the working tree"

  private val Refused = "The commit was refused, and nothing was committed:"
}
