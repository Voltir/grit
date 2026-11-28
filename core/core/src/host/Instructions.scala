package grit.core.host

import grit.core.place.Directory

/** The instruction files around a directory on this machine, what people wrote for agents
  * working there, and whether git tracks it.
  */
trait Instructions extends caps.SharedCapability {

  /** From `/` down to `dir`, each directory's `AGENTS.md`, else its `CLAUDE.md` (never both),
    * the farthest directory's first. Each as it reads now, cut at [[Instructions.MaxBytes]] on
    * a character boundary. A directory whose file cannot be read, or is not UTF-8, gives
    * nothing; none at all when `dir` is not a directory.
    */
  def around(dir: Directory): Vector[InstructionFile]

  /** Whether git tracks work in `dir`: it or a directory above it holds `.git` (a repository's
    * own, or the file a linked worktree has). False when `dir` is not a directory.
    */
  def inWorkTree(dir: Directory): Boolean
}

object Instructions {

  /** The most of one file [[Instructions.around]] reads: 64 KiB. */
  val MaxBytes: Int = 64 * 1024

  /** The files each directory is searched for, in order: the first found is its file. */
  val Names: Vector[String] = Vector("AGENTS.md", "CLAUDE.md")
}

/** An instruction file as [[Instructions.around]] found it: its absolute `path`, its `text`,
  * and whether that text was `cut` short at the cap.
  */
final case class InstructionFile(path: String, text: String, cut: Boolean)

/** A repository's own instruction file, the one at its root named `name`: its `text`, and
  * whether that text was `cut` short at [[Instructions.MaxBytes]].
  */
final case class RepositoryInstructions private (name: String, text: String, cut: Boolean)

object RepositoryInstructions {

  /** The file `name` at a repository's root, holding `text`; why not when `name` is none of
    * [[Instructions.Names]].
    */
  def of(name: String, text: String, cut: Boolean): Either[String, RepositoryInstructions] =
    if (Instructions.Names.contains(name)) Right(new RepositoryInstructions(name, text, cut))
    else Left(s"$name is not an instruction file: one of ${Instructions.Names.mkString(", ")}")
}
