package grit.core.host

import grit.core.place.Directory

/** The instruction files around a directory on this machine: what people wrote for agents
  * working there.
  */
trait Instructions extends caps.SharedCapability {

  /** From `/` down to `dir`, each directory's `AGENTS.md`, else its `CLAUDE.md` (never both),
    * the farthest directory's first. Each as it reads now, cut at [[Instructions.MaxBytes]] on
    * a character boundary. A directory whose file cannot be read, or is not UTF-8, gives
    * nothing; none at all when `dir` is not a directory.
    */
  def around(dir: Directory): Vector[InstructionFile]
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
