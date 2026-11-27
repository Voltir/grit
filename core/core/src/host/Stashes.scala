package grit.core.host

/** The git stash a checkout's commands reach, read for the machine guard before a command that
  * takes an entry off it.
  */
trait Stashes extends caps.SharedCapability {

  /** `Left` when git cannot say: the checkout is not one, or git fails. */
  def held: Either[HostError, Stashes.Held]
}

object Stashes {

  /** The stash as it stands: whether other checkouts reach it (`shared`: the checkout is a
    * linked worktree of a clone, whose stash every worktree of it shares), the checkout's
    * `branch` (`None` when it is on none), and each entry's branch, `stash@{0}` first (`None`
    * for an entry whose label names none).
    */
  final case class Held(shared: Boolean, branch: Option[String], entries: Vector[Option[String]])
      extends caps.Pure

  /** The branch a `git stash list` line names: `main` in `stash@{0}: WIP on main: 1a2b3c4 x`
    * and in `stash@{1}: On main: x`; `None` for a line of neither form.
    */
  def branchOf(line: String): Option[String] =
    line.split(": ", 3).toList match {
      case _ :: label :: _ if label.startsWith("WIP on ") => Some(label.stripPrefix("WIP on "))
      case _ :: label :: _ if label.startsWith("On ") => Some(label.stripPrefix("On "))
      case _ => None
    }
}
