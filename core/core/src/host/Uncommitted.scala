package grit.core.host

/** The files a commit in the checkout would leave out, read by git before a coding turn's
  * `git commit`.
  */
trait Uncommitted extends caps.SharedCapability {

  /** `Left` when git cannot say: the checkout is not one, or git fails. */
  def read: Either[HostError, Uncommitted.Files]
}

object Uncommitted {

  /** Tracked files whose changes are not staged, and files git neither tracks nor ignores, in
    * git's order.
    */
  final case class Files(unstaged: Vector[RelPath], untracked: Vector[RelPath]) extends caps.Pure
}
