package grit.host.git

import grit.core.host.{Argv, HostError, Processes, Ran, RelPath}
import grit.core.store.CheckoutState

/** How a checkout differs from origin's default branch, read by git there. */
object BranchDiff {

  /** How the checkout at the root `processes` runs programs in differs from origin's default
    * branch, as [[CheckoutState.Diff]] says: git's diff stat, of at most
    * [[CheckoutState.StatFiles]] files, against the commit where HEAD left that branch
    * (`git merge-base`), and the files git neither tracks nor ignores. Each git command is given
    * [[WorkingCopies.GitLongest]]. Left: the checkout names no default branch of origin
    * (`refs/remotes/origin/HEAD`), or a git command fails, cannot start or runs past its time.
    */
  def read(processes: Processes^): Either[HostError, CheckoutState.Diff] = {
    def git(args: Seq[String]): Either[HostError, Ran] =
      Argv
        .of("git", args*)
        .left
        .map(HostError.Failed(_))
        .flatMap(processes.run(_, RelPath.Root, WorkingCopies.GitLongest))
    def output(args: String*): Either[HostError, String] = git(args).flatMap { r =>
      if (r.exit == 0) Right(r.output.text)
      else
        Left(
          HostError.Failed(
            s"git ${args.mkString(" ")} exited ${r.exit}: ${r.output.text.linesIterator.mkString(" ").take(200)}"
          )
        )
    }
    for {
      base <- DefaultBranch
        .of(git)
        .flatMap(
          _.toRight(
            HostError.Failed(
              "The checkout names no default branch of origin (refs/remotes/origin/HEAD)."
            )
          )
        )
      fork <- output("merge-base", base, "HEAD").map(_.trim)
      stat <- output("diff", "--stat=100", s"--stat-count=${CheckoutState.StatFiles}", fork)
      untracked <- output("ls-files", "--others", "--exclude-standard")
    } yield CheckoutState.Diff.of(base, stat, untracked.linesIterator.filter(_.nonEmpty).toVector)
  }
}
