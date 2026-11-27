package grit.host.git

import grit.core.host.{HostError, Ran}

/** The branch of origin a clone takes as its default, as it last fetched it. */
private[git] object DefaultBranch {

  /** The branch `refs/remotes/origin/HEAD` names, `origin/{name}`, read by `git`, which runs git
    * with the arguments it is given in the clone; `None` when it names none. Nothing is asked of
    * origin.
    */
  def of(git: Seq[String] => Either[HostError, Ran]): Either[HostError, Option[String]] =
    git(Seq("symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD")).map { r =>
      val default = r.output.text.trim
      Option.when(r.exit == 0 && default.startsWith("origin/"))(default)
    }
}
