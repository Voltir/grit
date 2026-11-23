package grit.host.git

import java.net.URI

import grit.core.host.{Argv, Branch, Forge, Handing, HostError, Processes, Ran, RelPath, Variables}
import grit.core.place.WorkName
import grit.core.secret.Secret

/** A repository a run's branch is pushed to, as declared: an `https://` address with no user or
  * password in it, or an absolute path; `written` is made only of ASCII letters, digits and
  * `/._~:-`.
  */
final case class Remote private (written: String)

object Remote {

  /** `written` as a remote, or why not. */
  def of(written: String): Either[String, Remote] =
    if (Shape.matches(written)) Right(new Remote(written))
    else
      Left(
        s"A repository is pushed to at an https:// address with no user in it, or an absolute " +
          s"path, written in letters, digits and /._~:- alone; not $written."
      )

  private val Shape = """https://[A-Za-z0-9._~:/-]+|/[A-Za-z0-9._~/-]+""".r
}

/** The branch `grit/{work}` of the clone that `processes` and `handing` run at, pushed to `to`
  * with `secret` and offered on `forge`. git runs in the clone, never the run's copy, so nothing
  * a run writes in its copy reaches the push; it reads `secret` through a credential helper that
  * git asks only for `to`, with the clone's own helpers cleared and its hooks not run. Each git
  * command runs for at most [[WorkingCopies.GitLongest]].
  */
final class LocalBranch(
    to: Remote,
    work: WorkName,
    processes: Processes^,
    handing: Handing^,
    secret: Secret,
    forge: Forge^
) extends Branch {

  private val branch = s"grit/${WorkName.value(work)}"
  private val ref = s"refs/heads/$branch"

  def offer(title: String, body: String): Either[HostError, URI] =
    for {
      _ <- pushed()
      open <- forge.opened(branch)
      at <- open match {
        case Some(at) => Right(at)
        case None => forge.open(branch, title, body)
      }
    } yield at

  def stands: Either[HostError, Branch.Stands] =
    for {
      local <- head()
      remote <- remoteHead()
      stands <-
        if (!remote.contains(local)) Right(Branch.Stands.Unpushed)
        else
          forge.opened(branch).map {
            case Some(at) => Branch.Stands.Open(at)
            case None => Branch.Stands.Pushed
          }
    } yield stands

  /** The branch's last commit in the clone. */
  private def head(): Either[HostError, String] =
    Argv
      .of("git", "rev-parse", "--verify", "--quiet", ref)
      .left
      .map(HostError.Failed(_))
      .flatMap(argv => processes.run(argv, RelPath.Root, WorkingCopies.GitLongest))
      .flatMap { r =>
        if (r.exit == 0) Right(r.output.text.trim)
        else Left(HostError.Failed(s"The clone has no branch $branch."))
      }

  /** The branch pushed to `to`: only it, under its own name, never forced, no tag with it. */
  private def pushed(): Either[HostError, Unit] =
    handed("push", "--quiet", "--no-follow-tags", to.written, s"$ref:$ref").flatMap { (_, r) =>
      if (r.exit == 0) Right(())
      else
        Left(
          HostError.Failed(s"Pushing $branch to ${to.written} exited ${r.exit}: ${r.output.text}")
        )
    }

  /** The commit `to` holds at the branch; `None` when it has no such branch. */
  private def remoteHead(): Either[HostError, Option[String]] =
    handed("ls-remote", to.written, ref).flatMap { (ended: (Vector[String], Ran)) =>
      val (lines, r) = ended
      val at = lines.find(_.endsWith(s"\t$ref")).map(_.takeWhile(_ != '\t'))
      if (r.exit == 0) Right(at)
      else
        Left(
          HostError.Failed(s"Reading $branch at ${to.written} exited ${r.exit}: ${r.output.text}")
        )
    }

  /** git with `args`, handed `secret` as [[LocalBranch.argv]] has it: what it wrote, and how it
    * ended.
    */
  private def handed(args: String*): Either[HostError, (Vector[String], Ran)] =
    LocalBranch.Quiet.left
      .map(HostError.Failed(_))
      .flatMap(quiet =>
        handing
          .started(RelPath.Root, secret, quiet)(path => LocalBranch.argv(to, path, args*)) { s =>
            val lines: Vector[String] = Vector.from(s.lines)
            s.await(WorkingCopies.GitLongest).map((lines, _))
          }
          .flatten
      )
}

object LocalBranch {

  /** git run with `args`, asking the secret's file at `path` for a password only for `to`: every
    * credential helper the clone and its user declare cleared, one reading the file declared for
    * `to` alone, and no hook run. Why not: `path` holds a character outside `A-Za-z0-9/._-`, as
    * it would be read into the helper's shell unquoted.
    */
  private[git] def argv(to: Remote, path: String, args: String*): Either[String, Argv] =
    if (!HandedPath.matches(path))
      Left("The secret's file is at a path the credential helper cannot name.")
    else
      Argv.of(
        "git",
        (Seq(
          "-c",
          "credential.helper=",
          "-c",
          s"credential.${to.written}.helper=${helper(path)}",
          "-c",
          "core.hooksPath=/dev/null"
        ) ++ args)*
      )

  /** The credential helper reading the password from the file at `path`; `printf` is the
    * shell's own, so the password is in no program's arguments.
    */
  private[git] def helper(path: String): String =
    s"""!f() { test "$$1" = get && printf 'username=x-access-token\\npassword=%s\\n' "$$(cat $path)"; }; f"""

  private val HandedPath = """[A-Za-z0-9/._-]+""".r

  /** git asking no one at a terminal. */
  private val Quiet: Either[String, Variables] = Variables.of(Map("GIT_TERMINAL_PROMPT" -> "0"))
}
