package grit.host.git

import java.net.URI

import grit.core.host.{
  Argv,
  Branch,
  Handing,
  HostError,
  Processes,
  Ran,
  RelPath,
  RemoteRepository,
  Variables
}
import grit.core.place.WorkName
import grit.core.secret.Secret

/** A repository a run's branch is pushed to, as declared: an `https://` address with no user or
  * password in it, or an absolute path; `written` is made only of ASCII letters, digits and
  * `/._~:-`.
  */
final case class Remote private (written: String) {

  /** Whether git fetching from `url`, as `git remote get-url` gives it, fetches from this
    * remote. For an `https` remote: `url` an `https` address with no user or port,
    * `git@{host}:{path}` or `ssh://git@{host}[:22]/{path}`, its host the same lowercased and
    * its path the same less one trailing `/` or `.git`. For a path: `url` that path. Anything
    * else is not.
    */
  def fetchedBy(url: String): Boolean =
    if (written.startsWith("/")) url == written
    else
      (Remote.site(written), Remote.site(url)) match {
        case (Some(ours), Some(theirs)) => ours == theirs
        case _ => false
      }
}

object Remote {

  /** The host, lowercased, and the path, less one trailing `/` or `.git`, that `url` fetches
    * from, when it is one of the forms [[Remote.fetchedBy]] takes; `None` otherwise.
    */
  private def site(url: String): Option[(String, String)] = {
    def trimmed(path: String) =
      if (path.endsWith("/")) path.dropRight(1)
      else if (path.endsWith(".git")) path.dropRight(4)
      else path
    url match {
      case Https(host, path) => Some((host.toLowerCase, trimmed(path)))
      case Ssh(host, path) => Some((host.toLowerCase, trimmed(path)))
      case Scp(host, path) => Some((host.toLowerCase, trimmed(s"/$path")))
      case _ => None
    }
  }

  private val Https = """https://([A-Za-z0-9.-]+)(/[^?#]*)""".r
  private val Ssh = """ssh://git@([A-Za-z0-9.-]+)(?::22)?(/[^?#]*)""".r
  private val Scp = """git@([A-Za-z0-9.-]+):([^/][^?#]*)""".r

  /** `url` with any user and password in it dropped, so it may be shown. */
  def shown(url: String): String = url.replaceFirst("^([A-Za-z][A-Za-z0-9+.-]*://)[^/]*@", "$1")

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

/** The branch `grit/{work}` of the clone that `processes` and `handing` run at, pushed with
  * `secret` and offered as `target` says. git runs in the clone, never the run's copy, so nothing
  * a run writes in its copy reaches the push; it reads `secret` through a credential helper that
  * git asks only for `target`'s remote, with the clone's own helpers cleared and its hooks not
  * run. Each git command runs for at most [[WorkingCopies.GitLongest]].
  */
final class LocalBranch(
    target: PushTarget^,
    work: WorkName,
    processes: Processes^,
    handing: Handing^,
    secret: Secret
) extends Branch {

  private val to = target.remote
  private val remoteRepository = target.repository
  private val branch = s"grit/${WorkName.value(work)}"
  private val ref = s"refs/heads/$branch"

  def offer(title: String, body: String): Either[HostError, URI] =
    ahead()
      .flatMap { a =>
        if (a) Right(())
        else
          Left(
            HostError.Failed(
              s"$branch holds nothing origin's default branch lacks, so nothing was pushed and " +
                "no pull request was opened. Commit your change first."
            )
          )
      }
      .flatMap(_ => remoteRepository.pullRequest(branch))
      .flatMap {
        case Some(RemoteRepository.PullRequest.Merged(at, _)) =>
          Left(LocalBranch.ended(branch, at, "merged"))
        case Some(RemoteRepository.PullRequest.Closed(at, _)) =>
          Left(LocalBranch.ended(branch, at, "closed"))
        case Some(RemoteRepository.PullRequest.Open(at)) => pushed().map(_ => at)
        case None => pushed().flatMap(_ => remoteRepository.open(branch, title, body))
      }

  def stands: Either[HostError, Branch.Stands] =
    for {
      local <- head()
      remote <- remoteHead()
      pull <- remoteRepository.pullRequest(branch)
    } yield pull match {
      case Some(RemoteRepository.PullRequest.Merged(at, _)) => Branch.Stands.Merged(at)
      case Some(RemoteRepository.PullRequest.Closed(at, _)) => Branch.Stands.Closed(at)
      case _ if !remote.contains(local) => Branch.Stands.Unpushed
      case Some(RemoteRepository.PullRequest.Open(at)) => Branch.Stands.Open(at)
      case None => Branch.Stands.Pushed
    }

  /** Whether the branch holds a commit that origin's default branch lacks, as the clone last
    * fetched it: a stale fetch can only answer `true` wrongly, never `false`. `Failed` when the
    * clone names no default branch of origin, or has no such branch.
    */
  private[git] def ahead(): Either[HostError, Boolean] =
    for {
      symbolic <- local("symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD")
      default <-
        if (symbolic.exit == 0) Right(symbolic.output.text.trim)
        else Left(HostError.Failed("The clone names no default branch of origin."))
      count <- local("rev-list", "--count", s"$default..$ref")
      n <- count.output.text.trim.toIntOption
        .filter(_ => count.exit == 0)
        .toRight(HostError.Failed(s"The clone has no branch $branch."))
    } yield n > 0

  /** git with `args` in the clone, its own credentials and no secret. */
  private def local(args: String*): Either[HostError, Ran] =
    Argv
      .of("git", args*)
      .left
      .map(HostError.Failed(_))
      .flatMap(argv => processes.run(argv, RelPath.Root, WorkingCopies.GitLongest))

  /** The branch's last commit in the clone. */
  private def head(): Either[HostError, String] =
    local("rev-parse", "--verify", "--quiet", ref)
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

  /** Why `branch` is not offered again: its pull request at `at` was `how`, merged or closed. */
  private def ended(branch: String, at: URI, how: String): HostError =
    HostError.Failed(
      s"The pull request from $branch at $at was $how; grit does not open another. Nothing " +
        "was pushed. Run the task under a new --run name to offer the change again."
    )

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
