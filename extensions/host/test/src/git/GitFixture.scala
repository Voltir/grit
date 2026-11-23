package grit.host.git

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.{Argv, Processes, Ran, RelPath}
import grit.core.place.{Repository, WorkName}
import grit.host.checkout.{LocalProcesses, LocalTree, Redaction}

/** A real git repository with one commit on `main`, made in a fresh directory beside a root for
  * working copies, and deleted after. git sees a home of its own in that directory, so no
  * person's configuration reaches it.
  */
object GitFixture {

  /** The repository's clone, the root its copies are made under, and git run with the
    * fixture's own home.
    */
  final class Repo(val source: Path, val root: Path, val processes: Processes^)

  /** The repository every test's copies are of. */
  val repository: Repository =
    Repository.of("grit").getOrElse(throw new IllegalArgumentException("grit"))

  /** `s` as a run's name, which a test writes valid. */
  def work(s: String): WorkName = WorkName.of(s).getOrElse(throw new IllegalArgumentException(s))

  /** `body` over a fresh repository, deleted after. */
  def inRepository[A](body: Repo^ => A): A = {
    val dir = Files.createTempDirectory("grit-git")
    try {
      val clone = Files.createDirectories(dir.resolve("clone"))
      val home = Files.createDirectories(dir.resolve("home"))
      val processes = new LocalProcesses(
        dir,
        Map("PATH" -> sys.env.getOrElse("PATH", "/usr/bin:/bin"), "HOME" -> home.toString),
        Redaction.of(Map.empty)
      )
      val repo = new Repo(clone, Files.createDirectories(dir.resolve("copies")), processes)
      val _ = git(repo, clone, "init", "--quiet", "-b", "main")
      val _ = git(
        repo,
        clone,
        "-c",
        "user.name=grit",
        "-c",
        "user.email=grit@example.invalid",
        "-c",
        "commit.gpgsign=false",
        "commit",
        "--quiet",
        "--allow-empty",
        "-m",
        "first"
      )
      body(repo)
    } finally { val _ = LocalTree.removed(dir) }
  }

  /** `git -C in args` in `repo`, which must exit 0. */
  def git(repo: Repo^, in: Path, args: String*): Ran = {
    val argv = Argv
      .of("git", ("-C" +: in.toString +: args)*)
      .getOrElse(throw new IllegalArgumentException(args.mkString(" ")))
    repo.processes.run(argv, RelPath.Root, 30.seconds) match {
      case Right(r) if r.exit == 0 => r
      case other => throw new AssertionError(s"${argv.written}: $other")
    }
  }

  /** Whether `in` has the local branch `branch`. */
  def hasBranch(repo: Repo^, in: Path, branch: String): Boolean = {
    val argv = Argv
      .of("git", "-C", in.toString, "rev-parse", "--verify", "--quiet", s"refs/heads/$branch")
      .getOrElse(throw new IllegalArgumentException(branch))
    repo.processes.run(argv, RelPath.Root, 30.seconds).exists(_.exit == 0)
  }
}
