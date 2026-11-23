package grit.host.git

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import grit.core.clock.Clock
import grit.core.host.{Argv, HostError, Processes, Ran, RelPath}
import grit.core.place.{Repository, WorkName}
import grit.host.checkout.Checkout.attempt

/** Working copies of `repository`'s clone at `clone`, each at `root/{repository}/{work}` on the
  * branch `grit/{work}`, git run through `processes`.
  */
final class WorkingCopies(
    repository: Repository,
    clone: Path,
    root: Path,
    processes: Processes^,
    clock: Clock^
) extends caps.SharedCapability {
  import WorkingCopies.*

  private val home = clone.toAbsolutePath.normalize
  private val copies = root.toAbsolutePath.normalize.resolve(repository.name)

  /** `work`'s copy, made on `grit/{work}` when it has none: from the clone's HEAD, or from the
    * branch where it exists.
    */
  def opened(work: WorkName): Either[HostError, Path] = Made.synchronized {
    val dir = copies.resolve(WorkName.value(work))
    val lock = lockOf(dir)
    if (Files.isDirectory(dir) && Files.exists(lock)) used(dir)
    else
      for {
        _ <- attempt {
          val _ = Files.createDirectories(copies)
          if (!Files.exists(lock)) { val _ = Files.createFile(lock) }
        }
        _ <- made(work, dir)
        at <- used(dir)
      } yield at
  }

  /** `dir` made a worktree of the clone, on `grit/{work}`. The lock beside it exists first. */
  private def made(work: WorkName, dir: Path): Either[HostError, Unit] = {
    val branch = s"grit/${WorkName.value(work)}"
    for {
      found <- ran(home, "rev-parse", "--verify", "--quiet", s"refs/heads/$branch")
      _ <-
        if (found._2.exit == 0) git(home, "worktree", "add", "--quiet", dir.toString, branch)
        else git(home, "worktree", "add", "--quiet", "-b", branch, dir.toString)
    } yield ()
  }

  /** `dir`, its last use now. */
  private def used(dir: Path): Either[HostError, Path] =
    attempt(Files.setLastModifiedTime(dir, FileTime.from(clock.now())))

  /** `git -C in args`, and what it did; a git that cannot start is a `Left`. */
  private def ran(in: Path, args: String*): Either[HostError, (Argv, Ran)] =
    Argv
      .of("git", ("-C" +: in.toString +: args)*)
      .left
      .map(HostError.Failed(_))
      .flatMap(argv => processes.run(argv, RelPath.Root, GitLongest).map((argv, _)))

  /** As [[ran]], a non-zero exit a `Left` carrying git's output. */
  private def git(in: Path, args: String*): Either[HostError, Ran] =
    ran(in, args*).flatMap { (argv, r) =>
      if (r.exit == 0) Right(r)
      else Left(HostError.Failed(s"`${argv.written}` exited ${r.exit}: ${r.output.text}"))
    }
}

object WorkingCopies {

  /** 2 minutes: the longest a git command runs. */
  val GitLongest: FiniteDuration = FiniteDuration(2, TimeUnit.MINUTES)

  /** What every process's copies are made and removed under, one at a time. */
  private object Made

  private def lockOf(dir: Path): Path = dir.resolveSibling(s"${dir.getFileName}.lock")
}
