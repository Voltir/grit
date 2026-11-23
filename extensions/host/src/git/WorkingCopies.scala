package grit.host.git

import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.host.{Argv, HostError, Processes, Ran, RelPath}
import grit.core.place.{Repository, WorkName}
import grit.host.checkout.Checkout.attempt
import grit.host.checkout.LocalTree

/** Working copies of `repository`'s clone at `clone`, each at `root/{repository}/{work}` on the
  * branch `grit/{work}`, git run through `processes`. Its first call sweeps: every copy unused
  * for [[Kept]] that is neither held nor holding changes never committed is removed, its branch
  * kept, and so is every directory there without its lock.
  */
final class WorkingCopies(
    val repository: Repository,
    clone: Path,
    root: Path,
    processes: Processes^,
    clock: Clock^
) extends caps.SharedCapability {
  import WorkingCopies.*

  private val home = clone.toAbsolutePath.normalize
  private val copies = root.toAbsolutePath.normalize.resolve(repository.name)
  private val swept = new AtomicBoolean(false)

  /** `work`'s copy, made on `grit/{work}` when it has none: the branch as it stands where it
    * exists, else a new branch cut from origin's default branch, fetched first (with no tags).
    * At [[Most]] copies, the least recently used one that is neither held nor holding changes
    * never committed is removed first, its branch kept; [[HostError.NoCopyFree]] when there is
    * none. `Failed`, no copy or branch made, when a new branch is wanted and the clone has no
    * origin, it cannot be fetched, or its default branch cannot be read: never cut from HEAD.
    */
  def opened(work: WorkName): Either[HostError, Path] = Made.synchronized {
    sweep()
    val dir = dirOf(WorkName.value(work))
    val lock = lockOf(dir)
    if (isCopy(dir) && Files.exists(lock)) used(dir)
    else
      for {
        _ <- cleared(dir)
        _ <- bounded(WorkName.value(work))
        _ <- locked(lock)
        _ <- made(work, dir)
        at <- used(dir)
      } yield at
  }

  /** `body` given `work`'s copy, as [[opened]] gives it, held while it runs: neither the bound
    * nor a sweep removes a held copy. The path outlives the hold; the copy may not. `Failed`
    * when another process holds it.
    */
  def held[A](work: WorkName)(body: Path => A): Either[HostError, A] = {
    val dir = dirOf(WorkName.value(work))
    val lock = lockOf(dir)
    hold(lock).flatMap { _ =>
      try opened(work).map(body)
      finally release(lock, dir)
    }
  }

  private def dirOf(name: String): Path = copies.resolve(name)

  /** `lock` held by this process, once more when it holds it already. */
  private def hold(lock: Path): Either[HostError, Unit] = Made.synchronized {
    Option(Holds.get(lock)) match {
      case Some(h) => Right { val _ = Holds.put(lock, h.copy(depth = h.depth + 1)) }
      case None =>
        locked(lock).flatMap { _ =>
          attempt(FileChannel.open(lock, StandardOpenOption.WRITE)).flatMap { channel =>
            attempt(Option(channel.tryLock())) match {
              case Right(Some(_)) => Right { val _ = Holds.put(lock, Holding(channel, 1)) }
              case other =>
                channel.close()
                Left(other.left.getOrElse(HostError.Failed(s"Another grit holds `$lock`.")))
            }
          }
        }
    }
  }

  /** One hold of `lock` let go; at the last, `dir` used now and the lock freed. */
  private def release(lock: Path, dir: Path): Unit = Made.synchronized {
    Option(Holds.get(lock)).foreach { h =>
      if (h.depth > 1) { val _ = Holds.put(lock, h.copy(depth = h.depth - 1)) }
      else {
        val _ = Holds.remove(lock)
        if (Files.isDirectory(dir)) { val _ = used(dir) }
        h.channel.close()
      }
    }
  }

  /** `lock` there, made when it is not: never opened, as this process may hold it. */
  private def locked(lock: Path): Either[HostError, Unit] = attempt {
    val _ = Files.createDirectories(copies)
    if (!Files.exists(lock)) { val _ = Files.createFile(lock) }
  }

  /** The names of the copies there are, a lock or a directory each. */
  private def names(): Either[HostError, Set[String]] = attempt {
    if (!Files.isDirectory(copies)) Set.empty
    else {
      val listing = Files.list(copies)
      try listing.iterator.asScala.map(_.getFileName.toString.stripSuffix(".lock")).toSet
      finally listing.close()
    }
  }

  /** Room for `name`'s copy: below [[Most]] others, the least recently used free ones removed. */
  private def bounded(name: String): Either[HostError, Unit] =
    names().flatMap { all =>
      val others = all - name
      val over = others.size - Most + 1
      if (over <= 0) Right(())
      else {
        val gone = others.toVector.sortBy(lastUsed).iterator.filter(free).take(over).size
        if (gone > 0) pruned()
        if (gone == over) Right(()) else Left(HostError.NoCopyFree(Most))
      }
    }

  /** When `name`'s copy was last used: its directory's time, the start of time without one. */
  private def lastUsed(name: String): Long = {
    val dir = dirOf(name)
    if (Files.isDirectory(dir)) Files.getLastModifiedTime(dir).toMillis else 0L
  }

  /** Whether `name`'s copy was removed: not when held, by this process or another, nor when it
    * holds changes never committed.
    */
  private def free(name: String): Boolean = {
    val dir = dirOf(name)
    val lock = lockOf(dir)
    !Holds.containsKey(lock) && clean(dir) &&
    (if (Files.exists(lock)) LocalTree.free(lock)(_ => ()) else LocalTree.removed(dir).isEmpty)
  }

  /** Whether `dir` holds nothing never committed: no directory, or git showing no change. */
  private def clean(dir: Path): Boolean =
    !isCopy(dir) || git(dir, "status", "--porcelain").exists(_.output.text.trim.isEmpty)

  /** Whether `dir` is a worktree git made: its `.git` there, so git looks no further up. */
  private def isCopy(dir: Path): Boolean = Files.exists(dir.resolve(".git"))

  /** `dir` gone when it is there but no copy, as an interrupted removal leaves it. */
  private def cleared(dir: Path): Either[HostError, Unit] =
    if (!Files.exists(dir)) Right(())
    else
      LocalTree.removed(dir) match {
        case left if left.nonEmpty => Left(HostError.Failed(s"`$dir` could not be removed."))
        case _ => Right(pruned())
      }

  /** At this instance's first call, the sweep its doc names. */
  private def sweep(): Unit = if (!swept.getAndSet(true)) {
    val cutoff = clock.now().toEpochMilli - Kept.toMillis
    names().foreach { all =>
      val gone = all.toVector.filter { name =>
        val dir = dirOf(name)
        if (Files.exists(lockOf(dir))) lastUsed(name) < cutoff && free(name)
        else LocalTree.removed(dir).isEmpty
      }
      if (gone.nonEmpty) pruned()
    }
  }

  /** The clone's records of worktrees whose directory is gone, dropped. */
  private def pruned(): Unit = { val _ = git(home, "worktree", "prune") }

  /** `dir` made a worktree of the clone, on `grit/{work}`: the branch as it stands when it
    * exists, else a new one cut from [[base]]. The lock beside it exists first.
    */
  private def made(work: WorkName, dir: Path): Either[HostError, Unit] = {
    val branch = s"grit/${WorkName.value(work)}"
    for {
      found <- ran(home, "rev-parse", "--verify", "--quiet", s"refs/heads/$branch")
      _ <-
        if (found._2.exit == 0) git(home, "worktree", "add", "--quiet", dir.toString, branch)
        else
          base().flatMap(from =>
            git(home, "worktree", "add", "--quiet", "--no-track", "-b", branch, dir.toString, from)
          )
    } yield ()
  }

  /** What a new branch is cut from: origin's default branch, fetched now, never the clone's
    * HEAD, which may hold a person's commits never pushed. `origin/HEAD` names the default; when
    * the clone has none, origin is asked once. `Failed` when the clone has no origin, it cannot
    * be fetched, or its default branch cannot be read.
    */
  private def base(): Either[HostError, String] = {
    def head: Either[HostError, (Argv, Ran)] =
      ran(home, "symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD")
    for {
      _ <- git(home, "remote", "get-url", "origin").left.map(_ =>
        HostError.Failed(s"The clone at $home has no remote origin to cut a run's branch from.")
      )
      _ <- git(home, "fetch", "--quiet", "--no-tags", "origin")
      named <- head.flatMap {
        case (_, r) if r.exit == 0 => Right(r)
        case _ => git(home, "remote", "set-head", "origin", "--auto").flatMap(_ => head.map(_._2))
      }
      default = named.output.text.trim
      _ <-
        if (named.exit == 0 && default.startsWith("origin/")) Right(())
        else Left(HostError.Failed(s"origin's default branch cannot be read: ${named.output.text}"))
    } yield default
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

  /** 8: the most copies of one repository. */
  val Most: Int = 8

  /** 7 days: how long a copy neither held nor holding changes is kept unused. */
  val Kept: FiniteDuration = FiniteDuration(7, TimeUnit.DAYS)

  /** 2 minutes: the longest a git command runs. */
  val GitLongest: FiniteDuration = FiniteDuration(2, TimeUnit.MINUTES)

  /** What this process's copies are made, held and removed under, one at a time. */
  private object Made

  /** A lock this process holds, and how many holds are open on it. */
  private final case class Holding(channel: FileChannel, depth: Int)

  /** The locks this process holds, by path. Process-wide, not one instance's: a lock is the
    * process's, and opening a lock this process holds lets go of it (POSIX locks), so every
    * instance must see every hold before it touches a lock. Read and changed under [[Made]].
    */
  private val Holds = new java.util.HashMap[Path, Holding]()

  private def lockOf(dir: Path): Path = dir.resolveSibling(s"${dir.getFileName}.lock")
}
