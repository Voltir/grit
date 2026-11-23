package grit.host.git

import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.host.{
  Argv,
  Clipped,
  HostError,
  Instructions,
  Processes,
  Ran,
  RelPath,
  RemoteRepository,
  RepositoryInstructions,
  Started,
  Workspace
}
import grit.core.place.{Repository, WorkName}
import grit.host.checkout.Checkout.attempt
import grit.host.checkout.LocalTree

/** Working copies of `repository`'s clone at `clone`, each at `root/{repository}/{work}` on the
  * branch `grit/{work}`, git run through `processes`. Its first call sweeps: every copy unused
  * for [[Kept]] that is neither held nor holding changes never committed is removed, its branch
  * kept, and so is every directory there without its lock. With `target`, a new branch is cut
  * only when the clone's origin is `target`'s remote ([[Remote.fetchedBy]]); `Failed` otherwise,
  * naming both, no copy or branch made. With `target`, and the clone's origin its remote, the
  * sweep also deletes each local `grit/{work}` branch with no copy that is done with: one
  * origin's default branch holds all of, with no call; else, asking `target`'s remote
  * repository, oldest first and on from where the last sweep stopped, at most [[SweepAsks]]
  * calls a sweep (so every branch is reached within ceil(branches / [[SweepAsks]]) sweeps), one
  * whose pull request was merged or closed with the branch's tip its head, or one with none
  * opened whose tip was committed past [[Unpublished]] ago, said through `note` first. The
  * first answer the remote repository cannot give ends it; a branch there is never deleted.
  */
final class WorkingCopies(
    val repository: Repository,
    clone: Path,
    root: Path,
    processes: Processes^,
    clock: Clock^,
    target: Option[PushTarget^] = None,
    note: String -> Unit = _ => ()
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

  /** `work`'s copy removed, its branch kept, unless it holds changes never committed or another
    * process holds it.
    */
  def removed(work: WorkName): WorkingCopies.Removed = Made.synchronized {
    val name = WorkName.value(work)
    val dir = dirOf(name)
    if (!clean(dir)) WorkingCopies.Removed.Uncommitted(dir)
    else if (free(name)) {
      pruned()
      WorkingCopies.Removed.Gone
    } else WorkingCopies.Removed.Held(dir)
  }

  /** The repository's own instruction file: the first of [[Instructions.Names]] committed at its
    * root as a file on origin's default branch, as the clone last fetched it, never its working
    * tree or its own commits; its lines each ended by `\n` and redacted as `processes` redacts
    * what programs show, cut at [[Instructions.MaxBytes]] on a character boundary. `None` when
    * neither name is committed there. `Failed` when the clone has no origin, its origin is not
    * `target`'s remote, `refs/remotes/origin/HEAD` names no default branch (nothing is asked
    * of origin), the only one there is a link (never followed), it is over
    * [[Workspace.MaxFileBytes]], it holds a NUL or bytes that are not UTF-8, or git fails or runs
    * past [[GitLongest]].
    */
  def instructionFile(): Either[HostError, Option[RepositoryInstructions]] = Made.synchronized {
    // `ls-tree -l`: "{mode} {type} {sha} {size}\t{name}", one line per name found at the root.
    val Entry = """(\d+) (\w+) ([0-9a-f]+) +(-|\d+)\t(.+)""".r
    for {
      _ <- fetchesFromTarget()
      default <- defaultBranch().flatMap {
        case Some(d) => Right(d)
        case None =>
          Left(
            HostError.Failed(
              s"origin's default branch is not named in the clone at $home (it has no " +
                "refs/remotes/origin/HEAD), so no instruction file is read."
            )
          )
      }
      listed <- git(home, ("ls-tree" +: "-l" +: default +: "--" +: Instructions.Names)*)
      entries = listed.output.text.linesIterator.collect {
        case Entry(mode, kind, sha, size, name) =>
          (name, (mode, kind, sha, size))
      }.toMap
      read <- Instructions.Names.flatMap(n => entries.get(n).map(n -> _)).collectFirst {
        case (name, ("100644" | "100755", "blob", sha, size)) => (name, sha, size.toLong)
      } match {
        case Some((name, _, size)) if size > Workspace.MaxFileBytes =>
          Left(
            HostError.Failed(
              s"$name at the root of ${repository.name} is $size bytes, over the " +
                s"${Workspace.MaxFileBytes} read, so it is not read."
            )
          )
        case Some((name, sha, _)) => blob(name, sha).map(Some(_))
        case None =>
          Instructions.Names.find(n => entries.get(n).exists(_._1 == "120000")) match {
            case Some(link) =>
              Left(
                HostError.Failed(
                  s"$link is committed at the root of ${repository.name} only as a link, which " +
                    "is not followed."
                )
              )
            case None => Right(None)
          }
      }
    } yield read
  }

  /** The blob `sha`, the file `name`, read as [[instructionFile]] reads it. */
  private def blob(name: String, sha: String): Either[HostError, RepositoryInstructions] = {
    val deadline = clock.millis() + GitLongest.toMillis
    def left = FiniteDuration(math.max(0L, deadline - clock.millis()), TimeUnit.MILLISECONDS)
    val most = Instructions.MaxBytes
    @tailrec def lines(kept: StringBuilder, bytes: Int)(using
        s: Started^
    ): Either[HostError, Boolean] =
      if (bytes > most) { s.kill(); Right(true) }
      else
        s.next(left) match {
          case Started.Next.Line(line) =>
            val ended = line + "\n"
            val _ = kept.append(ended)
            lines(kept, bytes + ended.getBytes(java.nio.charset.StandardCharsets.UTF_8).length)
          case Started.Next.Quiet =>
            s.kill()
            Left(HostError.Failed(s"`git cat-file` ran past $GitLongest reading $name."))
          case Started.Next.Ended =>
            s.await(left).flatMap { r =>
              if (r.exit == 0) Right(false)
              else
                Left(
                  HostError
                    .Failed(s"`git cat-file` exited ${r.exit} reading $name: ${r.output.text}")
                )
            }
        }
    for {
      argv <- Argv
        .of("git", "-C", home.toString, "cat-file", "blob", sha)
        .left
        .map(HostError.Failed(_))
      kept = new StringBuilder
      over <- processes
        .started(argv, RelPath.Root)(started => lines(kept, 0)(using started))
        .flatten
      text = kept.toString
      _ <-
        if (text.exists(c => c == '\u0000' || c == '�'))
          Left(
            HostError.Failed(
              s"$name at the root of ${repository.name} is not UTF-8 text, so it is not read."
            )
          )
        else Right(())
      cut = Clipped.prefix(text, most)
      file <- RepositoryInstructions
        .of(name, cut, over || cut.length < text.length)
        .left
        .map(HostError.Failed(_))
    } yield file
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
    target match {
      case Some(t) => branchesSwept(t)
      case None => ()
    }
  }

  /** With `target`, at the sweep, and only when the clone's origin is `target`'s remote: each
    * local `grit/{work}` branch with no copy is deleted when it is done with. One origin's default
    * branch holds all of is deleted with no call. Every other is asked of `target`'s remote
    * repository, oldest first, starting after the branch the last sweep asked last, at most
    * [[SweepAsks]] calls a sweep, so every such branch is reached within ceil(branches /
    * [[SweepAsks]]) sweeps; it is deleted when its pull request was merged or closed with its
    * head the branch's tip, or none was opened and its tip was committed more than
    * [[Unpublished]] ago (said through `note` first, naming the branch and its tip). The first
    * answer the remote repository cannot give ends it, nothing more deleted; a branch on the
    * remote repository is never deleted.
    */
  private def branchesSwept(t: PushTarget^): Unit = {
    val origin = git(home, "remote", "get-url", "origin").toOption
      .map(_.output.text.linesIterator.nextOption().getOrElse("").trim)
    if (origin.exists(t.remote.fetchedBy)) {
      val default =
        ran(home, "symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD").toOption
          .collect { case (_, r) if r.exit == 0 => r.output.text.trim }
      val cutoff = clock.now().getEpochSecond - Unpublished.toSeconds
      val held = names().getOrElse(Set.empty)
      val branches = git(
        home,
        "for-each-ref",
        "--sort=committerdate",
        "--format=%(refname) %(objectname) %(committerdate:unix)",
        "refs/heads/grit/"
      ).toOption.toVector
        .flatMap(_.output.text.linesIterator.toVector)
        .flatMap { line =>
          line.split(' ') match {
            case Array(ref, tip, at)
                if ref.startsWith("refs/heads/") && at.toLongOption.isDefined =>
              Vector((ref.stripPrefix("refs/heads/"), tip, at.toLong))
            case _ => Vector.empty
          }
        }
        .filterNot((branch, _, _) => held.contains(branch.stripPrefix("grit/")))
      def deleted(branch: String): Unit = { val _ = git(home, "branch", "--quiet", "-D", branch) }
      val (merged, asking) = branches.partition((branch, _, _) =>
        default.exists(d =>
          git(home, "rev-list", "--count", s"$d..refs/heads/$branch")
            .exists(_.output.text.trim == "0")
        )
      )
      merged.foreach((branch, _, _) => deleted(branch))
      val after = attempt(Files.readString(cursor).trim).toOption
      val from = asking.indexWhere(_._1 == after.getOrElse("")) + 1
      @tailrec def each(left: List[(String, String, Long)], asked: Int): Unit = left match {
        case (branch, tip, at) :: rest if asked < SweepAsks =>
          t.repository.pullRequest(branch) match {
            case Left(_) => ()
            case Right(pull) =>
              val _ = attempt(Files.writeString(cursor, branch))
              val done = pull match {
                case Some(RemoteRepository.PullRequest.Merged(_, head)) => head == tip
                case Some(RemoteRepository.PullRequest.Closed(_, head)) => head == tip
                case Some(RemoteRepository.PullRequest.Open(_)) => false
                case None =>
                  at < cutoff && {
                    note(
                      s"[main] deleting $branch, never offered, its last commit $tip " +
                        s"made ${java.time.Instant.ofEpochSecond(at)}, past $Unpublished unpublished"
                    )
                    true
                  }
              }
              if (done) deleted(branch)
              each(rest, asked + 1)
          }
        case _ => ()
      }
      each((asking.drop(from) ++ asking.take(from)).toList, 0)
    }
  }

  /** Where the sweep keeps the branch it asked last, beside the copies' directory. */
  private def cursor: Path = copies.resolveSibling(s"${repository.name}.swept")

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
  private def base(): Either[HostError, String] =
    for {
      _ <- fetchesFromTarget()
      _ <- git(home, "fetch", "--quiet", "--no-tags", "origin")
      known <- defaultBranch()
      default <- known match {
        case Some(d) => Right(d)
        case None =>
          git(home, "remote", "set-head", "origin", "--auto").flatMap(_ =>
            defaultBranch().flatMap(
              _.toRight(HostError.Failed("origin's default branch cannot be read after set-head."))
            )
          )
      }
    } yield default

  /** Whether the clone has an origin that is `target`'s remote ([[declared]]). */
  private def fetchesFromTarget(): Either[HostError, Unit] =
    for {
      origin <- git(home, "remote", "get-url", "origin").left.map(_ =>
        HostError.Failed(s"The clone at $home has no remote origin to cut a run's branch from.")
      )
      _ <- declared(origin.output.text.linesIterator.nextOption().getOrElse("").trim)
    } yield ()

  /** The default branch `refs/remotes/origin/HEAD` names, `origin/{name}`; `None` when it names
    * none. Nothing is asked of origin.
    */
  private def defaultBranch(): Either[HostError, Option[String]] =
    ran(home, "symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD").map { (_, r) =>
      val default = r.output.text.trim
      Option.when(r.exit == 0 && default.startsWith("origin/"))(default)
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

  /** Whether the clone's origin, fetched from at `url`, is `target`'s remote; always, with no
    * `target`. The refusal shows `url` without its user or password.
    */
  private def declared(url: String): Either[HostError, Unit] = target match {
    case Some(t) if !t.remote.fetchedBy(url) =>
      Left(
        HostError.Failed(
          s"The clone at $home fetches from ${Remote.shown(url)}, not from the declared " +
            s"${t.remote.written}, and a run's branch is cut only from the declared repository. " +
            s"Point the clone's origin there (git -C $home remote set-url origin " +
            s"${t.remote.written}), or declare the repository it fetches from."
        )
      )
    case _ => Right(())
  }

  /** As [[ran]], a non-zero exit a `Left` carrying git's output. */
  private def git(in: Path, args: String*): Either[HostError, Ran] =
    ran(in, args*).flatMap { (argv, r) =>
      if (r.exit == 0) Right(r)
      else Left(HostError.Failed(s"`${argv.written}` exited ${r.exit}: ${r.output.text}"))
    }
}

object WorkingCopies {

  /** What came of removing a run's copy. */
  enum Removed derives CanEqual {

    /** It is gone, or there was none. */
    case Gone

    /** It is kept at `at`, as it holds changes never committed. */
    case Uncommitted(at: Path)

    /** It is kept at `at`, as another process holds it. */
    case Held(at: Path)
  }

  /** 20: the most branches one sweep asks the remote repository about. */
  val SweepAsks: Int = 20

  /** 90 days: how long a branch never offered, holding commits origin's default branch lacks,
    * is kept after its last commit.
    */
  val Unpublished: FiniteDuration = FiniteDuration(90, TimeUnit.DAYS)

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
