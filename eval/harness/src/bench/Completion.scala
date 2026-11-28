package grit.eval.harness.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.host.{Processes, RelPath}

/** What a finished trial must leave beyond green checks: at least `commits` commits since the
  * base; a clean tree (`out/` aside); every commit before HEAD compiling, as [[Rig.GritCompile]]
  * compiles it (HEAD's compile is the checks'); a file that each of `exists` matches; no file any
  * of `absent` matches; and every file each of `holds`' paths matches, one at least, holding its
  * text. A path is a glob over the tree's files, as `java.nio`'s `glob:` reads one (`*` within
  * a segment, `**` across them: `**.orig` matches a file at the root, `**` then `/` never does).
  */
final case class Completion(
    commits: Int,
    exists: Vector[String],
    absent: Vector[String],
    holds: Vector[Holding]
) extends caps.Pure

/** Text the files `path` matches must hold, as a [[Completion]] reads them. */
final case class Holding(path: String, text: String) extends caps.Pure

/** Each item of a [[Completion]] a checkout missed, as a line the report prints, and each it could
  * not read: the commit-level ones, as `<item>: not kept`, when only a diff is held (a regrade).
  * It meets its completion when `missed` is empty.
  */
final case class Completed(missed: Vector[String], unread: Vector[String]) extends caps.Pure

object Completed {

  /** A task's that names no completion: nothing missed, nothing unread. */
  val Nothing: Completed = Completed(Vector.empty, Vector.empty)
}

object Completion {

  /** `completion`'s items read in the checkout at `dir` against `base`, its commits before HEAD
    * compiled in turn in a worktree of their own beside it, `<dir>-commits`, removed after, its
    * daemon shut down. `watch` is asked as [[Checks.run]] asks it: a reason it gives stops the
    * compile and is a `Left`, as is a git command or a file read that fails.
    */
  def of(work: Processes)(
      dir: RelPath,
      base: Sha,
      completion: Completion,
      watch: () => Option[String]
  ): Either[String, Completed] =
    for {
      count <- git(work, dir, "rev-list", "--count", s"${Sha.value(base)}..HEAD").map(_.trim)
      made <- count.toIntOption.toRight(s"`git rev-list --count` said `$count`")
      status <- git(work, dir, "status", "--porcelain", "--untracked-files=all")
      changed = status.linesIterator
        .map(_.drop(3))
        .filter(p => p.nonEmpty && !p.startsWith("out/"))
        .toVector
      listed <- git(work, dir, "rev-list", "--reverse", s"${Sha.value(base)}..HEAD")
      before = listed.linesIterator.filter(_.nonEmpty).toVector.dropRight(1)
      broken <- uncompiled(work)(dir, before, watch)
      files <- ofFiles(work)(dir, completion)
    } yield Completed(
      Option
        .when(made < completion.commits)(
          s"commits: $made since the base, fewer than ${completion.commits}"
        )
        .toVector ++
        Option
          .when(changed.nonEmpty)(
            s"tree: ${changed.size} paths changed since the last commit, as " +
              changed.take(5).mkString(", ")
          )
          .toVector ++
        broken.map(c => s"compiles: commit $c does not compile") ++ files.missed,
      Vector.empty
    )

  /** The commit-level items unread, as only a diff applied at `dir` is held (a regrade). */
  private val NotKept: Vector[String] =
    Vector("commits: not kept", "tree: not kept", "compiles: not kept")

  /** `completion`'s path items read in the checkout at `dir`, and its commit-level ones unread, as
    * when only a diff applied there is held (a regrade). `Left` when a file cannot be read.
    */
  def ofDiff(work: Processes)(dir: RelPath, completion: Completion): Either[String, Completed] =
    ofFiles(work)(dir, completion).map(_.copy(unread = NotKept))

  /** The path items alone: `exists`, `absent` and `holds`, over every file under `dir` but those
    * under `.git` and `out`.
    */
  private def ofFiles(work: Processes)(
      dir: RelPath,
      completion: Completion
  ): Either[String, Completed] =
    for {
      root <- Clone.ran(work, dir, "pwd", "-P").map(p => Path.of(p.trim))
      files <- Try {
        val walk = Files.walk(root)
        try
          walk.iterator.asScala
            .filter(Files.isRegularFile(_))
            .map(p => root.relativize(p).toString)
            .filterNot(p => p.startsWith(".git/") || p.startsWith("out/"))
            .toVector
            .sorted
        finally walk.close()
      }.toEither.left.map(e => s"the checkout's files could not be listed: ${e.getMessage}")
      matching = (glob: String) => {
        val m = FileSystems.getDefault.getPathMatcher(s"glob:$glob")
        files.filter(f => m.matches(Path.of(f)))
      }
      held <- completion.holds.foldLeft[Either[String, Vector[String]]](Right(Vector.empty)) {
        (so, h) =>
          so.flatMap { missed =>
            val found = matching(h.path)
            if (found.isEmpty) Right(missed :+ s"holds: no file matches `${h.path}`")
            else
              found.foldLeft[Either[String, Vector[String]]](Right(missed)) { (m, f) =>
                m.flatMap { acc =>
                  Try(Files.readString(root.resolve(f), StandardCharsets.UTF_8)).toEither.left
                    .map(e => s"`$f` could not be read: ${e.getMessage}")
                    .map(text =>
                      if (text.contains(h.text)) acc
                      else acc :+ s"holds: `$f` does not hold `${h.text}`"
                    )
                }
              }
          }
      }
    } yield Completed(
      completion.exists
        .filter(g => matching(g).isEmpty)
        .map(g => s"exists: no file matches `$g`") ++
        completion.absent.flatMap(g => matching(g).map(f => s"absent: `$f` matches `$g`")) ++ held,
      Vector.empty
    )

  /** Each of `commits` that does not compile, as `<short sha> <subject>`, compiled in turn, oldest
    * first, in one worktree of the checkout at `dir`, so each after the first compiles only what
    * changed.
    */
  private def uncompiled(work: Processes)(
      dir: RelPath,
      commits: Vector[String],
      watch: () => Option[String]
  ): Either[String, Vector[String]] =
    if (commits.isEmpty) Right(Vector.empty)
    else
      for {
        tree <- RelPath.of(s"${RelPath.value(dir)}-commits").left.map(_.toString)
        root <- Clone.ran(work, RelPath.Root, "pwd", "-P").map(_.trim)
        broken <- {
          val made = git(
            work,
            dir,
            "worktree",
            "add",
            "-q",
            "--detach",
            s"$root/${RelPath
                .value(tree)}",
            commits.headOption.getOrElse("")
          )
          try
            made.flatMap(_ =>
              commits.foldLeft[Either[String, Vector[String]]](Right(Vector.empty)) { (so, c) =>
                for {
                  broken <- so
                  _ <- git(work, tree, "checkout", "-q", "--detach", c)
                  named <- git(work, tree, "log", "-1", "--format=%h %s").map(_.trim)
                  ok <- compiles(work)(tree, watch)
                } yield if (ok) broken else broken :+ named
              }
            )
          finally {
            if (Clone.ran(work, tree, "test", "-d", "out/mill-daemon").isRight) {
              val _ = Clone.ran(work, tree, "./mill", "shutdown")
            }
            val _ = git(work, dir, "worktree", "remove", "--force", s"$root/${RelPath.value(tree)}")
            val _ = Clone.ran(work, RelPath.Root, "rm", "-rf", RelPath.value(tree))
            val _ = git(work, dir, "worktree", "prune")
          }
        }
      } yield broken

  /** Whether the tree at `dir` compiles by [[Rig.GritCompile]], its JVMs capped
    * ([[Rig.CappedJvm]]), within [[Rig.Warming]]; `Left` when it cannot start or `watch` stops it.
    */
  private def compiles(
      work: Processes
  )(dir: RelPath, watch: () => Option[String]): Either[String, Boolean] = {
    val (k, v) = Rig.CappedJvm
    for {
      argv <- Watched.limited(Rig.Warming, (Vector("env", s"$k=$v") ++ Rig.GritCompile)*)
      read <- work
        .started(argv, dir)(
          Watched.ran(_, Rig.Warming + 1.minute, Rig.Warming + 2.minutes, _ => None, watch)
        )
        .left
        .map(e => s"a commit's compile did not run: ${e.message}")
      _ <- read.stopped.map(why => s"a commit's compile was stopped: $why").toLeft(())
    } yield read.ended.exists(_.exit == 0)
  }

  private def git(work: Processes, dir: RelPath, args: String*): Either[String, String] =
    Clone.ran(work, dir, "git", args*)
}
