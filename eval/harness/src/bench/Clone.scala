package grit.eval.harness.bench

import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import grit.core.host.{Argv, Processes, RelPath}

/** A task's checkout: a repository of its own holding only a base commit's history, so that no
  * later commit can be reached from it.
  */
object Clone {

  /** 10 minutes: the longest a git command runs. */
  val Longest: FiniteDuration = 10.minutes

  /** A new repository at `dir`, holding `base` of the repository at `repository` (an absolute
    * path) and its history alone, checked out on the branch `bench`, with no remote. `Left`, with
    * git's last words: `dir` exists, or a git command fails or runs past [[Longest]].
    */
  def fresh(work: Processes)(repository: String, base: Sha, dir: RelPath): Either[String, Unit] =
    for {
      _ <- ran(work, RelPath.Root, "mkdir", RelPath.value(dir))
      _ <- git(work, dir, "init", "-q", "-b", "bench")
      _ <- git(work, dir, "fetch", "-q", "--no-tags", repository, Sha.value(base))
      _ <- git(work, dir, "checkout", "-q", "-B", "bench", "FETCH_HEAD")
    } yield ()

  /** As [[fresh]] of `task`'s base, with `task`'s commits then applied in order; `Left` also when
    * one does not apply.
    */
  def picked(
      work: Processes
  )(repository: String, task: BenchTask, dir: RelPath): Either[String, Unit] = {
    val commits = task.commits.map(Sha.value)
    for {
      _ <- fresh(work)(repository, task.base, dir)
      _ <- git(work, dir, (Seq("fetch", "-q", "--no-tags", repository) ++ commits)*)
      _ <- git(work, dir, (Seq("cherry-pick") ++ commits)*)
    } yield ()
  }

  /** The source lines `dir`'s files change from `base`, files never added included (they are
    * marked to be added, as `git add -N` does). `Left`, with git's last words, as for [[fresh]].
    */
  def changed(work: Processes)(dir: RelPath, base: Sha): Either[String, DiffLines] =
    for {
      _ <- git(work, dir, "add", "-N", ".")
      read <- diff(work)(dir, base, Seq("-U0"), Long.MaxValue)
    } yield DiffLines.of(read.text)

  /** What a coder left at `dir`, as lines to keep once the checkout is gone: `[rig] the coder's
    * commits:` and its commits since `base`, a line each, as `git log --oneline` writes them;
    * then `[rig] its diff against the base:` and its diff from `base`, files never added
    * included, cut at a line to at most [[SavedDiff]] bytes, with `[rig] the diff was cut at
    * <SavedDiff> of <bytes> bytes` after when it was. `Left`, with git's last words, as for
    * [[fresh]].
    */
  def left(work: Processes)(dir: RelPath, base: Sha): Either[String, Vector[String]] =
    left(work)(dir, base, SavedDiff)

  /** 200 KB: the most of a coder's diff [[left]] keeps. */
  val SavedDiff: Int = 200 * 1024

  /** As the public `left`, the diff cut at `most` bytes. */
  private[bench] def left(work: Processes)(
      dir: RelPath,
      base: Sha,
      most: Int
  ): Either[String, Vector[String]] =
    for {
      commits <- git(work, dir, "log", "--oneline", "--no-decorate", s"${Sha.value(base)}..HEAD")
      _ <- git(work, dir, "add", "-N", ".")
      read <- diff(work)(dir, base, Seq.empty, most.toLong)
    } yield ("[rig] the coder's commits:" +: commits.linesIterator.filter(_.nonEmpty).toVector) ++
      ("[rig] its diff against the base:" +: read.text.linesIterator.toVector) ++
      Option
        .when(read.bytes > most)(s"[rig] the diff was cut at $most of ${read.bytes} bytes")
        .toVector

  /** `git diff` from `base` in `dir` with `options`, read whole from its output, not from run's
    * tail, which keeps only the last lines: its first lines within `most` bytes of UTF-8, newlines
    * counted, and how many bytes it wrote in all.
    */
  private def diff(work: Processes)(
      dir: RelPath,
      base: Sha,
      options: Seq[String],
      most: Long
  ): Either[String, (text: String, bytes: Long)] = {
    val args = Seq("diff", "--no-color", "--no-ext-diff", "--no-renames") ++ options :+
      Sha.value(base)
    for {
      argv <- Argv.of("git", args*)
      read <- work
        .started(argv, dir) { s =>
          val kept = new StringBuilder
          var bytes = 0L
          s.lines.foreach { line =>
            val size = line.getBytes(StandardCharsets.UTF_8).length + 1L
            if (bytes + size <= most) kept ++= line ++= "\n"
            bytes += size
          }
          s.await(Longest).map(r => (r.exit, kept.result(), bytes, r.output.text))
        }
        .left
        .map(e => s"`git diff` ${e.message}")
      text <- read.left.map(e => s"`git diff` ${e.message}").flatMap {
        (exit, text, bytes, errors) =>
          if (exit == 0) Right((text = text, bytes = bytes))
          else Left(s"`git diff` failed: ${errors.trim}")
      }
    } yield text
  }

  /** `git` with `args` in `dir`, as a picking committer who signs nothing: its output, or why
    * not.
    */
  private def git(work: Processes, dir: RelPath, args: String*): Either[String, String] =
    ran(
      work,
      dir,
      "git",
      (Seq("-c", "user.name=grit bench", "-c", "user.email=bench@grit.invalid") ++
        Seq("-c", "commit.gpgsign=false") ++ args)*
    ).left.map(why => why.replaceFirst("^`git`", s"`git ${args.headOption.getOrElse("")}`"))

  /** `program` with `args` run in `dir` to its end: its output when it exits 0, or why not. */
  private[bench] def ran(
      work: Processes,
      dir: RelPath,
      program: String,
      args: String*
  ): Either[String, String] =
    Argv
      .of(program, args*)
      .flatMap(work.run(_, dir, Longest).left.map(e => s"`$program` ${e.message}"))
      .flatMap { r =>
        if (r.exit == 0) Right(r.output.text)
        else Left(s"`$program` failed: ${r.output.text.trim}")
      }
}
