package grit.eval.harness.bench

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
  def fresh(work: Processes)(repository: String, base: Commit, dir: RelPath): Either[String, Unit] =
    for {
      _ <- ran(work, RelPath.Root, "mkdir", RelPath.value(dir))
      _ <- git(work, dir, "init", "-q", "-b", "bench")
      _ <- git(work, dir, "fetch", "-q", "--no-tags", repository, Commit.value(base))
      _ <- git(work, dir, "checkout", "-q", "-B", "bench", "FETCH_HEAD")
    } yield ()

  /** As [[fresh]] of `task`'s base, with `task`'s commits then applied in order; `Left` also when
    * one does not apply.
    */
  def picked(
      work: Processes
  )(repository: String, task: Task, dir: RelPath): Either[String, Unit] = {
    val commits = task.commits.map(Commit.value)
    for {
      _ <- fresh(work)(repository, task.base, dir)
      _ <- git(work, dir, (Seq("fetch", "-q", "--no-tags", repository) ++ commits)*)
      _ <- git(work, dir, (Seq("cherry-pick") ++ commits)*)
    } yield ()
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
  private def ran(
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
