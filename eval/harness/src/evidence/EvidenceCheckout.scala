package grit.eval.harness.evidence

import java.nio.charset.StandardCharsets

import grit.core.host.{Argv, Processes, RelPath}
import grit.eval.harness.bench.Clone

/** An evidence checkout: a directory holding a clone of a task's base and, beside it, the files a
  * proposer is shown.
  */
object EvidenceCheckout {

  /** The subdirectory holding the clone of the task's base. */
  val Base: String = "base"

  /** The subdirectory holding the shown files. */
  val Evidence: String = "evidence"

  /** A new directory `dir` holding [[Base]], a clone of `task`'s base from the repository at
    * `repository` (an absolute path; [[Clone.fresh]]), and [[Evidence]], each of `shown`'s files
    * at its path under it, as UTF-8. `Left`, writing nothing, when a file's path is the root, is
    * given twice, or lies under another file's path; else `Left`, with what was written left,
    * when `dir` exists, or the clone or a write fails or runs past [[Clone.Longest]].
    */
  def write(
      work: Processes
  )(repository: String, task: BlindTask, shown: Shown, dir: RelPath): Either[String, Unit] =
    for {
      _ <- placed(shown.files.map(_._1))
      base <- under(dir, Base)
      evidence <- under(dir, Evidence)
      _ <- Clone.ran(work, RelPath.Root, "mkdir", RelPath.value(dir))
      _ <- Clone.fresh(work)(repository, task.base, base)
      _ <- Clone.ran(work, RelPath.Root, "mkdir", RelPath.value(evidence))
      _ <- each(shown.files)((path, text) => written(work, evidence, path, text))
    } yield ()

  /** `paths` when each names a file of its own: not the root, not given twice, and none under
    * another; else why not, for the first that fails.
    */
  private def placed(paths: Vector[RelPath]): Either[String, Unit] = {
    val named = paths.map(RelPath.value)
    each(named.zipWithIndex) { (p, i) =>
      if (p == RelPath.value(RelPath.Root)) Left(s"an evidence file's path is the root: $p")
      else if (named.indexOf(p) < i) Left(s"an evidence file's path is given twice: $p")
      else
        named
          .find(other => p.startsWith(s"$other/"))
          .fold(Right(()))(other =>
            Left(s"an evidence file's path lies under another's: $p under $other")
          )
    }
  }

  /** `dir`'s subdirectory `name`. */
  private def under(dir: RelPath, name: String): Either[String, RelPath] =
    RelPath.of(s"${RelPath.value(dir)}/$name").left.map(e => s"no directory $name: $e")

  /** `text` written at `path` under `dir`, its directories made. */
  private def written(
      work: Processes,
      dir: RelPath,
      path: RelPath,
      text: String
  ): Either[String, Unit] =
    for {
      argv <- Argv.of(
        "sh",
        "-c",
        "mkdir -p -- \"$(dirname -- \"$1\")\" && cat > \"$1\"",
        "sh",
        RelPath.value(path)
      )
      ran <- work
        .started(argv, dir) { s =>
          s.input.write(text.getBytes(StandardCharsets.UTF_8))
          s.input.close()
          s.await(Clone.Longest)
        }
        .flatten
        .left
        .map(e => s"${RelPath.value(path)} was not written: ${e.message}")
      _ <- Either.cond(
        ran.exit == 0,
        (),
        s"${RelPath.value(path)} was not written: ${ran.output.text.trim}"
      )
    } yield ()

  private def each[A](as: Vector[A])(f: A => Either[String, Unit]): Either[String, Unit] =
    as.foldLeft[Either[String, Unit]](Right(()))((done, a) => done.flatMap(_ => f(a)))
}
