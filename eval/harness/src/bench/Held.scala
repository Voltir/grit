package grit.eval.harness.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.util.Try

import grit.core.host.{Processes, RelPath}

/** A held file: its path in a checkout, whose last segment starts with `Held` so that it never
  * overwrites a coder's, and its text.
  */
final case class HeldFile private (path: RelPath, text: String) extends caps.Pure

object HeldFile {

  /** `text` at `path`, or why not: its last segment does not start with `Held`. */
  def of(path: RelPath, text: String): Either[String, HeldFile] = {
    val name = RelPath.value(path).split('/').lastOption.getOrElse("")
    Either.cond(
      name.startsWith("Held"),
      HeldFile(path, text),
      s"held file `${RelPath.value(path)}` is not named `Held…`"
    )
  }
}

/** A task's held tests: files kept out of the coder's checkout, at `held/<repo path>` under the
  * task's directory, copied into it once the coder has stopped and its diff is read, and the
  * checks that run them (the law not among them).
  */
final case class Held(files: Vector[HeldFile], checks: Vector[Check]) extends caps.Pure

object Held {

  /** `run` with `held`'s files written into the checkout at `dir`, over any file there at their
    * paths, and removed after, whatever `run` gives, so that what the coder left is read without
    * them. `Left` when one cannot be written.
    */
  def within[A](work: Processes)(dir: RelPath, held: Held)(
      run: => Either[String, A]
  ): Either[String, A] =
    Clone.ran(work, dir, "pwd", "-P").map(p => Path.of(p.trim)).flatMap { root =>
      val paths = held.files.map(f => root.resolve(RelPath.value(f.path)))
      try
        held.files
          .zip(paths)
          .foldLeft[Either[String, Unit]](Right(())) { (so, written) =>
            val (f, at) = written
            so.flatMap(_ =>
              Try {
                val _ = Files.createDirectories(at.getParent)
                val _ = Files.writeString(at, f.text, StandardCharsets.UTF_8)
              }.toEither.left
                .map(e => s"held file `${RelPath.value(f.path)}` was not written: ${e.getMessage}")
            )
          }
          .flatMap(_ => run)
      finally paths.foreach(p => Try(Files.deleteIfExists(p)))
    }
}
