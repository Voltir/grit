package grit.host

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import grit.core.host.{EditError, Edited, Edits, HostError, RelPath, Replace}

/** Changing files in the checkout whose root is `root`. A change is written to a temporary
  * file beside the target, which is then renamed over it; an existing file keeps its
  * permissions, and a link keeps pointing at the file it names.
  */
final class LocalEdits(root: Path) extends Edits {
  import Checkout.attempt

  private val checkout = new Checkout(root)

  def write(path: RelPath, text: String): Either[HostError, Unit] =
    checkout.find(path).flatMap { at =>
      if (Files.isDirectory(at)) Left(HostError.NotAFile(path))
      else replace(at, text)
    }

  def edit(path: RelPath, edits: Seq[Replace]): Either[EditError, Edited] =
    (for {
      at <- checkout.find(path)
      text <- checkout.text(path, at)
    } yield (at, text)).left.map(EditError.Host(_)).flatMap { (at, text) =>
      Replace.onto(path, text, edits).flatMap { (changed, edited) =>
        replace(at, changed).left.map(EditError.Host(_)).map(_ => edited)
      }
    }

  /** Makes `text` the whole of the file at `at`, atomically, creating missing parents. */
  private def replace(at: Path, text: String): Either[HostError, Unit] =
    Option(at.getParent) match {
      case None => Left(HostError.Failed(s"$at has no parent directory"))
      case Some(parent) =>
        attempt(Files.createDirectories(parent)).flatMap { _ =>
          attempt(Files.createTempFile(parent, ".grit-", ".tmp")).flatMap { temp =>
            val moved = attempt {
              Files.write(temp, text.getBytes(StandardCharsets.UTF_8))
              if (Files.exists(at)) keepPermissions(at, temp)
              Files.move(
                temp,
                at,
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING
              )
              ()
            }
            if (moved.isLeft) attempt(Files.deleteIfExists(temp))
            moved
          }
        }
    }

  /** Gives `temp` the permissions of `existing`, where the file system has POSIX ones. */
  private def keepPermissions(existing: Path, temp: Path): Unit =
    try Files.setPosixFilePermissions(temp, Files.getPosixFilePermissions(existing))
    catch { case _: UnsupportedOperationException => () }
}
