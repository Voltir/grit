package grit.host

import java.nio.charset.{CharacterCodingException, StandardCharsets}
import java.nio.file.{Files, LinkOption, Path}

import scala.util.control.NonFatal

import grit.core.host.{HostError, RelPath, Workspace}

/** A checkout's root on this machine, and how a [[RelPath]] is found under it. Every method
  * turns what `java.nio.file` throws into a [[HostError]].
  */
private[host] final class Checkout(root: Path) {
  import Checkout.attempt

  /** The root's real path; `Failed` when it does not exist. */
  def real: Either[HostError, Path] = attempt(root.toRealPath())

  /** Where `path` is, its links followed: `Outside` when that leaves the checkout or names
    * a secrets file. A path that does not exist yet resolves through its nearest existing
    * ancestor, which must be inside.
    */
  def find(path: RelPath): Either[HostError, Path] =
    real.flatMap { top =>
      val target = top.resolve(RelPath.value(path)).normalize()
      attempt {
        if (Files.exists(target)) Some(target.toRealPath())
        else None
      }.flatMap {
        case Some(found) => inside(top, found, path).map(_ => found)
        case None =>
          ancestor(target).flatMap(a => inside(top, a, path)).map(_ => target)
      }
    }

  /** The text of the existing file at `at`, which is `path`: `NotFound`, `NotAFile`,
    * `TooLarge` or `NotText` when it cannot be read as UTF-8.
    */
  def text(path: RelPath, at: Path): Either[HostError, String] =
    if (!Files.exists(at)) Left(HostError.NotFound(path))
    else if (Files.isDirectory(at)) Left(HostError.NotAFile(path))
    else
      attempt(Files.size(at)).flatMap { size =>
        if (size > Workspace.MaxFileBytes)
          Left(HostError.TooLarge(path, size, Workspace.MaxFileBytes))
        else
          try Right(Files.readString(at, StandardCharsets.UTF_8))
          catch {
            case _: CharacterCodingException => Left(HostError.NotText(path))
            case NonFatal(e) => Left(Checkout.failed(e))
          }
      }

  /** `path` relative to the root, `/`-separated. */
  def relative(top: Path, at: Path): String =
    top.relativize(at).toString.replace(java.io.File.separatorChar, '/')

  private def inside(top: Path, found: Path, path: RelPath): Either[HostError, Unit] =
    if (found.startsWith(top) && RelPath.of(relative(top, found)).isRight) Right(())
    else Left(HostError.Outside(path))

  /** The real path of `p`'s nearest ancestor that exists. */
  private def ancestor(p: Path): Either[HostError, Path] =
    Option(p.getParent) match {
      case None => Left(HostError.Failed(s"$p has no existing parent"))
      case Some(parent) =>
        if (Files.exists(parent, LinkOption.NOFOLLOW_LINKS) || Files.exists(parent))
          attempt(parent.toRealPath())
        else ancestor(parent)
    }
}

private[host] object Checkout {

  /** `body`'s value, or what it threw as [[failed]]. */
  def attempt[A](body: => A): Either[HostError, A] =
    try Right(body)
    catch {
      case NonFatal(e) => Left(failed(e))
    }

  /** `e` as [[HostError.Failed]]: its class and message. */
  def failed(e: Throwable): HostError = {
    val said = Option(e.getMessage).fold("")(m => s": $m")
    HostError.Failed(s"${e.getClass.getSimpleName}$said")
  }
}
