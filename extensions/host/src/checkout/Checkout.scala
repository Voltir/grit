package grit.host.checkout

import java.nio.charset.{CharacterCodingException, StandardCharsets}
import java.nio.file.{Files, LinkOption, Path}

import scala.util.control.NonFatal

import grit.core.host.{HostError, RelPath, Workspace}
import grit.core.secret.Named
import grit.std.near.Near

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
    if (!Files.exists(at)) Left(HostError.NotFound(path, nearest(path)))
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

  /** The path that exists most like `path`, which does not: each of its parts that does not
    * exist taken as its nearest sibling's name ([[Near.nearest]]); failing that, of the entries
    * named as its last part, the one sharing the most parts at its end with it, then the
    * shortest. Never a secrets file, nor anything inside a directory [[Workspace.skipped]]
    * names; `None` when nothing is that near.
    */
  def nearest(path: RelPath): Option[RelPath] =
    real.toOption.flatMap { top =>
      val parts = RelPath.value(path).split('/').toList.filter(p => p.nonEmpty && p != ".")
      def walk(at: Path, rest: List[String], taken: Vector[String]): Option[Vector[String]] =
        rest match {
          case Nil => Some(taken)
          case part :: more =>
            if (Files.exists(at.resolve(part), LinkOption.NOFOLLOW_LINKS))
              walk(at.resolve(part), more, taken :+ part)
            else
              names(at)
                .flatMap(ns => Near.nearest(part, ns, 1).headOption)
                .flatMap(n => walk(at.resolve(n), more, taken :+ n))
        }
      def elsewhere: Option[Vector[String]] =
        parts.lastOption.flatMap { last =>
          val wanted = parts.reverse
          entries(top, Int.MaxValue).toOption
            .getOrElse(Vector.empty)
            .map((p, _) => relative(top, p).split('/').toVector)
            .filter(_.lastOption.contains(last))
            .sortBy(found =>
              (-found.reverse.zip(wanted).takeWhile(_ == _).size, found.size, found.mkString("/"))
            )
            .headOption
        }
      walk(top, parts, Vector.empty)
        .orElse(elsewhere)
        .map(_.mkString("/"))
        .filter(n => n != RelPath.value(path) && !n.split('/').exists(Named.file))
        .flatMap(n => RelPath.of(n).toOption)
        .filter(n => find(n).exists(Files.exists(_)))
    }

  /** The names in directory `at` but secrets files', `None` when it is not a directory. */
  private def names(at: Path): Option[Vector[String]] =
    if (!Files.isDirectory(at, LinkOption.NOFOLLOW_LINKS)) None
    else
      attempt(Checkout.children(at)).toOption.map(
        _.flatMap(p => Option(p.getFileName)).map(_.toString).filterNot(Named.file)
      )

  /** Every entry under `dir`, `depth` levels down, with whether it is a directory; a
    * directory [[Workspace.skipped]] is listed but not entered. Links are not followed.
    */
  def entries(dir: Path, depth: Int): Either[HostError, Vector[(Path, Boolean)]] =
    attempt(Checkout.children(dir)).flatMap { kids =>
      kids.foldLeft[Either[HostError, Vector[(Path, Boolean)]]](Right(Vector.empty)) { (acc, kid) =>
        acc.flatMap { so =>
          val isDir = Files.isDirectory(kid, LinkOption.NOFOLLOW_LINKS)
          val name = Option(kid.getFileName).fold("")(_.toString)
          if (isDir && depth > 1 && !Workspace.skipped(name))
            entries(kid, depth - 1).map(below => (so :+ (kid, isDir)) ++ below)
          else Right(so :+ (kid, isDir))
        }
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

  /** The entries of directory `dir`. */
  def children(dir: Path): Vector[Path] = {
    val stream = Files.newDirectoryStream(dir)
    try {
      val out = Vector.newBuilder[Path]
      stream.forEach(p => out += p)
      out.result()
    } finally stream.close()
  }

  /** `e` as [[HostError.Failed]]: its class and message. */
  def failed(e: Throwable): HostError = {
    val said = Option(e.getMessage).fold("")(m => s": $m")
    HostError.Failed(s"${e.getClass.getSimpleName}$said")
  }
}
