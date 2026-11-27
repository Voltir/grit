package grit.core.host

/** An absolute path on this machine, in normal form: `/` then segments joined by `/`, with no
  * `.`, no `..` and no empty segment; the root itself is `/`.
  */
opaque type AbsPath = String

object AbsPath {

  /** The file system's root, `/`. */
  val Root: AbsPath = "/"

  /** `path` in normal form, each `..` taking the segment before it (the root's parent is the
    * root). Refused, with why, when it does not start with `/` or holds a NUL character.
    */
  def of(path: String): Either[String, AbsPath] =
    if (path.contains('\u0000')) Left(s"`$path` holds a NUL character")
    else if (!path.startsWith("/")) Left(s"`$path` is not absolute")
    else Right(normal(Vector.empty, path))

  /** `path` read from `from` when it is relative, as `of` reads it when it is absolute. */
  def resolve(from: AbsPath, path: String): AbsPath =
    if (path.startsWith("/")) normal(Vector.empty, path)
    else normal(segments(from), path)

  /** Whether `path` is `dir` or lies under it. */
  def within(path: AbsPath, dir: AbsPath): Boolean =
    dir == Root || path == dir || path.startsWith(dir + "/")

  def value(path: AbsPath): String = path

  private def segments(path: AbsPath): Vector[String] =
    path.split('/').toVector.filter(_.nonEmpty)

  private def normal(start: Vector[String], path: String): AbsPath = {
    val kept = path.split('/').toVector.foldLeft(start) {
      case (so, "" | ".") => so
      case (so, "..") => so.dropRight(1)
      case (so, s) => so :+ s
    }
    "/" + kept.mkString("/")
  }
}

/** Where a path a command is given leads, from the checkout's root. */
enum Reached extends caps.Pure {
  case Inside

  /** `real`: each link on the way followed. */
  case Outside(real: AbsPath)
}

/** Where the paths a command is given lead on this machine, from its checkout's root. */
trait Reach extends caps.SharedCapability {

  /** Where `path` leads, read from the root when relative: each link on the way followed, and
    * each `..` taking the directory it was reached in, as far as the path exists; past that,
    * read as written. Never fails: a part that cannot be read is read as written.
    */
  def reached(path: String): Reached
}
