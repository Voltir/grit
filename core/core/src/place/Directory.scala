package grit.core.place

/** An absolute, normalized directory path: it starts with `/`, and holds no `.`, `..` or
  * empty segment, nor a trailing `/` (but `/` itself). It is not checked to exist; a caller
  * that wants one place per directory resolves links first.
  */
opaque type Directory = String

object Directory {

  /** `path` as a directory, or why it is not one. */
  def of(path: String): Either[String, Directory] =
    if (!path.startsWith("/")) Left(s"not an absolute path: $path")
    else if (path == "/") Right(path)
    else {
      val segments = path.drop(1).split("/", -1).toVector
      segments.find(s => s.isEmpty || s == "." || s == "..") match {
        case Some(bad) =>
          Left(s"not a normalized path (${if (bad.isEmpty) "an empty segment" else bad}): $path")
        case None => Right(path)
      }
    }

  def value(d: Directory): String = d
}
