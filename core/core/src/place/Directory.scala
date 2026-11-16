package grit.core.place

/** An absolute, normalized directory path: it starts with `/`, and holds no `.`, `..` or
  * empty segment, nor a trailing `/` (but `/` itself). One [[Directory.of]] made holds no
  * control character; one read back from a stored record ([[Directory.stored]]) may. It is not
  * checked to exist; a caller that wants one place per directory resolves links first.
  */
opaque type Directory = String

object Directory {

  /** `path` as a directory, or why it is not one: not absolute, not normalized, or holding a
    * control character (`Character.isISOControl`: U+0000 to U+001F, U+007F to U+009F), named by
    * its code point, with `path` shown each such character as `?`.
    */
  def of(path: String): Either[String, Directory] =
    stored(path).flatMap { d =>
      path.find(Character.isISOControl) match {
        case Some(c) =>
          Left(f"holds a control character (U+${c.toInt}%04X): ${path.map(shown)}")
        case None => Right(d)
      }
    }

  /** `path` as a directory read back from what grit stored, or why it is not one: as [[of]],
    * but a control character, which a stored record may hold, is kept. Never for a path sent in.
    */
  private[grit] def stored(path: String): Either[String, Directory] =
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

  private def shown(c: Char): Char = if (Character.isISOControl(c)) '?' else c

  def value(d: Directory): String = d
}
