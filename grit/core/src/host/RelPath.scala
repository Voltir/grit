package grit.core.host

/** A path inside the checkout, relative to its root, in normal form: segments joined by
  * `/`, with no `.`, no `..` and no empty segment; the root itself is `.`. It names no
  * secrets file: no segment is `.env` or starts with `.env.`.
  */
opaque type RelPath = String

object RelPath {

  /** The checkout's root. */
  val Root: RelPath = "."

  /** `path` in normal form. Refused when it is absolute (starts with `/` or `~`), when a `..`
    * climbs above the root, when a segment names a secrets file, or when it holds a NUL
    * character. `""` and `.` are the root.
    */
  def of(path: String): Either[PathError, RelPath] =
    if (path.contains('\u0000')) Left(PathError.Malformed(path))
    else if (path.startsWith("/") || path.startsWith("~")) Left(PathError.Absolute(path))
    else {
      val kept = path
        .split('/')
        .toVector
        .filter(s => s.nonEmpty && s != ".")
        .foldLeft[Option[Vector[String]]](Some(Vector.empty)) {
          case (None, _) => None
          case (Some(so), "..") => Option.when(so.nonEmpty)(so.dropRight(1))
          case (Some(so), s) => Some(so :+ s)
        }
      kept match {
        case None => Left(PathError.Escapes(path))
        case Some(segments) if segments.exists(secret) => Left(PathError.Secret(path))
        case Some(segments) => Right(if (segments.isEmpty) Root else segments.mkString("/"))
      }
    }

  /** Whether the file name `name` is a secrets file: `.env`, or `.env.` and anything. */
  def secret(name: String): Boolean = name == ".env" || name.startsWith(".env.")

  def value(path: RelPath): String = path
}

/** Why a path cannot be used. `message` is what the model reads. */
enum PathError {
  case Absolute(path: String)
  case Escapes(path: String)
  case Secret(path: String)
  case Malformed(path: String)

  /** One sentence: what is wrong with the path, and what is allowed. */
  def message: String = this match {
    case Absolute(p) =>
      s"`$p` is absolute; give a path relative to the checkout's root, such as `src/Main.scala`."
    case Escapes(p) => s"`$p` leads outside the checkout; paths must stay inside it."
    case Secret(p) => s"`$p` names a secrets file (.env or .env.*), which tools may not touch."
    case Malformed(p) => s"`${p.replace("\u0000", "\\0")}` holds a NUL character."
  }
}
