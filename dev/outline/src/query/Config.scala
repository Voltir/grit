package grit.outline.query

import grit.outline.locate.Root

/** The query settings a repository gives in its own `.outline.conf`: source paths no query shows unless it names them fully. */
final case class Config(hidden: Vector[String])

object Config {

  /** No settings: nothing is hidden. */
  val empty: Config = Config(Vector.empty)

  private val Hide = raw"hide\s+(.+)".r

  /** `root`'s `.outline.conf`, or `empty` when it has none: lines `hide <prefix>` (a repo-relative source path prefix); `#` comments and blank lines are skipped. `Left` names the line that is neither. */
  def read(root: Root): Either[String, Config] = {
    val file = root.dir / ".outline.conf"
    if (!os.isFile(file)) Right(empty)
    else
      os.read
        .lines(file)
        .zipWithIndex
        .map { case (line, index) => (line.trim, index + 1) }
        .filter { case (line, _) => line.nonEmpty && !line.startsWith("#") }
        .foldLeft[Either[String, Vector[String]]](Right(Vector.empty)) {
          case (Left(message), _) => Left(message)
          case (Right(prefixes), (line, number)) =>
            line match {
              case Hide(prefix) => Right(prefixes :+ prefix.trim)
              case _ => Left(s"$file: line $number is not `hide <prefix>`: $line")
            }
        }
        .map(Config(_))
  }
}
