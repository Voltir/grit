package grit.outline.query

import grit.outline.locate.Root

/** A named set of symbols and families, read from an `area` line: `symbols` are names as `show` takes them, `families` the traits (`family:<Trait>` on the line) whose implementations and contracts it holds. */
final case class Area(name: String, symbols: Vector[String], families: Vector[String])

/** The query settings a repository gives in its own `.outline.conf`: source paths no query shows unless it names them fully, and the named areas `area` answers for. */
final case class Config(hidden: Vector[String], areas: Vector[Area])

object Config {

  /** No settings: nothing is hidden and no area is declared. */
  val empty: Config = Config(Vector.empty, Vector.empty)

  private val Hide = raw"hide\s+(.+)".r
  private val AreaLine = raw"area\s+([^:\s]+):(.*)".r

  /** `root`'s `.outline.conf`, or `empty` when it has none: lines `hide <prefix>` (a repo-relative source path prefix) and `area <name>: <item> …` (an item a symbol name, or `family:<Trait>`); `#` comments and blank lines are skipped. `Left` names the line that is neither, an area with no items, and an area whose name a line before already declared. */
  def read(root: Root): Either[String, Config] = {
    val file = root.dir / ".outline.conf"
    if (!os.isFile(file)) Right(empty)
    else
      os.read
        .lines(file)
        .zipWithIndex
        .map { case (line, index) => (line.trim, index + 1) }
        .filter { case (line, _) => line.nonEmpty && !line.startsWith("#") }
        .foldLeft[Either[String, (Vector[String], Vector[Area])]](
          Right((Vector.empty, Vector.empty))
        ) {
          case (Left(message), _) => Left(message)
          case (Right((prefixes, areas)), (line, number)) =>
            line match {
              case Hide(prefix) => Right((prefixes :+ prefix.trim, areas))
              case AreaLine(name, items) =>
                val words = items.trim.split("\\s+").toVector.filter(_.nonEmpty)
                if (words.isEmpty)
                  Left(s"$file: line $number declares area $name with no items: $line")
                else if (areas.exists(_.name == name))
                  Left(s"$file: line $number repeats area $name: $line")
                else {
                  val families = words.filter(_.startsWith("family:")).map(_.stripPrefix("family:"))
                  val symbols = words.filterNot(_.startsWith("family:"))
                  Right((prefixes, areas :+ Area(name, symbols, families)))
                }
              case _ =>
                Left(
                  s"$file: line $number is neither `hide <prefix>` nor `area <name>: <items>`: $line"
                )
            }
        }
        .map { case (prefixes, areas) => Config(prefixes, areas) }
  }
}
