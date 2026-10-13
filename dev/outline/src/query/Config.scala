package grit.outline.query

import grit.outline.locate.Root

/** A named set of symbols and families, read from an `area` line: `symbols` are names as `show` takes them, `families` the traits (`family:<Trait>` on the line) whose implementations and contracts it holds, and `purpose` the line saying what the area is for, which `area` with no name lists. */
final case class Area(
    name: String,
    symbols: Vector[String],
    families: Vector[String],
    purpose: Option[String] = None
)

/** The query settings a repository gives in its own `.outline.conf`: source paths no query shows unless it names them fully, the named areas `area` answers for, and the call `tests` reads test names from. */
final case class Config(
    hidden: Vector[String],
    areas: Vector[Area],
    testCall: String = Config.defaultTestCall
)

object Config {

  /** The test call when a repository names none. */
  val defaultTestCall: String = "test"

  /** No settings: nothing is hidden, no area is declared and the test call is `test`. */
  val empty: Config = Config(Vector.empty, Vector.empty)

  private val Hide = raw"hide\s+(.+)".r
  private val AreaLine = raw"""area\s+([^:\s"]+)(?:\s+"([^"]*)")?\s*:(.*)""".r
  private val TestCallLine = raw"test-call\s+(\S+)".r

  /** `root`'s `.outline.conf`, or `empty` when it has none: lines `hide <prefix>` (a repo-relative source path prefix), `area <name> "<purpose>": <item> …` (the quoted purpose optional; an item a symbol name, or `family:<Trait>`) and `test-call <name>` (the call `tests` reads); `#` comments and blank lines are skipped. `Left` names the line that is none of these, an area with no items, an area whose name a line before already declared, and a `test-call` line after the first. */
  def read(root: Root): Either[String, Config] = {
    val file = root.dir / ".outline.conf"
    if (!os.isFile(file)) Right(empty)
    else
      os.read
        .lines(file)
        .zipWithIndex
        .map { case (line, index) => (line.trim, index + 1) }
        .filter { case (line, _) => line.nonEmpty && !line.startsWith("#") }
        .foldLeft[Either[String, (Vector[String], Vector[Area], Option[String])]](
          Right((Vector.empty, Vector.empty, None))
        ) {
          case (Left(message), _) => Left(message)
          case (Right((prefixes, areas, testCall)), (line, number)) =>
            line match {
              case Hide(prefix) => Right((prefixes :+ prefix.trim, areas, testCall))
              case AreaLine(name, purpose, items) =>
                val words = items.trim.split("\\s+").toVector.filter(_.nonEmpty)
                if (words.isEmpty)
                  Left(s"$file: line $number declares area $name with no items: $line")
                else if (areas.exists(_.name == name))
                  Left(s"$file: line $number repeats area $name: $line")
                else {
                  val families = words.filter(_.startsWith("family:")).map(_.stripPrefix("family:"))
                  val symbols = words.filterNot(_.startsWith("family:"))
                  Right(
                    (
                      prefixes,
                      areas :+ Area(
                        name,
                        symbols,
                        families,
                        Option(purpose).map(_.trim).filter(_.nonEmpty)
                      ),
                      testCall
                    )
                  )
                }
              case TestCallLine(name) =>
                if (testCall.isDefined) Left(s"$file: line $number repeats test-call: $line")
                else Right((prefixes, areas, Some(name)))
              case _ =>
                Left(
                  s"$file: line $number is neither `hide <prefix>`, `area <name> $"<purpose>$": <items>` nor `test-call <name>`: $line"
                )
            }
        }
        .map { case (prefixes, areas, testCall) =>
          Config(prefixes, areas, testCall.getOrElse(Config.defaultTestCall))
        }
  }
}
