package grit.outline.query

import grit.outline.locate.Root

/** A named set of symbols and families, read from an `area` line: `symbols` are names as `show` takes them, `families` the traits (`family:<Trait>` on the line) whose implementations and contracts it holds, and `purpose` the line saying what the area is for, which `area` with no name lists. */
final case class Area(
    name: String,
    symbols: Vector[String],
    families: Vector[String],
    purpose: Option[String] = None
)

/** The query settings a repository gives in its own `.outline.conf`: source paths no query shows unless it names them fully, the named areas `area` answers for, the call `tests` reads test names from, and the `secondary` source path prefixes, main sources a short name rarely means: a short name that one definition outside them and outside test sources matches resolves to that one. `ignored` holds each line naming a setting this outline does not know, as `line N: <text>`, which every answer names. */
final case class Config(
    hidden: Vector[String],
    areas: Vector[Area],
    testCall: String = Config.defaultTestCall,
    secondary: Vector[String] = Vector.empty,
    ignored: Vector[String] = Vector.empty
) {

  /** Whether `path` (repo-relative source path text) lies under one of the `hidden` prefixes. */
  def hides(path: String): Boolean = hidden.exists(prefix => path.startsWith(prefix))
}

object Config {

  /** The test call when a repository names none. */
  val defaultTestCall: String = "test"

  /** No settings: nothing is hidden, no area is declared and the test call is `test`. */
  val empty: Config = Config(Vector.empty, Vector.empty)

  private val Hide = raw"hide\s+(.+)".r
  private val AreaLine = raw"""area\s+([^:\s"]+)(?:\s+"([^"]*)")?\s*:(.*)""".r
  private val TestCallLine = raw"test-call\s+(\S+)".r
  private val SecondaryLine = raw"secondary\s+(.+)".r

  // The settings this outline reads; a line opening with any other word is a newer outline's, and is ignored.
  private val known = Set("hide", "secondary", "area", "test-call")

  /** `root`'s `.outline.conf`, or `empty` when it has none: lines `hide <prefix>` and `secondary <prefix>` (each a repo-relative source path prefix), `area <name> "<purpose>": <item> …` (the quoted purpose optional; an item a symbol name, or `family:<Trait>`) and `test-call <name>` (the call `tests` reads); `#` comments and blank lines are skipped, and a line opening with any other word is kept in `ignored`. `Left` names a line opening with one of these words that is none of them, an area with no items, an area whose name a line before already declared, and a `test-call` line after the first. */
  def read(root: Root): Either[String, Config] = {
    val file = root.dir / ".outline.conf"
    if (!os.isFile(file)) Right(empty)
    else
      os.read
        .lines(file)
        .zipWithIndex
        .map { case (line, index) => (line.trim, index + 1) }
        .filter { case (line, _) => line.nonEmpty && !line.startsWith("#") }
        // The config so far, and whether a test-call line has been read.
        .foldLeft[Either[String, (Config, Boolean)]](Right((empty, false))) {
          case (Left(message), _) => Left(message)
          case (Right((config, namedTestCall)), (line, number)) =>
            line match {
              case Hide(prefix) =>
                Right((config.copy(hidden = config.hidden :+ prefix.trim), namedTestCall))
              case SecondaryLine(prefix) =>
                Right((config.copy(secondary = config.secondary :+ prefix.trim), namedTestCall))
              case AreaLine(name, purpose, items) =>
                val words = items.trim.split("\\s+").toVector.filter(_.nonEmpty)
                if (words.isEmpty)
                  Left(s"$file: line $number declares area $name with no items: $line")
                else if (config.areas.exists(_.name == name))
                  Left(s"$file: line $number repeats area $name: $line")
                else {
                  val families = words.filter(_.startsWith("family:")).map(_.stripPrefix("family:"))
                  val symbols = words.filterNot(_.startsWith("family:"))
                  val area =
                    Area(name, symbols, families, Option(purpose).map(_.trim).filter(_.nonEmpty))
                  Right((config.copy(areas = config.areas :+ area), namedTestCall))
                }
              case TestCallLine(name) =>
                if (namedTestCall) Left(s"$file: line $number repeats test-call: $line")
                else Right((config.copy(testCall = name), true))
              case _ if !known.contains(line.takeWhile(!_.isWhitespace)) =>
                Right(
                  (config.copy(ignored = config.ignored :+ s"line $number: $line"), namedTestCall)
                )
              case _ =>
                Left(
                  s"$file: line $number is none of `hide <prefix>`, `secondary <prefix>`, `area <name> $"<purpose>$": <items>` and `test-call <name>`: $line"
                )
            }
        }
        .map { case (config, _) => config }
  }
}
