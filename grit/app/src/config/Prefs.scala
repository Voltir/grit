package grit.app.config

import java.nio.file.{Files, Path, StandardCopyOption}

import scala.util.control.NonFatal

/** What grit remembers between runs on this machine: the `theme` last chosen, by key.
  * Kept in a small file grit writes itself ([[Prefs.path]]), as `name=value` lines. A line
  * it does not know is ignored, and so is a file it cannot read: a stale or broken file
  * never stops a launch, it is only forgotten.
  */
final case class Prefs(theme: Option[String])

object Prefs {

  /** Nothing remembered. */
  val empty: Prefs = Prefs(None)

  /** The preferences `contents` holds; the last of a repeated name wins. */
  def parse(contents: String): Prefs =
    contents.linesIterator
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .foldLeft(empty) { (prefs, line) =>
        line.split("=", 2) match {
          case Array(name, value) if name.trim == ThemeKey && value.trim.nonEmpty =>
            prefs.copy(theme = Some(value.trim))
          case _ => prefs
        }
      }

  /** `prefs` as the file holds them: what [[parse]] reads back as `prefs`. */
  def render(prefs: Prefs): String =
    s"# written by grit: what it remembers between runs\n" +
      prefs.theme.fold("")(t => s"$ThemeKey=$t\n")

  /** Where the preferences live for `env`: `grit/prefs` under `XDG_CONFIG_HOME`, or under
    * `.config` in `HOME` when that is unset or empty. `None` when neither is set.
    */
  def path(env: Map[String, String]): Option[Path] =
    env
      .get("XDG_CONFIG_HOME")
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .orElse(env.get("HOME").filter(_.nonEmpty).map(Path.of(_, ".config")))
      .map(_.resolve("grit").resolve("prefs"))

  /** The preferences in the file at `file`; [[empty]] when it is missing or unreadable. */
  def load(file: Path): Prefs =
    try if (Files.isRegularFile(file)) parse(Files.readString(file)) else empty
    catch { case NonFatal(_) => empty }

  /** `prefs` written to `file`, its directories made as needed, replacing what was there
    * in one move; or why it could not be.
    */
  def save(file: Path, prefs: Prefs): Either[String, Path] =
    try {
      val dir = file.toAbsolutePath.getParent
      val _ = Files.createDirectories(dir)
      val staged = Files.createTempFile(dir, "prefs", ".tmp")
      val _ = Files.writeString(staged, render(prefs))
      Right(Files.move(staged, file, StandardCopyOption.REPLACE_EXISTING))
    } catch {
      case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString))
    }

  private val ThemeKey = "theme"
}
