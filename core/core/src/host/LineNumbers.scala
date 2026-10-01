package grit.core.host

/** How [[Workspace.read]] shows a file's lines, `cat -n` style: each line is its number
  * right-aligned in [[Width]] columns (wider for a number that needs more), a tab, then the
  * line's text. The numbers count from 1, as [[Workspace.search]]'s `path:line:` do. They are
  * presentation only: no file holds them.
  */
object LineNumbers {

  /** The columns a line's number is right-aligned in, as `cat -n` uses. */
  val Width = 6

  // `(?s)`: the rest of a line may hold a `\r`, which `.` alone does not match.
  private val Prefixed = """(?s) *\d+\t(.*)""".r

  /** `lines`, the first of them line `first` of its file, each numbered. */
  def show(first: Int, lines: Vector[String]): Vector[String] =
    lines.zipWithIndex.map { (line, i) =>
      val n = (first + i).toString
      " " * (Width - n.length).max(0) + n + "\t" + line
    }

  /** `text` with the number and tab [[show]] puts before each line taken off, when every line
    * of it starts with one (any spaces, then digits, then a tab); `None` when a line does not,
    * or `text` is empty. A final line break ends the last line and is kept.
    */
  def strip(text: String): Option[String] = {
    val parts = text.split("\n", -1).toVector
    val lines = if (text.endsWith("\n")) parts.dropRight(1) else parts
    val stripped = lines.map {
      case Prefixed(rest) => Some(rest)
      case _ => None
    }
    Option.when(lines.nonEmpty && stripped.forall(_.isDefined)) {
      stripped.flatten.mkString("\n") + (if (text.endsWith("\n")) "\n" else "")
    }
  }
}
