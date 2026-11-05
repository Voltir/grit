package grit.core.host

/** Reading the checkout. A path that is a link is followed, and refused
  * ([[HostError.Outside]]) when it leads outside the checkout or to a secrets file.
  */
trait Workspace extends caps.SharedCapability {

  /** The lines `lines` of the file `path`, each numbered ([[LineNumbers]]), clipped from
    * the head ([[Clipped.head]]), the numbers counted in its bytes. When
    * lines are left out, by the clip or by `lines.limit`, the hint says which lines are
    * shown, of how many, and the offset to continue from ("Use offset=2001 to continue.").
    * Fails when there is no such file, it is a directory, it is over
    * [[Workspace.MaxFileBytes]] or not UTF-8, or `lines.offset` is past its end (an empty
    * file has no lines, but offset 1 reads it as empty).
    */
  def read(path: RelPath, lines: Lines): Either[HostError, Clipped]

  /** The entries under the directory `dir`, `depth` levels down (1: its own entries; below
    * 1 reads as 1), one per line as paths relative to `dir`, sorted, a directory's with a
    * final `/`; `(empty)` when there are none. A directory [[Workspace.skipped]] is listed
    * but not entered, and a link is not followed. Clipped from the head, the hint saying how many entries were left out. Fails when `dir` is missing
    * or a file.
    */
  def list(dir: RelPath, depth: Int): Either[HostError, Clipped]

  /** Every line matching the regular expression `pattern` (`java.util.regex`, found
    * anywhere in the line) in the files under `under`, or in `under` when it is a file:
    * `path:line: text`, paths relative to the root in sorted order, lines counting from 1,
    * a line's text cut at [[Workspace.MaxLineChars]] characters. Skips secrets files, links,
    * the directories [[Workspace.skipped]], files over [[Workspace.MaxFileBytes]] and files
    * that are not UTF-8. Answered as [[Workspace.searched]] says: every matching line, or,
    * past [[Workspace.SearchLines]] lines or [[Workspace.SearchBytes]] bytes, each matching
    * file's count instead. Fails on a pattern that does not compile or an `under` that is
    * missing.
    */
  def search(pattern: String, under: RelPath): Either[HostError, Clipped]
}

object Workspace {

  /** The largest file read or searched: 16 MB. */
  val MaxFileBytes: Long = 16L * 1024 * 1024

  /** The most characters of a matching line [[Workspace.search]] shows. */
  val MaxLineChars = 500

  /** The most matching lines a search shows: 200. One more, and it shows none of them. */
  val SearchLines = 200

  /** The most bytes of UTF-8 a search's matching lines may hold, a newline each, to be shown:
    * 20 KB. One more, and it shows none of them.
    */
  val SearchBytes: Int = 20 * 1024

  /** A search's answer from `found`, each searched file's path and its matching lines in path
    * order: "No matches." when no file has one; every matching line when they are at most
    * [[SearchLines]] and [[SearchBytes]]; else no line, but how many matched in how many files,
    * which limit they passed, that the counts follow and to narrow the path or the pattern,
    * then `path: n` for each matching file. Not a partial listing: the counts say where to
    * search again. Clipped from the head ([[Clipped.cutHint]]) when even the counts are over
    * its limits.
    */
  def searched(found: Vector[(String, Vector[String])]): Clipped = {
    val matching = found.filter(_._2.nonEmpty)
    val lines = matching.flatMap(_._2)
    val bytes = lines.map(_.getBytes(java.nio.charset.StandardCharsets.UTF_8).length + 1).sum
    if (lines.isEmpty) Clipped.head("No matches.", _ => None)
    else if (lines.size <= SearchLines && bytes <= SearchBytes)
      Clipped.head(lines.mkString("\n"), Clipped.cutHint)
    else {
      val files = if (matching.size == 1) "1 file" else s"${matching.size} files"
      val why =
        if (lines.size > SearchLines) s"too many to show (the limit is $SearchLines)"
        else s"too long to show (the limit is ${SearchBytes / 1024} KB)"
      Clipped.head(
        ((s"${lines.size} matches in $files, $why. Matches per file are below; narrow the " +
          "path or the pattern to see the lines.") +:
          matching.map((path, ls) => s"$path: ${ls.size}")).mkString("\n"),
        Clipped.cutHint
      )
    }
  }

  /** Build output and package directories, which [[Workspace.list]] and
    * [[Workspace.search]] do not enter.
    */
  val Skipped: Set[String] = Set("node_modules", "out", "target")

  /** Whether `list` and `search` stay out of a directory named `name`: a hidden one (its
    * name starts with `.`, such as `.git`) or one of [[Skipped]].
    */
  def skipped(name: String): Boolean = name.startsWith(".") || Skipped.contains(name)
}

/** Which lines of a file to read: from line `offset`, counting from 1, and at most `limit`
  * of them, or to the end.
  */
final case class Lines private (offset: Int, limit: Option[Int])

object Lines {

  /** The whole file. */
  val All: Lines = Lines(1, None)

  /** An `offset` below 1 reads as 1, and a `limit` below 1 as 1. */
  def of(offset: Int, limit: Option[Int]): Lines = Lines(offset.max(1), limit.map(_.max(1)))
}
