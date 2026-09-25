package grit.core.host

/** Reading the checkout. A path that is a link is followed, and refused
  * ([[HostError.Outside]]) when it leads outside the checkout or to a secrets file.
  */
trait Workspace extends caps.SharedCapability {

  /** The lines `lines` of the file `path`, clipped from the head ([[Clipped.head]]). When
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
    * that are not UTF-8. "No matches." when nothing matches. Clipped from the head, the hint
    * saying how many matching lines were left out. Fails on a pattern that does not compile
    * or an `under` that is missing.
    */
  def search(pattern: String, under: RelPath): Either[HostError, Clipped]
}

object Workspace {

  /** The largest file read or searched: 16 MB. */
  val MaxFileBytes: Long = 16L * 1024 * 1024

  /** The most characters of a matching line [[Workspace.search]] shows. */
  val MaxLineChars = 500

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
