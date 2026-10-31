package grit.eval.harness.bench

/** A diff's changed lines in source files (`*.scala`, `build.mill`, `schema.sql`), counted by file,
  * sign and text.
  */
final case class Changed private (lines: Map[(String, Char, String), Int]) extends caps.Pure

object Changed {

  /** The changed source lines of `diff`, as `git diff -U0 --no-renames` prints one. */
  def of(diff: String): Changed = {
    // Walks the lines once: a `diff --git` line opens a file and leaves its hunks, an `@@` line
    // opens a hunk, and inside one each `+` or `-` line is a change of the file open.
    val counts = scala.collection.mutable.Map.empty[(String, Char, String), Int]
    var file: Option[String] = None
    var inHunk = false
    diff.linesIterator.foreach { line =>
      if (line.startsWith("diff --git ")) {
        file = line.split(" b/", 2).lift(1).filter(source)
        inHunk = false
      } else if (line.startsWith("@@")) inHunk = true
      else if (inHunk && (line.startsWith("+") || line.startsWith("-")))
        file.foreach { path =>
          val key = (path, line.charAt(0), line.drop(1))
          counts(key) = counts.getOrElse(key, 0) + 1
        }
    }
    new Changed(counts.toMap)
  }

  /** How many changed lines `a` and `b` differ by, each counted as often as one has it more than
    * the other; 0 when they change the same lines.
    */
  def distance(a: Changed, b: Changed): Long =
    (a.lines.keySet ++ b.lines.keySet).toVector
      .map(k => math.abs(a.lines.getOrElse(k, 0) - b.lines.getOrElse(k, 0)).toLong)
      .sum

  /** Whether the file at `path` is source: Scala, `build.mill` or `schema.sql`. */
  private def source(path: String): Boolean = {
    val name = path.split('/').lastOption.getOrElse(path)
    name.endsWith(".scala") || name == "build.mill" || name == "schema.sql"
  }
}
