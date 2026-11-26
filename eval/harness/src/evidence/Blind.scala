package grit.eval.harness.evidence

/** A task's diffs packed for a reviewer who is not to know which arm wrote which. */
object Blind {

  /** What a reviewer is shown, each file by its name in the packing's directory and its text, and
    * `key`, each shown diff's letter (`A`, `B`, …) to the label it was given, never shown.
    */
  final case class Packed(files: Vector[(String, String)], key: Map[String, String])
      extends caps.Pure

  /** `diffs`, each a label and its diff, packed with `task`'s brief as `brief.md` and `rubric` as
    * `rubric.md`: the diffs as `diff-A.patch`, `diff-B.patch`, … in an order `seed` alone decides,
    * the same seed giving the same order. No label reaches a shown file: each diff keeps only its
    * file sections, each from its `diff --git` line to its last hunk line, with no `index` line, so
    * every commit's message, author, trailers and blob ids are dropped. Refused when fewer than 2
    * or more than 26 diffs are given, when two share a label, or when one holds no file section,
    * naming its label.
    */
  def packed(
      task: BlindTask,
      rubric: String,
      diffs: Vector[(String, String)],
      seed: Long
  ): Either[String, Packed] = {
    val labels = diffs.map(_._1)
    val sections = diffs.map((label, diff) => (label, sectionsOf(diff)))
    if (diffs.size < 2 || diffs.size > 26) Left(s"2 to 26 diffs are packed, not ${diffs.size}")
    else
      labels.diff(labels.distinct).headOption match {
        case Some(twice) => Left(s"the label $twice is given twice")
        case None =>
          sections.collectFirst { case (label, "") => label } match {
            case Some(empty) => Left(s"the diff $empty holds no file section")
            case None =>
              // A seeded generator, local to this call: the order is a function of `seed`.
              val shuffled = new scala.util.Random(seed).shuffle(sections)
              val lettered = shuffled.zipWithIndex.map { case ((label, kept), i) =>
                (('A' + i).toChar.toString, label, kept)
              }
              Right(
                Packed(
                  Vector("brief.md" -> task.brief, "rubric.md" -> rubric) ++
                    lettered.map((letter, _, kept) => s"diff-$letter.patch" -> kept),
                  lettered.map((letter, label, _) => letter -> label).toMap
                )
              )
          }
      }
  }

  /** Where a line of a diff falls: outside any file section, in a section's header, or in a hunk
    * with `old` and `added` of its lines still to come.
    */
  private enum At {
    case Outside, Header
    case Hunk(old: Int, added: Int)
  }

  private val Headers: Vector[String] = Vector(
    "old mode ",
    "new mode ",
    "deleted file mode ",
    "new file mode ",
    "similarity index ",
    "dissimilarity index ",
    "rename from ",
    "rename to ",
    "copy from ",
    "copy to ",
    "--- ",
    "+++ ",
    "Binary files "
  )

  private val HunkHead = """^@@ -\d+(?:,(\d+))? \+\d+(?:,(\d+))? @@.*""".r

  private def hunk(line: String): Option[At.Hunk] = line match {
    case HunkHead(old, added) =>
      Some(At.Hunk(Option(old).fold(1)(_.toInt), Option(added).fold(1)(_.toInt)))
    case _ => None
  }

  /** `diff`'s file sections alone, without their `index` lines; empty when it holds none. */
  private def sectionsOf(diff: String): String = {
    val kept = diff.linesIterator
      .foldLeft((At.Outside: At, Vector.empty[String])) { case ((at, out), line) =>
        val next: At =
          if (line.startsWith("diff --git ")) At.Header
          else
            at match {
              case At.Outside => At.Outside
              case At.Header | At.Hunk(0, 0) =>
                hunk(line).getOrElse(
                  if (
                    line.startsWith("\\") ||
                    (at == At.Header && (line.startsWith("index ") || Headers
                      .exists(line.startsWith)))
                  ) at
                  else At.Outside
                )
              case At.Hunk(old, added) =>
                if (line.startsWith(" ")) At.Hunk(old - 1, added - 1)
                else if (line.startsWith("-")) At.Hunk(old - 1, added)
                else if (line.startsWith("+")) At.Hunk(old, added - 1)
                else if (line.startsWith("\\")) at
                else At.Outside
            }
        val shown = next != At.Outside && !(next == At.Header && line.startsWith("index "))
        (next, if (shown) out :+ line else out)
      }
      ._2
    if (kept.isEmpty) "" else kept.mkString("", "\n", "\n")
  }
}
