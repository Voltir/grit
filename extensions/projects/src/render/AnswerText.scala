package grit.projects.render

import java.time.Instant

import grit.projects.artifact.ArtifactHead
import grit.projects.name.{KindName, StateName}

/** What the projects plugin's commands answer, the same for every edge that asks. */
object AnswerText {

  /** One line per head, in the order given: `<key> (<kind>, <state>): <title>`; `none` when
    * there is no head.
    */
  def heads(heads: Vector[ArtifactHead], none: String): String =
    if (heads.isEmpty) none
    else
      heads
        .map(h =>
          s"${h.key.value} (${KindName.value(h.kind)}, ${StateName.value(h.state)}): ${h.title}"
        )
        .mkString("\n")

  /** One line per version, in the order given: `<version> <written> by <by> (<state>): <title>`,
    * leaving out `by <by> ` when the head records no writer, and `<version> <written> withdrawn`
    * for a version with no head; `none` when there is none.
    */
  def versions(
      versions: Vector[(version: Long, written: Instant, head: Option[ArtifactHead])],
      none: String
  ): String =
    if (versions.isEmpty) none
    else
      versions
        .map(v =>
          v.head.fold(s"${v.version} ${v.written} withdrawn")(h =>
            s"${v.version} ${v.written} ${h.by.fold("")(b => s"by $b ")}" +
              s"(${StateName.value(h.state)}): ${h.title}"
          )
        )
        .mkString("\n")

  /** A line diff of `older` to `newer`, two texts named `from` and `to`: a `--- <from>` and a
    * `+++ <to>` line, then every line of either in order, `- ` before one only `older` holds,
    * `+ ` before one only `newer` holds and two spaces before one both hold, each removal before
    * the addition replacing it.
    */
  def diff(from: String, older: String, to: String, newer: String): String = {
    val (a, b) = (older.linesIterator.toVector, newer.linesIterator.toVector)
    // longest(i)(j): the longest common subsequence of a.drop(i) and b.drop(j).
    val longest = Array.ofDim[Int](a.size + 1, b.size + 1)
    for (i <- a.indices.reverse; j <- b.indices.reverse)
      longest(i)(j) =
        if (a(i) == b(j)) longest(i + 1)(j + 1) + 1
        else longest(i + 1)(j).max(longest(i)(j + 1))
    val out = Vector.newBuilder[String]
    var (i, j) = (0, 0)
    while (i < a.size || j < b.size)
      if (i < a.size && j < b.size && a(i) == b(j)) { out += s"  ${a(i)}"; i += 1; j += 1 }
      else if (j == b.size || (i < a.size && longest(i + 1)(j) >= longest(i)(j + 1))) {
        out += s"- ${a(i)}"; i += 1
      } else { out += s"+ ${b(j)}"; j += 1 }
    (Vector(s"--- $from", s"+++ $to") ++ out.result()).mkString("\n")
  }

  /** A page of export files as a JSON object a program reads: `files`, each its `path` and its
    * `text`, in order; and `next`, the path to ask for the page after from, or `null` on the
    * last page.
    */
  def page(files: Vector[(String, String)], next: Option[String]): String =
    ujson.write(
      ujson.Obj(
        "files" -> ujson.Arr.from(files.map((p, t) => ujson.Obj("path" -> p, "text" -> t))),
        "next" -> next.fold[ujson.Value](ujson.Null)(ujson.Str(_))
      )
    )
}
