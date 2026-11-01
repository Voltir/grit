package grit.projects.render

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.claim.{Claim, ClaimRole}
import grit.core.id.ClaimHolder
import grit.projects.artifact.{Artifact, ArtifactHead, Body}
import grit.projects.name.{ArtifactKey, KindName, StateName}

/** What the projects plugin's commands answer, the same for every edge that asks. */
object AnswerText {

  /** One line per head, in the order given: `<key> (<kind>, <state>): <title>`; `none` when
    * there is no head.
    */
  def heads(heads: Vector[ArtifactHead], none: String): String =
    if (heads.isEmpty) none else heads.map(head).mkString("\n")

  /** One line per artifact, in the order given: its [[heads]] line, then ` · under <parent>`
    * when it has a parent, then ` · ` and its body's [[Body.lead]] when that is not empty;
    * `none` when there is none.
    */
  def listed(artifacts: Vector[Artifact], none: String): String =
    if (artifacts.isEmpty) none
    else
      artifacts
        .map(a =>
          (head(a.head) +: (a.head.parent.map(p => s"under ${p.value}").toVector ++
            Vector(Body.lead(a.body)).filter(_.nonEmpty))).mkString(" · ")
        )
        .mkString("\n")

  private def head(h: ArtifactHead): String =
    s"${h.key.value} (${KindName.value(h.kind)}, ${StateName.value(h.state)}): ${h.title}"

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

  /** What a withdrawal answers: a `Withdrawn: ` line naming `gone`, in the order given; a
    * `Citations of them dropped from: ` line naming `uncited` when there are any; and that their
    * history stays readable `readable` long, in whole days, through `project_show` and
    * `project_history`.
    */
  def withdrawn(
      gone: Vector[ArtifactKey],
      uncited: Vector[ArtifactKey],
      readable: FiniteDuration
  ): String =
    (Vector(s"Withdrawn: ${gone.map(_.value).mkString(", ")}") ++
      Option.when(uncited.nonEmpty)(
        s"Citations of them dropped from: ${uncited.map(_.value).mkString(", ")}"
      ) :+
      s"Their history stays readable for ${readable.toDays} days: project_show with an `at` " +
      "before now, and project_history.").mkString("\n")

  /** A freed claim as a JSON object a program reads: `released`, the artifact's key, and
    * `holder`, who it was claimed for, which no longer works it.
    */
  def released(key: ArtifactKey, holder: ClaimHolder): String =
    ujson.write(ujson.Obj("released" -> key.value, "holder" -> ClaimHolder.value(holder)))

  /** Live claims as a JSON object a program reads: `claims`, each its artifact's `key`, its
    * `role`, `holder`, who it is claimed for, `by`, who claimed it (written as given), `since`,
    * when it was made, and `lapses`, when it lapses unless its holder keeps working; in the order
    * given.
    */
  def claims(held: Vector[(ArtifactKey, Claim, String)]): String =
    ujson.write(
      ujson.Obj(
        "claims" -> ujson.Arr.from(held.map { (key, c, by) =>
          ujson.Obj(
            "key" -> key.value,
            "role" -> ClaimRole.value(c.role),
            "holder" -> ClaimHolder.value(c.holder),
            "by" -> by,
            "since" -> c.made.toString,
            "lapses" -> c.lapses.toString
          )
        })
      )
    )

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
