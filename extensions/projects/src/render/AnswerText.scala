package grit.projects.render

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
