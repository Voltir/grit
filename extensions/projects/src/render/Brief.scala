package grit.projects.render

import scala.annotation.tailrec

import grit.projects.artifact.{Artifact, Body, Project}
import grit.projects.method.RoleView
import grit.projects.name.{ArtifactKey, KindName, MethodName, StateName}

/** Why a brief could not be given. */
enum BriefRefusal extends caps.Pure {

  /** `judged` is no artifact of the project. */
  case Unknown(judged: ArtifactKey)

  /** The artifact alone, its header line and body markdown, is `size` characters, over the budget. */
  case Over(size: Int)
}

/** What a session working on one artifact is given (ADR 0049), as text: a header line (key, kind,
  * state, title, method, version and the pin's short digest), the artifact's body whole, then a
  * `## Cited` section with each artifact its role view reaches, nearest first, under its own
  * `### <key> — <title>` heading, whole or, past the budget, as a pointer line
  * `- <key> (<kind>, <title>, <n> chars)`; artifacts at the same distance in key order. A function
  * of the project's value, not of the order its artifacts were read in: the hook file (written at
  * the claim) and `project_brief` show the same text for the project as it stood at the claim.
  *
  * The header line is `<key> | <kind> | <state> | <title> | <method> v<version> | pin <digest>`,
  * the digest its first [[PinDigits]] hex characters. `n` in a pointer, and the size [[BriefRefusal.Over]]
  * carries, are the characters of the artifact's body markdown, and of its header line and body
  * markdown together. The links followed are the artifact's `parent` and its `cites`, in its head
  * and its body, up to the view's depth; a path may pass through artifacts of kinds the view does
  * not name, but only artifacts of its kinds are cited. Pointers replace the farthest whole
  * citations first; then each pointer, nearest first, is given whole again if it still fits.
  * `pointed` lists the keys shown as pointers, and a citation that does not fit even as a
  * pointer is left out.
  */
final case class Brief private (artifact: ArtifactKey, text: String, pointed: Vector[ArtifactKey])
    extends caps.Pure

object Brief {

  /** 12: the hex digits of the pin's digest a header line shows. */
  val PinDigits: Int = 12

  /** `judged`'s brief for `view` within `project`, at most `chars` characters; `Left` with
    * [[BriefRefusal.Unknown]] when `judged` is no artifact of `project`, and with
    * [[BriefRefusal.Over]] when the artifact alone does not fit.
    */
  def of(
      project: Project,
      judged: ArtifactKey,
      view: RoleView,
      chars: Int
  ): Either[BriefRefusal, Brief] =
    project.artifacts.find(_.head.key == judged) match {
      case None => Left(BriefRefusal.Unknown(judged))
      case Some(own) =>
        val ownText = headerLine(project, own) + "\n\n" + Body.markdown(own.body)
        if (ownText.length > chars) Left(BriefRefusal.Over(ownText.length))
        else Right(fit(judged, ownText, reach(project, judged, view), chars))
    }

  private def headerLine(project: Project, a: Artifact): String =
    s"${a.head.key.value} | ${KindName.value(a.head.kind)} | ${StateName.value(a.head.state)} | " +
      s"${a.head.title} | ${MethodName.value(project.pin.method)} v${project.pin.version} | " +
      s"pin ${project.pin.digest.hex.take(PinDigits)}"

  /** What `a` links to: its parent, and what its head and its body cite. */
  private def neighbours(a: Artifact): Vector[ArtifactKey] =
    (a.head.parent.toVector ++ a.head.cites ++ Body.cites(a.body)).distinct

  /** The artifacts of the view's kinds within its depth of `judged`, with their distances,
    * nearest first and in key order at a distance; the path may pass through other kinds.
    */
  private def reach(
      project: Project,
      judged: ArtifactKey,
      view: RoleView
  ): Vector[(Artifact, Int)] = {
    def at(keys: Vector[ArtifactKey]): Vector[Artifact] =
      keys.flatMap(k => project.artifacts.find(_.head.key == k).toVector)
    val (_, _, reached) =
      (1 to view.depth).foldLeft((Set(judged), Vector(judged), Vector.empty[(Artifact, Int)])) {
        case ((seen, frontier, out), d) =>
          val next = at(frontier).flatMap(neighbours).distinct.filterNot(seen)
          (seen ++ next, next, out ++ at(next).map(a => (a, d)))
      }
    reached
      .filter { case (a, _) => view.kinds.contains(a.head.kind) }
      .sortBy { case (a, d) => (d, a.head.key.value) }
  }

  private def section(a: Artifact): String =
    s"### ${a.head.key.value} — ${a.head.title}\n\n${Body.markdown(a.body)}"

  private def pointer(a: Artifact): String =
    s"- ${a.head.key.value} (${KindName.value(a.head.kind)}, ${a.head.title}, " +
      s"${Body.markdown(a.body).length} chars)"

  /** The brief text with `items`, each whole or a pointer, in order. */
  private def render(ownText: String, items: Vector[(Artifact, Boolean)]): String =
    if (items.isEmpty) ownText
    else
      ownText + "\n\n## Cited\n\n" + items
        .map { case (a, whole) => if (whole) section(a) else pointer(a) }
        .mkString("\n\n")

  /** Whole citations, the farthest turned into pointers until the text fits; a pointer still
    * over the budget is dropped, farthest first.
    */
  @tailrec
  private def shrink(
      ownText: String,
      items: Vector[(Artifact, Boolean)],
      chars: Int
  ): Vector[(Artifact, Boolean)] =
    if (render(ownText, items).length <= chars) items
    else
      items.zipWithIndex.findLast { case ((_, whole), _) => whole } match {
        case None => shrink(ownText, items.dropRight(1), chars)
        case Some(((a, _), i)) =>
          shrink(
            ownText,
            items.zipWithIndex.map { case (item, j) => if (j == i) (a, false) else item },
            chars
          )
      }

  /** `items` with each pointer, nearest first, given whole again when the text still fits: one
    * pointed out before a larger one was can fit once that one is a pointer too.
    */
  @tailrec
  private def refill(
      ownText: String,
      items: Vector[(Artifact, Boolean)],
      chars: Int,
      from: Int = 0
  ): Vector[(Artifact, Boolean)] =
    items.zipWithIndex.find { case ((_, whole), j) => j >= from && !whole } match {
      case None => items
      case Some(((a, _), i)) =>
        val tried = items.zipWithIndex.map { case (item, j) => if (j == i) (a, true) else item }
        refill(ownText, if (render(ownText, tried).length <= chars) tried else items, chars, i + 1)
    }

  private def fit(
      judged: ArtifactKey,
      ownText: String,
      cited: Vector[(Artifact, Int)],
      chars: Int
  ): Brief = {
    val kept =
      refill(ownText, shrink(ownText, cited.map { case (a, _) => (a, true) }, chars), chars)
    new Brief(
      judged,
      render(ownText, kept),
      kept.collect { case (a, false) => a.head.key }
    )
  }
}
