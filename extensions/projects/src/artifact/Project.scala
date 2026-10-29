package grit.projects.artifact

import scala.annotation.tailrec

import grit.projects.name.{ArtifactKey, Pin, ProjectKey}

/** A project as one value: its key, the method version it pinned, and its artifacts by key. */
final case class Project private (key: ProjectKey, pin: Pin, artifacts: Vector[Artifact])
    extends caps.Pure

object Project {

  /** These as a project, or why not, every reason joined by `; `: an artifact of another
    * project, two artifacts of one key, a parent or a cite (in its head or its body) that is
    * none of them, or a parent cycle.
    */
  def of(key: ProjectKey, pin: Pin, artifacts: Vector[Artifact]): Either[String, Project] =
    refusals(key, artifacts) match {
      case Vector() => Right(new Project(key, pin, artifacts))
      case some => Left(some.map(_._2).mkString("; "))
    }

  /** What [[withdrawn]] leaves: `project` without the artifacts withdrawn; `gone`, their keys,
    * the one named first, then the rest by key; and `uncited`, by key, each ended artifact left
    * citing one of them as rewritten not to, its `by` as it was.
    */
  final case class Withdrawn(project: Project, gone: Vector[ArtifactKey], uncited: Vector[Artifact])
      extends caps.Pure

  /** `project` without `key`, or why not, naming each artifact in the way: a `key` it lacks;
    * without `cascade`, any artifact having `key` as its parent or citing it. With `cascade`,
    * without every artifact under `key` by parent too, each artifact `ended` holds of that cites
    * one of them rewritten not to; refused for an artifact under `key` that `ended` does not
    * hold of, and for one outside them that it does not hold of citing one of them.
    */
  def withdrawn(
      project: Project,
      key: ArtifactKey,
      cascade: Boolean,
      ended: ArtifactHead -> Boolean
  ): Either[String, Withdrawn] = {
    val children = project.artifacts.groupMap(_.head.parent)(_.head.key)
    @tailrec def under(
        frontier: Vector[ArtifactKey],
        found: Vector[ArtifactKey]
    ): Vector[ArtifactKey] =
      frontier.flatMap(k => children.getOrElse(Some(k), Vector.empty)) match {
        case Vector() => found
        case next => under(next, found ++ next)
      }
    val gone = key +: (if (cascade) under(Vector(key), Vector.empty).sortBy(_.value) else Vector())
    val withdrawing = gone.toSet
    val (inside, outside) = project.artifacts.partition(a => withdrawing(a.head.key))
    val citers = outside.filter(_.cites.exists(withdrawing))
    def named(as: Vector[Artifact]): String = as.map(_.head.key.value).sorted.mkString(", ")
    if (inside.isEmpty)
      Left(s"project ${ProjectKey.value(project.key)} has no artifact ${key.value}")
    else if (!cascade)
      referrers(key, outside) match {
        case Vector() => remainder(project, gone, outside, Vector())
        case some =>
          Left(
            s"${key.value} is named by ${some.map(_.value).mkString(", ")}: withdraw them " +
              "first, or withdraw it with cascade"
          )
      }
    else {
      val open = inside.filter(a => a.head.key != key && !ended(a.head))
      val live = citers.filterNot(a => ended(a.head))
      if (open.nonEmpty)
        Left(
          s"${key.value} has artifacts under it not ended: ${named(open)}: end or withdraw them first"
        )
      else if (live.nonEmpty)
        Left(
          s"artifacts not ended cite what would be withdrawn: ${named(live)}: end them, or drop " +
            "their citations, first"
        )
      else
        remainder(
          project,
          gone,
          outside,
          citers
            .map(a =>
              Artifact(
                a.head.copy(cites = a.head.cites.filterNot(withdrawing)),
                Body.uncited(a.body, withdrawing)
              )
            )
            .sortBy(_.head.key.value)
        )
    }
  }

  /** `outside` as `project`, each of `uncited` in place of the artifact of its key. */
  private def remainder(
      project: Project,
      gone: Vector[ArtifactKey],
      outside: Vector[Artifact],
      uncited: Vector[Artifact]
  ): Either[String, Withdrawn] = {
    val rewritten = uncited.map(a => a.head.key -> a).toMap
    of(project.key, project.pin, outside.map(a => rewritten.getOrElse(a.head.key, a)))
      .map(Withdrawn(_, gone, uncited))
  }

  /** The keys `among` name as their parents or cite. */
  def named(among: Vector[Artifact]): Set[ArtifactKey] =
    among.flatMap(a => a.head.parent ++ a.cites).toSet

  /** The keys of those of `among` naming `key` as their parent or citing it, sorted. */
  def referrers(key: ArtifactKey, among: Vector[Artifact]): Vector[ArtifactKey] =
    among
      .filter(a => a.head.parent.contains(key) || a.cites.contains(key))
      .map(_.head.key)
      .sortBy(_.value)

  /** Why `artifacts` are not project `key`'s, each with the artifact it is about. */
  private[projects] def refusals(
      key: ProjectKey,
      artifacts: Vector[Artifact]
  ): Vector[(ArtifactKey, String)] = {
    val keys = artifacts.map(_.head.key)
    val parents = artifacts.flatMap(a => a.head.parent.map(a.head.key -> _)).toMap
    @tailrec def cycles(k: ArtifactKey, seen: Set[ArtifactKey]): Boolean = parents.get(k) match {
      case Some(p) if seen(p) => true
      case Some(p) => cycles(p, seen + p)
      case None => false
    }
    artifacts.flatMap { a =>
      val k = a.head.key
      val links = a.cites
      Option.when(k.project != key)(
        k -> s"${k.value} is not of project ${ProjectKey.value(key)}"
      ) ++
        Option.when(keys.count(_ == k) > 1)(k -> s"${k.value} is two artifacts") ++
        a.head.parent
          .filterNot(keys.contains)
          .map(p => k -> s"${k.value}'s parent ${p.value} is no artifact of it") ++
        links
          .filterNot(keys.contains)
          .map(c => k -> s"${k.value} cites ${c.value}, no artifact of it") ++
        Option.when(cycles(k, Set(k)))(k -> s"${k.value} is its own ancestor")
    }.distinct
  }
}
