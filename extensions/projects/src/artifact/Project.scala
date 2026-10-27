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
      val links = (a.head.cites ++ Body.cites(a.body)).distinct
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
