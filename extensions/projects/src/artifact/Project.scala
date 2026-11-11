package grit.projects.artifact

import scala.annotation.tailrec

import grit.core.id.ClaimHolder
import grit.projects.name.{ArtifactKey, Pin, ProjectKey}

/** A project as one value: its key, the method version it pinned, and its artifacts by key. */
final case class Project private (key: ProjectKey, pin: Pin, artifacts: Vector[Artifact])
    extends caps.Pure

object Project {

  /** These as a project, or why not, every reason joined by `; `: an artifact of another
    * project, two artifacts of one key, a parent or a cite (in its head or its body) that is
    * none of them (naming the nearest that are, [[ArtifactKey.near]]), or a parent cycle.
    */
  def of(key: ProjectKey, pin: Pin, artifacts: Vector[Artifact]): Either[String, Project] =
    refusals(key, artifacts) match {
      case Vector() => Right(new Project(key, pin, artifacts))
      case some => Left(some.map(_._2).mkString("; "))
    }

  /** What a withdrawal leaves: `project` without the artifacts withdrawn; `gone`, each as it was,
    * in the order its producer states; and `uncited`, by key, each artifact left citing one of
    * them as rewritten not to, its `by` as it was.
    */
  final case class Withdrawn(project: Project, gone: Vector[Artifact], uncited: Vector[Artifact])
      extends caps.Pure

  /** `project` without `key`, or why not, naming each artifact in the way: a `key` it lacks;
    * without `cascade`, any artifact having `key` as its parent or citing it. With `cascade`,
    * without every artifact under `key` by parent too, each artifact `ended` holds of that cites
    * one of them rewritten not to; refused for an artifact under `key` that `ended` does not
    * hold of, and for one outside them that it does not hold of citing one of them. `gone` is
    * `key`, then the rest by key.
    */
  def withdrawn(
      project: Project,
      key: ArtifactKey,
      cascade: Boolean,
      ended: ArtifactHead -> Boolean
  ): Either[String, Withdrawn] = {
    val gone =
      key +: (if (cascade) beneath(project, key).toVector.sortBy(_.value) else Vector())
    val withdrawing = gone.toSet
    val (inside, outside) = project.artifacts.partition(a => withdrawing(a.head.key))
    val citers = outside.filter(_.cites.exists(withdrawing))
    val held = inside.map(a => a.head.key -> a).toMap
    val going = gone.flatMap(held.get)
    def named(as: Vector[Artifact]): String = as.map(_.head.key.value).sorted.mkString(", ")
    if (inside.isEmpty)
      Left(s"project ${ProjectKey.value(project.key)} has no artifact ${key.value}")
    else if (!cascade)
      referrers(key, outside) match {
        case Vector() => Right(remainder(project, going, outside, Vector()))
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
      else Right(remainder(project, going, outside, uncite(citers, withdrawing)))
    }
  }

  /** What [[swept]] leaves: `withdrawn`, its `gone` and `uncited` by key; and `stayed`, by key,
    * each artifact in `due` that `ended` held of and that stays, and why.
    */
  final case class Swept(withdrawn: Withdrawn, stayed: Vector[(artifact: Artifact, why: Stay)])
      extends caps.Pure

  /** Why an artifact a sweep would withdraw stays. */
  enum Stay extends caps.Pure {
    case Claimed(holder: ClaimHolder)

    /** `first` and `rest`, by key: the artifacts that are claimed or not ended citing it. */
    case Cited(first: ArtifactHead, rest: Vector[ArtifactHead])

    /** `child`, under it by parent, stays: of those that do, the first by key. */
    case Above(child: ArtifactHead)
  }

  /** `project` swept: `withdrawn`, the largest set of the artifacts in `due` that `ended` holds
    * of and `claimed` lacks, such that no artifact that is claimed or that `ended` does not hold
    * of cites one of them, and every artifact under one by parent is in it. Each artifact left
    * that cites one of them is rewritten not to, in its head and its body, the words of each link
    * kept. Every other artifact in `due` that `ended` holds of stays, with the first reason that
    * holds: claimed, cited, then above one that stays.
    */
  def swept(
      project: Project,
      due: Set[ArtifactKey],
      ended: ArtifactHead -> Boolean,
      claimed: Map[ArtifactKey, ClaimHolder]
  ): Swept = {
    val byKey = project.artifacts.sortBy(_.head.key.value)
    val children = byKey.flatMap(a => a.head.parent.map(_ -> a)).groupMap(_._1)(_._2)
    val citedBy = byKey
      .filter(a => !ended(a.head) || claimed.contains(a.head.key))
      .flatMap(a => a.cites.map(_ -> a.head))
      .groupMap(_._1)(_._2)
    def asked(a: Artifact): Boolean = due(a.head.key) && ended(a.head)
    // Each step down is to a child, and parents form no cycle, so the two end.
    def stay(a: Artifact): Option[Stay] = {
      val k = a.head.key
      claimed
        .get(k)
        .map(Stay.Claimed(_))
        .orElse(citedBy.get(k).collect { case first +: rest => Stay.Cited(first, rest) })
        .orElse(children.getOrElse(k, Vector()).find(c => !goes(c)).map(c => Stay.Above(c.head)))
    }
    def goes(a: Artifact): Boolean = asked(a) && stay(a).isEmpty
    val fates = byKey.filter(asked).map(a => a -> stay(a))
    val gone = fates.collect { case (a, None) => a }
    val going = gone.map(_.head.key).toSet
    val outside = project.artifacts.filterNot(a => going(a.head.key))
    Swept(
      remainder(project, gone, outside, uncite(outside.filter(_.cites.exists(going)), going)),
      fates.collect { case (a, Some(why)) => (artifact = a, why = why) }
    )
  }

  /** The keys of every artifact of `project` under `key` by parent, at any depth. */
  private def beneath(project: Project, key: ArtifactKey): Set[ArtifactKey] = {
    val children = project.artifacts.groupMap(_.head.parent)(_.head.key)
    @tailrec def under(frontier: Vector[ArtifactKey], found: Set[ArtifactKey]): Set[ArtifactKey] =
      frontier.flatMap(k => children.getOrElse(Some(k), Vector.empty)) match {
        case Vector() => found
        case next => under(next, found ++ next)
      }
    under(Vector(key), Set.empty)
  }

  /** Each of `citers`, by key, rewritten not to cite any of `gone`, in its head and its body. */
  private def uncite(citers: Vector[Artifact], gone: Set[ArtifactKey]): Vector[Artifact] =
    citers
      .map(a =>
        Artifact(a.head.copy(cites = a.head.cites.filterNot(gone)), Body.uncited(a.body, gone))
      )
      .sortBy(_.head.key.value)

  /** `outside` as `project`, each of `uncited` in place of the artifact of its key. Its callers
    * pass an `outside` holding no artifact whose parent is in `gone`, and an `uncited` holding
    * every artifact of `outside` citing one of `gone`.
    */
  private def remainder(
      project: Project,
      gone: Vector[Artifact],
      outside: Vector[Artifact],
      uncited: Vector[Artifact]
  ): Withdrawn = {
    val rewritten = uncited.map(a => a.head.key -> a).toMap
    // Built without `of`: `outside` is a subset of a project, so its keys are distinct, its
    // parents form no cycle, and none is outside it (no parent is in `gone`); and, with each
    // citer of `gone` rewritten not to cite it, every citation is of an artifact still in it.
    Withdrawn(
      new Project(project.key, project.pin, outside.map(a => rewritten.getOrElse(a.head.key, a))),
      gone,
      uncited
    )
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
          .map(p =>
            k -> s"${k.value}'s parent ${p.value} is no artifact of it${ArtifactKey.near(p, keys)}"
          ) ++
        links
          .filterNot(keys.contains)
          .map(c =>
            k -> s"${k.value} cites ${c.value}, no artifact of it${ArtifactKey.near(c, keys)}"
          ) ++
        Option.when(cycles(k, Set(k)))(k -> s"${k.value} is its own ancestor")
    }.distinct
  }
}
