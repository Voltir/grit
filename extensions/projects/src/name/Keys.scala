package grit.projects.name

import grit.core.id.{Sha256, Slug}

/** A project's key: the first part of each of its artifacts' keys. */
opaque type ProjectKey = String

object ProjectKey {

  /** 40: the most characters a project's key holds. */
  val Longest: Int = 40

  /** `s` as a project's key, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, ProjectKey] =
    Slug.check("a project's key", s, Some(Longest), leadingDigit = true)
  def value(k: ProjectKey): String = k
}

/** An artifact's part of its key, `<artifact>` in `<project>/<artifact>`: the same name in any
  * project.
  */
opaque type LocalKey = String

object LocalKey {

  /** 80: the most characters a local key holds. */
  val Longest: Int = 80

  /** `s` as a local key, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, LocalKey] =
    Slug.check("an artifact's part of its key", s, Some(Longest), leadingDigit = true)

  def value(k: LocalKey): String = k

  /** 3: the most keys [[nearest]] gives. */
  val NearestMost: Int = 3

  /** At most [[NearestMost]] of `among`, nearest `asked` first, never `asked` itself: first
    * those that extend `asked` or that it extends, the closer in length first; then those within
    * an edit distance of a third of `asked`'s length, rounded down, the smaller first; ties by
    * key. Empty when none is that near.
    */
  def nearest(asked: LocalKey, among: Vector[LocalKey]): Vector[LocalKey] = {
    val others = among.distinct.filterNot(_ == asked)
    val extending = others
      .filter(k => k.startsWith(asked) || asked.startsWith(k))
      .sortBy(k => ((k.length - asked.length).abs, k))
    val within = asked.length / 3
    val close = others
      .filterNot(extending.contains)
      .map(k => k -> distance(asked, k))
      .filter(_._2 <= within)
      .sortBy((k, d) => (d, k))
      .map(_._1)
    (extending ++ close).take(NearestMost)
  }

  /** The Levenshtein distance between `a` and `b`. */
  private def distance(a: String, b: String): Int = {
    // The table's rows, one per character of `a`: each cell from the one left of it (the second of
    // `done`), the one above (`up`) and the one above-left (`diagonal`).
    val first: Vector[Int] = Vector.range(0, b.length + 1)
    val last = a.toVector.zipWithIndex.foldLeft(first) { (row: Vector[Int], at: (Char, Int)) =>
      val pairs: Vector[(Char, (Int, Int))] = b.toVector.zip(row.zip(row.drop(1)))
      pairs
        .foldLeft((Vector(at._2 + 1), at._2 + 1)) {
          (done: (Vector[Int], Int), cell: (Char, (Int, Int))) =>
            val (diagonal, up) = cell._2
            val swap = if (cell._1 == at._1) 0 else 1
            val here = math.min(math.min(done._2 + 1, up + 1), diagonal + swap)
            (done._1 :+ here, here)
        }
        ._1
    }
    last.lastOption.getOrElse(a.length)
  }
}

/** An artifact's key, `<project>/<artifact>`: its id, never its title, so a rename moves no
  * reference to it.
  */
final case class ArtifactKey private (project: ProjectKey, local: LocalKey) extends caps.Pure {
  def value: String = s"$project/$local"
}

object ArtifactKey {

  /** The key of the artifact `local` of `project`, or why not, as [[LocalKey.of]] refuses. */
  def of(project: ProjectKey, local: String): Either[String, ArtifactKey] =
    LocalKey.of(local).map(new ArtifactKey(project, _))

  /** ` (nearest: <key>, <key>)`, naming the keys of `among` in `asked`'s project that
    * [[LocalKey.nearest]] gives for it, nearest first; empty when it gives none.
    */
  def near(asked: ArtifactKey, among: Vector[ArtifactKey]): String =
    LocalKey.nearest(asked.local, among.filter(_.project == asked.project).map(_.local)) match {
      case Vector() => ""
      case some => some.map(l => s"${asked.project}/$l").mkString(" (nearest: ", ", ", ")")
    }

  /** `s` as [[ArtifactKey.value]] writes a key, or why not: no `/`, or a part
    * [[ProjectKey.of]] or [[of]] refuses.
    */
  def read(s: String): Either[String, ArtifactKey] = s.indexOf('/') match {
    case -1 => Left(s"an artifact's key has no '/': $s")
    case i => ProjectKey.of(s.take(i)).flatMap(of(_, s.drop(i + 1)))
  }
}

/** The method version a project pinned: its name, its version and the digest it had then. */
final case class Pin private (method: MethodName, version: Int, digest: Sha256) extends caps.Pure

object Pin {

  /** These as a pin, or why not: `version` under 1. */
  def of(method: MethodName, version: Int, digest: Sha256): Either[String, Pin] =
    if (version < 1) Left(s"a pinned version is under 1: $version")
    else Right(new Pin(method, version, digest))

  /** These as a pin, the digest as written ([[Sha256.read]]), or why not: `version` under 1, or
    * `digest` not 64 lowercase hex digits.
    */
  def read(method: MethodName, version: Int, digest: String): Either[String, Pin] =
    Sha256
      .read(digest)
      .toRight(s"a pinned digest is not 64 lowercase hex digits: $digest")
      .flatMap(of(method, version, _))
}
