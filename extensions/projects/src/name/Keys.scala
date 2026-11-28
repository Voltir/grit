package grit.projects.name

import grit.core.id.Slug
import grit.std.near.Near

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

/** A name an artifact may carry beside its id, held by at most one current artifact of its
  * project, and changed without moving any reference to it.
  */
opaque type Handle = String

object Handle {

  /** 80: the most characters a handle holds. */
  val Longest: Int = 80

  /** `s` as a handle, or why not: not a slug ([[Slug.check]]), not led by a letter, or over
    * [[Longest]] characters.
    */
  def of(s: String): Either[String, Handle] =
    Slug.check("a handle", s, Some(Longest), leadingDigit = false)

  def value(h: Handle): String = h

  /** ` (nearest: <handle>, <handle>)`, naming the handles of `among` that [[nearest]] gives for
    * `asked`, nearest first; empty when it gives none.
    */
  def near(asked: Handle, among: Vector[Handle]): String =
    nearest(asked, among) match {
      case Vector() => ""
      case some => some.mkString(" (nearest: ", ", ", ")")
    }

  /** 3: the most handles [[nearest]] gives. */
  val NearestMost: Int = 3

  /** At most [[NearestMost]] of `among`, nearest `asked` first, as [[Near.nearest]] ranks them. */
  def nearest(asked: Handle, among: Vector[Handle]): Vector[Handle] =
    Near.nearest(asked, among, NearestMost)
}

/** An artifact's number in its project. */
opaque type ArtifactNumber = Int

object ArtifactNumber {

  /** `n` as a number, or why not: below 1. */
  def of(n: Int): Either[String, ArtifactNumber] =
    if (n < 1) Left(s"an artifact's number is below 1: $n") else Right(n)

  /** 1: the number a project's first artifact takes. */
  val First: ArtifactNumber = 1
  def value(n: ArtifactNumber): Int = n

  /** The number after `n`. */
  def after(n: ArtifactNumber): ArtifactNumber = n + 1

  /** `s`, decimal digits alone, as a number, or why not: other characters, below 1, or past
    * `Int`'s range.
    */
  def read(s: String): Either[String, ArtifactNumber] =
    if (s.isEmpty || !s.forall(c => c >= '0' && c <= '9'))
      Left(s"an artifact's number is digits alone: $s")
    else s.toIntOption.toRight(s"an artifact's number is too large: $s").flatMap(of)

  given Ordering[ArtifactNumber] = Ordering.Int
}

/** An artifact's id, `<project>/<n>`: minted when it is first kept, never reused or changed. */
final case class ArtifactKey private (project: ProjectKey, number: ArtifactNumber)
    extends caps.Pure {
  def value: String = s"$project/$number"
}

object ArtifactKey {

  /** By project, then by number. */
  given Ordering[ArtifactKey] = Ordering.by(k => (ProjectKey.value(k.project), k.number))

  /** The id of artifact `number` of `project`. */
  def of(project: ProjectKey, number: ArtifactNumber): ArtifactKey =
    new ArtifactKey(project, number)

  /** `s` as [[ArtifactKey.value]] writes an id, or why not: no `/`, a project [[ProjectKey.of]]
    * refuses, or a number [[ArtifactNumber.read]] refuses.
    */
  def read(s: String): Either[String, ArtifactKey] = s.indexOf('/') match {
    case -1 => Left(s"an artifact's id has no '/': $s")
    case i =>
      for {
        project <- ProjectKey.of(s.take(i))
        number <- ArtifactNumber.read(s.drop(i + 1))
      } yield new ArtifactKey(project, number)
  }
}

/** An artifact as a caller or a body names it: by its id, or by a name, which resolves as a
  * current handle or, in the commands, as a current artifact's store key.
  */
enum ArtifactRef extends caps.Pure {
  case Id(key: ArtifactKey)
  case Named(override val project: ProjectKey, name: Handle)

  /** The project it names an artifact of. */
  def project: ProjectKey = this match {
    case Id(k) => k.project
    case Named(p, _) => p
  }

  /** As [[ArtifactRef.read]] reads it back. */
  def value: String = this match {
    case Id(k) => k.value
    case Named(p, h) => s"$p/$h"
  }
}

object ArtifactRef {

  /** `s`, `<project>/<n>` or `<project>/<name>`, as a reference, or why not: no `/`, a project
    * [[ProjectKey.of]] refuses, or after the `/`, led by a digit, a number [[ArtifactNumber.read]]
    * refuses, else a handle [[Handle.of]] refuses.
    */
  def read(s: String): Either[String, ArtifactRef] = s.indexOf('/') match {
    case -1 => Left(s"an artifact's key has no '/': $s")
    case i =>
      val local = s.drop(i + 1)
      ProjectKey.of(s.take(i)).flatMap { p =>
        if (local.headOption.exists(_.isDigit))
          ArtifactNumber.read(local).map(n => Id(ArtifactKey.of(p, n)))
        else Handle.of(local).map(Named(p, _))
      }
  }

  /** `ref`'s id, or why not: it names a handle, which only its project resolves
    * (`Project.resolve`). What resolves a text's links where every link is by id.
    */
  def id(ref: ArtifactRef): Either[String, ArtifactKey] = ref match {
    case Id(k) => Right(k)
    case named => Left(s"${named.value} names no id: only its project tells which artifact it is")
  }
}
