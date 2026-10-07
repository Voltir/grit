package grit.core.visibility

import scala.collection.immutable.SortedSet

/** A named area a deployment declares (a team, a client, a project): a thing in it is read
  * only by a clearance holding it, at whatever level.
  */
opaque type Compartment = String

object Compartment {

  private val Shape = "[a-z0-9-]{1,32}".r

  /** `name` as a compartment: lowercase letters, digits and `-`, 1 to 32 of them, and no
    * level's name; or why not.
    */
  def of(name: String): Either[String, Compartment] =
    if (!Shape.matches(name)) {
      Left(s"$name is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them")
    } else if (Level.named(name).isDefined) {
      Left(s"$name is a level's name, never a compartment's")
    } else Right(name)

  /** What a mapping could not place. Every deployment declares it. */
  val Unmapped: Compartment = "unmapped"

  /** Its name, as [[of]] took it: what a person reads in a refusal naming it, and what it is
    * stored as. It says nothing of any label's structure: no public operation takes a
    * compartment out of a label.
    */
  def name(c: Compartment): String = c

  private[visibility] given Ordering[Compartment] = Ordering.String
}

/** What a thing is classified at: a level and some compartments. One label dominates another
  * when its level is at least the other's and it holds every compartment of the other.
  * Opaque: a label is compared, joined, met and passed to core as a key, never taken apart.
  */
opaque type Label = Label.Repr

object Label {

  /* Not a case class and not a set, so a type test outside this file finds no structure to
   * take apart: `Repr` cannot be named there, and it is no `Product` or `Set`. */
  private[visibility] final class Repr(val level: Level, val compartments: SortedSet[Compartment]) {
    override def equals(that: Any): Boolean = that match {
      case r: Repr => level == r.level && compartments == r.compartments
      case _ => false
    }
    override def hashCode: Int = (level, compartments).##
    override def toString: String = written(this)
  }

  /** The bottom: [[Level.Public]], no compartment. Every label dominates it. */
  val Public: Label = new Repr(Level.Public, SortedSet.empty)

  def at(level: Level, compartments: Compartment*): Label =
    new Repr(level, SortedSet.from(compartments))

  extension (l: Label) {
    def dominates(other: Label): Boolean =
      l.level.ordinal >= other.level.ordinal && other.compartments.subsetOf(l.compartments)

    /** The least label dominating both: what is made from both is kept at least here. */
    def join(other: Label): Label =
      new Repr(
        if (l.level.ordinal >= other.level.ordinal) l.level else other.level,
        l.compartments ++ other.compartments
      )

    /** The greatest label both dominate. */
    def meet(other: Label): Label =
      new Repr(
        if (l.level.ordinal <= other.level.ordinal) l.level else other.level,
        l.compartments.intersect(other.compartments)
      )
  }

  given CanEqual[Label, Label] = CanEqual.derived

  /** Its written form, stable across releases: what logs, recorded step outputs and files
    * outside the database hold. Parseable by [[read]], so a label is not secret from whoever
    * holds its written form; opacity buys representation independence, not secrecy.
    */
  def written(l: Label): String =
    (Level.name(l.level) +: l.compartments.toVector.map(Compartment.name)).mkString("+")

  /** The label `text` writes, or why not. */
  def read(text: String): Either[String, Label] =
    text.split('+').toVector match {
      case head +: rest =>
        for {
          level <- Level.named(head).toRight(s"$text names no level first: $head")
          compartments <- rest.foldLeft[Either[String, Vector[Compartment]]](Right(Vector.empty)) {
            (acc, c) => acc.flatMap(cs => Compartment.of(c).map(cs :+ _))
          }
        } yield at(level, compartments*)
      case _ => Left(s"$text names no level")
    }

  /** `l`'s level, for this package's own checks. */
  private[visibility] def level(l: Label): Level = l.level

  /** `l`'s compartments, in order, for this package's own checks. */
  private[visibility] def compartments(l: Label): Vector[Compartment] = l.compartments.toVector
}

/** A label as the level and compartments it is stored as (`grit.labels`), for the code that
  * stores and checks labels (`grit.core.visibility`, `grit.dbos`) and for nothing else
  * (enola-intent.yaml).
  */
object LabelParts {

  /** `l`'s level as stored: its rank on core's scale, from 0 for [[Level.Public]]. */
  def rank(l: Label): Int = Label.level(l).ordinal

  /** `l`'s compartments' names, distinct and sorted bytewise: the form
    * `grit.canonical_compartments` keeps.
    */
  def compartments(l: Label): Vector[String] = Label.compartments(l).map(Compartment.name)

  /** The label stored as `rank` and `compartments`. Total, and failing closed: a rank naming
    * no level reads as [[Level.Restricted]], and a name that is no compartment's
    * ([[Compartment.of]]) is dropped; either way the label also holds
    * [[Compartment.Unmapped]].
    */
  def of(rank: Int, compartments: Vector[String]): Label = {
    val level = Level.values.lift(rank)
    val named = compartments.flatMap(Compartment.of(_).toOption)
    val placed = Label.at(level.getOrElse(Level.Restricted), named*)
    if (level.nonEmpty && named.size == compartments.size) placed
    else placed.join(Label.at(Level.Public, Compartment.Unmapped))
  }
}
