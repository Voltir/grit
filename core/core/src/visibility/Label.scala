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
    } else if (LabelAtoms.level(name).isDefined) {
      Left(s"$name is a level's name, never a compartment's")
    } else Right(name)

  /** What a mapping could not place. Every deployment declares it. */
  val Unmapped: Compartment = "unmapped"

  /** Its name, the atom it is stored as, for this package's own forms. */
  private[visibility] def name(c: Compartment): String = c

  private[visibility] given Ordering[Compartment] = Ordering.String
}

/** What a thing is classified at: a level and some compartments. One label dominates another
  * when its level is at least the other's and it holds every compartment of the other.
  * Opaque: a label is compared, joined, met and passed to core as a key, never taken apart.
  */
opaque type Label = Label.Repr

object Label {

  /* Not a case class and not a set, so a type test outside this file finds no structure to
   * take apart: `Repr` cannot be named there, and it is no `Product` or `Set`. A level and its
   * compartments, rather than the atoms they store as, so a compartment can never be mistaken
   * for a level's atom (LabelAtoms.of). */
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
    (LabelAtoms.name(l.level) +: l.compartments.toVector.map(Compartment.name)).mkString("+")

  /** The label `text` writes, or why not. */
  def read(text: String): Either[String, Label] =
    text.split('+').toVector match {
      case head +: rest =>
        for {
          level <- LabelAtoms.level(head).toRight(s"$text names no level first: $head")
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

/** A label as atoms, for the code that stores and checks labels (`grit.core.visibility`,
  * `grit.dbos`) and for nothing else (enola-intent.yaml). A level is the atoms of itself and
  * each level below it but [[Level.Public]]; a compartment is its own atom.
  */
object LabelAtoms {

  /* Each level's atom, and so its stored name: changing one changes every stored label. */
  private val Ranked: Vector[(Level, String)] =
    Vector(
      Level.Internal -> "internal",
      Level.Confidential -> "confidential",
      Level.Restricted -> "restricted"
    )

  private val PublicName = "public"

  private[visibility] def name(level: Level): String =
    Ranked.collectFirst { case (`level`, n) => n }.getOrElse(PublicName)

  private[visibility] def level(name: String): Option[Level] =
    if (name == PublicName) Some(Level.Public) else Ranked.collectFirst { case (l, `name`) => l }

  /** `l`'s atoms, sorted and distinct. */
  def atoms(l: Label): Vector[String] =
    (Ranked.takeWhile(_._1.ordinal <= Label.level(l).ordinal).map(_._2) ++ Label
      .compartments(l)
      .map(Compartment.name)).sorted

  /** The label these atoms make: the highest level whose atoms they all hold, and each other
    * atom that names a compartment ([[Compartment.of]]) as that compartment. Total, and failing
    * closed: a level's atom without those below it (`confidential` without `internal`) raises
    * the level to its own and adds [[Compartment.Unmapped]], as does an atom naming neither a
    * level nor a compartment.
    */
  def of(atoms: Vector[String]): Label = {
    val held = atoms.toSet
    val whole = Ranked.takeWhile((_, a) => held(a)).lastOption.fold(Level.Public)(_._1)
    val stray = Ranked.filter((l, a) => held(a) && l.ordinal > whole.ordinal)
    val others = held -- Ranked.map(_._2)
    val named = others.toVector.flatMap(Compartment.of(_).toOption)
    val unplaced = stray.nonEmpty || named.size < others.size
    val level = stray.lastOption.fold(whole)(_._1)
    Label.at(level, (if (unplaced) named :+ Compartment.Unmapped else named)*)
  }
}
