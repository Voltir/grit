package grit.core.visibility

import scala.collection.immutable.SortedSet

import grit.core.place.Service

/** How far a thing may be believed to steer an effect (Biba, ADR 0036): a level and the
  * compartments it is endorsed in. One dominates another when its level is at least the
  * other's and it holds every compartment of the other. What is made from several is believed
  * at their meet. Opaque, as `Label` is.
  */
opaque type Integrity = Integrity.Repr

object Integrity {

  /* Not a case class and not a set, so a type test outside this file finds no structure to
   * take apart, as for `Label.Repr`. */
  private[visibility] final class Repr(val level: Level, val compartments: SortedSet[Compartment]) {
    override def equals(that: Any): Boolean = that match {
      case r: Repr => level == r.level && compartments == r.compartments
      case _ => false
    }
    override def hashCode: Int = (level, compartments).##
    override def toString: String = written(this)
  }

  /** grit's integrity levels (Biba), lowest first; not the confidentiality `Level` beside it.
    * Each unlocks what those below it do and its own.
    */
  enum Level {

    /** Content no source vouches for (a relaying service's results, an undeclared one's).
      * Unlocks nothing.
      */
    case Untrusted

    /** An unattested account's words, a guest's. May be kept and replied to. */
    case Outside

    /** An attested member of a realm. May drive a write out of grit within the realm. */
    case Member

    /** An administrator, or a steward in their compartments. May set standing policy,
      * relabel rooms and declare schedules.
      */
    case Authority
  }

  /** [[Level.Untrusted]], no compartment: the least. */
  val Untrusted: Integrity = at(Level.Untrusted)

  /** [[Level.Outside]], no compartment. */
  val Outside: Integrity = at(Level.Outside)

  /** [[Level.Member]], no compartment. */
  val Member: Integrity = at(Level.Member)

  /** [[Level.Authority]], no compartment. */
  val Authority: Integrity = at(Level.Authority)

  /** grit's own text: [[Level.Authority]] in every one of `compartments`; the meet's identity. */
  def top(compartments: Compartments): Integrity =
    at(Level.Authority, compartments.declared.toVector*)

  /** `level` in `compartments`, read as the compartments it is *endorsed in*: the reverse of
    * `Label.at`'s reading, so the usual value names none.
    */
  def at(level: Level, compartments: Compartment*): Integrity =
    new Repr(level, SortedSet.from(compartments))

  extension (i: Integrity) {
    def dominates(other: Integrity): Boolean =
      i.level.ordinal >= other.level.ordinal && other.compartments.subsetOf(i.compartments)

    /** Whether it is believed below [[Outside]]: at [[Level.Untrusted]], in any compartments. */
    def belowOutside: Boolean = !i.dominates(Outside)

    /** The greatest integrity both dominate: what is made from both. */
    def meet(other: Integrity): Integrity =
      new Repr(
        if (i.level.ordinal <= other.level.ordinal) i.level else other.level,
        i.compartments.intersect(other.compartments)
      )

    /** The least integrity dominating both. */
    def join(other: Integrity): Integrity =
      new Repr(
        if (i.level.ordinal >= other.level.ordinal) i.level else other.level,
        i.compartments ++ other.compartments
      )
  }

  given CanEqual[Integrity, Integrity] = CanEqual.derived

  /* Its level's name, as the written form holds it: changing one changes every written
   * integrity. */
  private def name(level: Level): String = level match {
    case Level.Untrusted => "untrusted"
    case Level.Outside => "outside"
    case Level.Member => "member"
    case Level.Authority => "authority"
  }

  /** Its written form, stable across releases; parsed by [[read]]. */
  def written(i: Integrity): String =
    (name(i.level) +: i.compartments.toVector.map(Compartment.name)).mkString("+")

  /** Its form for people and the model, e.g. `[integrity: member]`. Never parsed. */
  def shown(i: Integrity): String = {
    val in =
      if (i.compartments.isEmpty) ""
      else i.compartments.toVector.map(Compartment.name).mkString(", in: {", ", ", "}")
    s"[integrity: ${name(i.level)}$in]"
  }

  /** The integrity `text` writes, or why not. */
  def read(text: String): Either[String, Integrity] =
    text.split('+').toVector match {
      case head +: rest =>
        for {
          level <- Level.values
            .find(name(_) == head)
            .toRight(s"$text names no integrity level first: $head")
          compartments <- rest.foldLeft[Either[String, Vector[Compartment]]](Right(Vector.empty)) {
            (acc, c) => acc.flatMap(cs => Compartment.of(c).map(cs :+ _))
          }
        } yield at(level, compartments*)
      case _ => Left(s"$text names no integrity level")
    }

  /** `i`'s compartments, in order, for this package's own checks. */
  private[visibility] def compartments(i: Integrity): Vector[Compartment] = i.compartments.toVector

  /** `i`'s level, for this package's own checks. */
  private[visibility] def level(i: Integrity): Level = i.level
}

/** The deployment declares what `service`'s results are believed at: a service that relays
  * others' text (mail, the web) declares [[Integrity.Untrusted]]. An undeclared service's are
  * `Untrusted`.
  */
final case class Output(service: Service, integrity: Integrity) derives CanEqual

/** An integrity as the level and compartments it is stored as (`grit.integrity_labels`), for the
  * code that stores and checks integrity (`grit.core.visibility`, `grit.dbos`) and for nothing
  * else (enola-intent.yaml).
  */
object IntegrityParts {

  /** `i`'s level as stored: its rank on core's scale, from 0 for [[Integrity.Level.Untrusted]]. */
  def rank(i: Integrity): Int = Integrity.level(i).ordinal

  /** `i`'s compartments' names, distinct and sorted bytewise: the form
    * `grit.canonical_compartments` keeps.
    */
  def compartments(i: Integrity): Vector[String] =
    Integrity.compartments(i).map(Compartment.name)

  /** The integrity stored as `rank` and `compartments`. Total, and failing closed: a rank naming
    * no level reads as [[Integrity.Level.Untrusted]], and a name that is no compartment's
    * ([[Compartment.of]]) is dropped, so it is endorsed in none it cannot name.
    */
  def of(rank: Int, compartments: Vector[String]): Integrity =
    Integrity.at(
      Integrity.Level.values.lift(rank).getOrElse(Integrity.Level.Untrusted),
      compartments.flatMap(Compartment.of(_).toOption)*
    )
}
