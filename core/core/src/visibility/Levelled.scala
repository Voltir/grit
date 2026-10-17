package grit.core.visibility

import scala.collection.immutable.SortedSet

/* The one lattice both axes are instances of: a level on a fixed chain and a set of
 * compartments. One dominates another when its level is at least the other's and it holds every
 * compartment of the other; join takes the higher level and the union, meet the lower and the
 * intersection. `Label` and `Integrity` are each an opaque type over it at their own levels, so
 * neither is passed where the other is wanted. Not a case class and not a set, so a type test
 * outside this package finds no structure to take apart. */
private[visibility] final class Levelled[L <: reflect.Enum & caps.Pure](
    val level: L,
    val compartments: SortedSet[Mark]
)(using val levels: Levels[L])
    extends caps.Pure {

  def dominates(other: Levelled[L]): Boolean =
    level.ordinal >= other.level.ordinal && other.compartments.subsetOf(compartments)

  def join(other: Levelled[L]): Levelled[L] =
    new Levelled(
      if (level.ordinal >= other.level.ordinal) level else other.level,
      compartments ++ other.compartments
    )

  def meet(other: Levelled[L]): Levelled[L] =
    new Levelled(
      if (level.ordinal <= other.level.ordinal) level else other.level,
      compartments.intersect(other.compartments)
    )

  def written: String =
    (levels.name(level) +: compartments.toVector.map(Mark.name)).mkString("+")

  /* The unmapped mark is read as what no one has labelled yet, never named. */
  def shown: String = {
    val named = compartments.toVector.filter(_ != Mark.Unmapped).map(Mark.name)
    val in = if (named.isEmpty) "" else named.mkString(", in: {", ", ", "}")
    val unlabelled =
      if (compartments.contains(Mark.Unmapped)) ", and what no one has labelled yet" else ""
    s"[${levels.shownAs}: ${levels.name(level)}$in$unlabelled]"
  }

  override def equals(that: Any): Boolean = that match {
    case r: Levelled[?] => level == r.level && compartments == r.compartments
    case _ => false
  }
  override def hashCode: Int = (level, compartments).##
  override def toString: String = written
}

private[visibility] object Levelled {

  def at[L <: reflect.Enum & caps.Pure](level: L, compartments: Seq[Mark])(using
      Levels[L]
  ): Levelled[L] =
    new Levelled(level, SortedSet.from(compartments))

  def read[L <: reflect.Enum & caps.Pure](text: String)(using
      levels: Levels[L]
  ): Either[String, Levelled[L]] =
    text.split('+').toVector match {
      case head +: rest =>
        for {
          level <- levels.all
            .find(levels.name(_) == head)
            .toRight(s"$text names no ${levels.refusedAs} first: $head")
          compartments <- rest.foldLeft[Either[String, Vector[Mark]]](Right(Vector.empty)) {
            (acc, c) => acc.flatMap(cs => Mark.read(c).map(cs :+ _))
          }
        } yield at(level, compartments)
      case _ => Left(s"$text names no ${levels.refusedAs}")
    }
}

/* A chain's levels, lowest first, and the words its written and shown forms use: changing a
 * name changes every written value at these levels. */
private[visibility] trait Levels[L] extends caps.Pure {
  def all: Vector[L]
  def name(level: L): String
  def shownAs: String
  def refusedAs: String
}
