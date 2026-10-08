package grit.core.visibility

import grit.core.place.Place

/** What a mapping made of something it labels. */
enum Labelled {

  /** Placed at `label`. */
  case Mapped(placed: Label)

  /** Not fully placed: `partial` is what could be, and it is kept at `partial` joined with
    * [[Compartment.Unmapped]].
    */
  case Unmapped(partial: Label)

  /** The label it is kept at. */
  def label: Label = this match {
    case Mapped(placed) => placed
    case Unmapped(partial) => partial.join(Label.at(Level.Public, Compartment.Unmapped))
  }
}

/** How a source labels what it brings in: an edge its rooms, a plugin the items it keeps.
  * Decided by declared data and the item alone, never by a call.
  */
trait Labeller[-A] extends caps.Pure {
  def label(item: A): Labelled

  /** The compartments its labels may hold; a deployment not declaring each is refused. A label
    * it returns is admitted against the deployment's declared compartments
    * ([[Compartments.admit]]): one the deployment does not declare is kept at
    * [[Compartment.Unmapped]] instead.
    */
  def requires: Vector[Compartment]
}

/** A deployment's labels for rooms: a room takes the label declared at its own place; else,
  * when its access is reported, `open` for [[RoomAccess.Open]] and unmapped for
  * [[RoomAccess.Invited]]; else the label declared at the longest declared place it is within,
  * or `otherwise`. Which rooms are labelled how, never which rooms may read which.
  */
final case class RoomLabels private (
    declared: Vector[(Place, Label)],
    otherwise: Labelled,
    open: Labelled
) extends Labeller[Room] {

  def label(item: Room): Labelled =
    declared.collectFirst { case (place, l) if place == item.place => Labelled.Mapped(l) } match {
      case Some(own) => own
      case None =>
        item.access match {
          case Some(RoomAccess.Open) => open
          case Some(RoomAccess.Invited) => Labelled.Unmapped(Label.Public)
          case None =>
            declared
              .filter((place, _) => item.place.within(place))
              .maxByOption((place, _) => place.segments.size)
              .fold(otherwise)((_, l) => Labelled.Mapped(l))
        }
    }

  def requires: Vector[Compartment] =
    (declared.map(_._2) :+ otherwise.label :+ open.label).flatMap(Label.compartments).distinct
}

object RoomLabels {

  /** Every room public. */
  val Public: RoomLabels =
    new RoomLabels(Vector.empty, Labelled.Mapped(Label.Public), Labelled.Mapped(Label.Public))

  /** These, or the place declared twice; `open` is `otherwise` when not given. */
  def of(
      declared: Vector[(Place, Label)],
      otherwise: Labelled,
      open: Option[Labelled] = None
  ): Either[Place, RoomLabels] = {
    val places = declared.map(_._1)
    places
      .diff(places.distinct)
      .headOption
      .toLeft(new RoomLabels(declared, otherwise, open.getOrElse(otherwise)))
  }
}
