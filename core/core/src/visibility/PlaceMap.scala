package grit.core.visibility

import grit.core.place.Place

/** Values declared at places, read for a place by the deepest declared place it is or lies
  * within.
  */
private[visibility] final case class PlaceMap[+V] private (declared: Map[Place, V]) {

  /** The value at the deepest declared place `place` is or lies within, or `None`. */
  def covering(place: Place): Option[V] =
    declared.filter((p, _) => place.within(p)).maxByOption((p, _) => p.segments.size).map(_._2)
}

private[visibility] object PlaceMap {

  /** No place declared. */
  val Empty: PlaceMap[Nothing] = new PlaceMap(Map.empty)

  /** `pairs` as a map, or the first place declared twice. */
  def of[V](pairs: Vector[(Place, V)]): Either[Place, PlaceMap[V]] = {
    val places = pairs.map(_._1)
    places.diff(places.distinct).headOption.toLeft(new PlaceMap(pairs.toMap))
  }
}
