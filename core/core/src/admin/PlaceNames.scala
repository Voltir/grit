package grit.core.admin

import grit.core.place.Place

/** How people write, where a command names a place, the places a service shares with grit: as
  * the service links or names them (a shared folder's link, say), never as grit writes a place.
  */
trait PlaceNames extends caps.Pure {

  /** The place `word` names; `None` when it names none of these. */
  def place(word: String): Option[Place]
}

object PlaceNames {

  /** Names no place. */
  val None: PlaceNames = new PlaceNames {
    def place(word: String): Option[Place] = Option.empty
  }

  /** The place the first of `names` that names `word` gives. */
  def of(names: Vector[PlaceNames]): PlaceNames = new PlaceNames {
    def place(word: String): Option[Place] = names.iterator.flatMap(_.place(word)).nextOption()
  }
}
