package grit.outline.fixture

/** An almanac, whose members' docs are cut to their first sentence when it is shown. The type's own doc
  * is printed whole.
  */
object Almanac {

  /** The years it covers, in order.
    * A second line, cut when the almanac is shown.
    */
  def years: Vector[Int] = Vector.empty

  /** How many tides it lists, a count whose first sentence runs past
    * its first line. Then more.
    */
  def tides: Int = 0

  /** One line, two sentences. Both kept. */
  def moons: Int = 1

  /** Its first sentence ends here. The line holds more,
    * and the doc runs on.
    */
  def stars: Int = 2

  def plain: Int = 3
}
