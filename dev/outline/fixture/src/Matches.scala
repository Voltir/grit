package grit.outline.fixture

/** Pattern and construction sites, each on a line of its own. */
object Matches {

  /** `b`'s `c`, by its extractor. */
  def cOf(b: B): C = b match {
    case B(c, _) => c
  }

  /** Whether `x` is a [[C]], by its type. */
  def isC(x: Any): Boolean = x match {
    case _: C => true
    case _ => false
  }

  /** A colour's weight, by an enum case's extractor. */
  def weight(c: Colour): Double = c match {
    case Colour.Mix(w) => w
    case Colour.Red => 0.0
  }

  /** A mix, built. */
  def mix: Colour = Colour.Mix(1.0)

  /** A [[C]], built with `new`. */
  def made: C = new C(1)

  /** A [[B]], built: a construction, not a pattern. */
  def built: B = B(made, "n")
}
