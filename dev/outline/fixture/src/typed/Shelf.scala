package grit.outline.fixture.typed

/** A trait no code calls: it is only extended and named in types. */
trait Shelf[A] {

  /** The first item, if any: `A` is a type parameter, no definition of its own. */
  def first: Option[A]
}

/** A shelf of numbers: `Shelf` as a parent. */
final class Numbers extends Shelf[Int] {
  def first: Option[Int] = None
}

/** Shelves held and told apart: `Shelf` as a parameter's type, a type argument and a typed pattern. */
object Shelves {

  /** A shelf's label. */
  type Label = String

  /** `s` alone. */
  def one(s: Shelf[?]): Vector[Shelf[?]] = Vector(s)

  /** Whether `x` is a shelf. */
  def isShelf(x: Any): Boolean = x match {
    case s: Shelf[?] => s.first.isEmpty || s.first.nonEmpty
    case _ => false
  }
}

/** Labels, named through their owner: a type selection. */
object Labels {

  /** `l`, unchanged. */
  def of(l: Shelves.Label): Shelves.Label = l
}
