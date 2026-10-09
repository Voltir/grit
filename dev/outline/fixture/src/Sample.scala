package grit.outline.fixture

/** A transaction, as a capability. */
trait Tx extends caps.SharedCapability

/** Where a chain starts: it takes a [[B]]. */
trait A {

  /** `b` kept, in `tx`. */
  def keep(b: B)(using Tx^): Either[String, Int]

  /** `f` run over `b`'s value. */
  def run(f: Int -> Tx^ ?-> Int, b: B): Int
}

/** A middle link: it holds a [[C]]. */
final case class B(c: C, note: String)

/** The chain's end. */
final case class C(n: Int)

/** Limits, built only through [[Limits.of]]. */
final case class Limits private (asks: Int, calls: Int)

object Limits {

  /** Two limits, or why not. */
  def of(asks: Int, calls: Int): Either[String, Limits] = of(asks, calls, 0)

  /** Three limits, or why not: any negative. */
  def of(asks: Int, calls: Int, extra: Int): Either[String, Limits] =
    if (asks < 0 || calls < 0 || extra < 0) { Left("negative") }
    else { Right(new Limits(asks, calls + hidden)) }

  private def hidden: Int = 0
}

/** A colour. */
enum Colour {

  /** Red. */
  case Red

  /** A mix, by weight. */
  case Mix(weight: Double)
}

object Ids {

  /** An id, opaque outside [[Ids]]. */
  opaque type Id = String
}

/** A quiver, so a last segment shared by two owners is found under both. */
trait Quiver

/** A class whose constructor parameter has a default: the compiler adds a synthetic default-argument method for it. */
final case class Defaulted(n: Int = 1)

object Defaulted {

  /** The companion, which holds the synthetic default-argument method beside its own member. */
  def zero: Defaulted = Defaulted()
}
