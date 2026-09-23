package grit.core

/** A count of model tokens. Opaque so it cannot be mixed with other counts. */
opaque type Tokens = Long

object Tokens {
  val Zero: Tokens = 0L
  def apply(value: Long): Tokens = value
  def value(t: Tokens): Long = t

  extension (t: Tokens) {
    def +(other: Tokens): Tokens = t + other
  }
}
