package grit.core.flow

import java.time.{Instant, LocalDate}

import scala.compiletime.ops.int.{+, `*`}
import scala.deriving.Mirror

/** Evidence that every `T` is written in at most `B` bits, so an endorsed `T` carries at most
  * `B` bits its source chose.
  */
sealed abstract class Bounded[T, B <: Int] private[flow] ()

object Bounded {

  /** The most bits one endorsement's answer may carry, besides whether it has one; two
    * endorsements carry twice as many.
    */
  type K = 256

  private final class Is[T, B <: Int] extends Bounded[T, B]

  given boolean: Bounded[Boolean, 1] = Is()
  given int: Bounded[Int, 32] = Is()
  given long: Bounded[Long, 64] = Is()
  given instant: Bounded[Instant, 96] = Is()
  given localDate: Bounded[LocalDate, 32] = Is()

  /** An absent value costs one bit more than a present one. */
  given option[T, B <: Int](using Bounded[T, B]): Bounded[Option[T], B + 1] = Is()

  given pair[A, BA <: Int, C, BC <: Int](using
      Bounded[A, BA],
      Bounded[C, BC]
  ): Bounded[(A, C), BA + BC] = Is()

  /** Each of the `N` places present or not. */
  given capped[T, B <: Int, N <: Int](using Bounded[T, B]): Bounded[Capped[T, N], N * (B + 1)] =
    Is()

  /** Six bits a character. */
  given token[N <: Int]: Bounded[Token[N], N * 6] = Is()

  /** An enum all of whose cases are singletons, costed at 8 bits: one of more than 256 cases is
    * costed short.
    */
  given singletons[E, Cases <: Tuple](using
      Mirror.SumOf[E] { type MirroredElemTypes = Cases },
      Tuple.Union[Cases] <:< Singleton
  ): Bounded[E, 8] = Is()

  /** At most K, as [[Shape.of]] bounds every shape: a choice among at most 64 options the model
    * listed is 6 bits, the widest answer a whole number's 64.
    */
  given extracted: Bounded[Extracted, K] = Is()
}
