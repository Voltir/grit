package grit.core.id

/** A turn's position within its conversation, from 0. Opaque so it cannot be
  * transposed with an entry's `seq`.
  */
opaque type TurnSeq = Long

object TurnSeq {
  val First: TurnSeq = 0L
  def apply(value: Long): TurnSeq = value
  def value(seq: TurnSeq): Long = seq
  given Ordering[TurnSeq] = Ordering.Long

  extension (seq: TurnSeq) {
    def next: TurnSeq = seq + 1
    def <(other: TurnSeq): Boolean = seq < other
    def <=(other: TurnSeq): Boolean = seq <= other
    def >(other: TurnSeq): Boolean = seq > other
    def >=(other: TurnSeq): Boolean = seq >= other
  }
}
