package grit.core.id

/** A period's number in its conversation: 1 for the first, in the order they opened. */
opaque type PeriodSeq = Long

object PeriodSeq {
  val First: PeriodSeq = 1L

  /** The period numbered `value`; `None` below [[First]]. */
  def of(value: Long): Option[PeriodSeq] = Option.when(value >= First)(value)

  def value(seq: PeriodSeq): Long = seq

  extension (seq: PeriodSeq) {
    def next: PeriodSeq = seq + 1
  }
}
