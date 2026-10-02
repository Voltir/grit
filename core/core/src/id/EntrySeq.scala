package grit.core.id

/** An entry's position in its conversation: no two of its entries share one, and an entry
  * written later has a greater one, even after earlier ones are purged
  * ([[grit.core.store.EntryStore.lockNext]]).
  */
opaque type EntrySeq = Long

object EntrySeq {

  /** A conversation's first entry's. */
  val First: EntrySeq = 0L
  def apply(value: Long): EntrySeq = value
  def value(seq: EntrySeq): Long = seq
  given Ordering[EntrySeq] = Ordering.Long
  extension (seq: EntrySeq) {
    def next: EntrySeq = seq + 1
    def <(other: EntrySeq): Boolean = seq < other
    def <=(other: EntrySeq): Boolean = seq <= other
    def >(other: EntrySeq): Boolean = seq > other
    def >=(other: EntrySeq): Boolean = seq >= other
  }
}
