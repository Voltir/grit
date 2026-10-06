package grit.core.job

/** The desk contract, kept by the in-memory fake. */
object InMemoryDeskTests extends DeskContract {
  protected def fresh(): ScheduleContract.Under = InMemoryUnder()
}
