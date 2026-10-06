package grit.core.id

/** Tool calls' slots for tests. */
object TestCallSlots {

  /** The turn a slot is in unless a test names its own: `c:0`. */
  val Turn: TurnRef = TurnRef(ConversationId("c"), TurnSeq.First)

  /** The call at `index` of round `round`'s reply in `turn`; a test's own error when either is
    * negative.
    */
  def at(turn: TurnRef = Turn, round: Int = 0, index: Int = 0): CallSlot =
    CallSlot
      .of(turn, round, index)
      .getOrElse(throw new java.lang.AssertionError(s"no call at round $round, index $index"))

  /** The first call of [[Turn]]'s first round. */
  val First: CallSlot = at()
}
