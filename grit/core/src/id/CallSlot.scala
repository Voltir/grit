package grit.core.id

/** A tool call's place in its turn's loop: call `index` (from 0) of the reply to round
  * `round` (from 0) of `turn`. Neither is negative.
  */
final case class CallSlot private (turn: TurnRef, round: Int, index: Int) {

  /** `tool:{workflow}:{round}:{index}`: the call's request key and its message topic. A stored
    * and wire form: a request row is keyed by it, and a turn waits on it.
    */
  def key: String = s"tool:${WorkflowId.value(turn.workflowId)}:$round:$index"
}

object CallSlot {

  /** Call `index` of round `round`'s reply in `turn`; `None` when either is negative. */
  def of(turn: TurnRef, round: Int, index: Int): Option[CallSlot] =
    Option.when(round >= 0 && index >= 0)(CallSlot(turn, round, index))

  /** The slot whose [[CallSlot.key]] is `key`; `None` when `key` is no slot's. */
  def read(key: String): Option[CallSlot] =
    key.split(':') match {
      case Array("tool", conversation, seq, round, index) =>
        for {
          turn <- TurnRef.fromWorkflowId(WorkflowId(s"$conversation:$seq"))
          r <- round.toIntOption
          i <- index.toIntOption
          slot <- of(turn, r, i)
        } yield slot
      case _ => None
    }
}
