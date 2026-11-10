package grit.eval.harness.sent

import grit.core.message.{Tokens, Usage}
import grit.turn.TurnRecord

/** One model call of a turn: what it was for (`None` when its ledger row names no call of the
  * turn), the model that answered, what it consumed, and grit's estimate of its input.
  */
final case class Spent(
    role: Option[TurnRecord.Role],
    model: String,
    usage: Usage,
    estimated: Tokens
)
