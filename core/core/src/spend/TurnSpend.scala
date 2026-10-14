package grit.core.spend

import grit.core.id.TurnRef
import grit.core.message.Cost
import grit.core.model.Grade
import grit.core.store.UsageLedger

/** What a turn's calls cost, every row its workflow recorded ([[UsageLedger.of]]), and the row of
  * the call that gave its answer (its reply, else a heard turn's draft), when it gave one.
  */
final case class TurnSpend private (cost: Cost, answer: Option[UsageLedger.Row]) {

  /** The answer's model, then ` at ` and who served it when that was reported; `None` when the
    * turn gave no answer.
    */
  def served: Option[String] =
    answer.map(row => row.model + row.upstream.fold("")(u => s" at $u"))

  /** `0.42¢ · <served>` ([[Cost.inCents]]); the cost alone when the turn gave no answer. */
  def line: String = (cost.inCents +: served.toVector).mkString(" · ")

  /** Whether the answer was made at a grade above `grade`; false with no answer, or one
    * recorded with no grade.
    */
  def above(grade: Grade): Boolean = answer.flatMap(_.grade).exists(_.ordinal > grade.ordinal)
}

object TurnSpend {

  /** `turn`'s spend from `rows`, the rows `UsageLedger.of(turn.workflowId)` gave: their total,
    * and the row whose entry is its reply ([[TurnRef.replyId]]), else its draft
    * ([[TurnRef.draftId]]).
    */
  def of(turn: TurnRef, rows: Vector[UsageLedger.Row]): TurnSpend =
    TurnSpend(
      Cost.total(rows.map(_.usage)),
      rows.find(_.entry == turn.replyId).orElse(rows.find(_.entry == turn.draftId))
    )
}
