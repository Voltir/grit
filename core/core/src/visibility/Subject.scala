package grit.core.visibility

import grit.core.id.ConversationId
import grit.core.id.TurnRef

/** Whom a database transaction is opened for, which decides its [[Clearance]] as it opens
  * (ADR 0030).
  */
enum Subject {

  /** A turn, read for the person it answers: everything recorded or kept in its conversation's
    * room up to the label its conversation was created with; anything else, other rooms and
    * documents kept in none included, up to that label met with its asker's clearance
    * ([[Visibility.cleared]]). The asker is the author of the turn's first entry when that
    * entry is inbound, else grit when the turn is a job's run or begins with grit's own entry,
    * else no one, whose clearance is public. A turn whose conversation is gone reads only what
    * is public.
    */
  case Turn(turn: TurnRef)

  /** Work on one conversation that no person asked for and that keeps what it makes at the
    * conversation's label (closing, settling, triaging, stitching, posting it): up to that
    * label, in its room and beyond.
    */
  case Conversation(id: ConversationId)

  /** Public rows alone, and what carries no label (settings, schedules, workflow state). */
  case Public
}

/** The clearance [[grit.dbos]] opens its own transactions at (an edge's inbox, its sweeps,
  * the collector): every row, whatever its label, and writes at `everything`. Nothing outside
  * `grit.dbos` may name it (enola-intent.yaml).
  */
object Maintenance {
  def clearance(everything: Label): Clearance = Clearance.of(everything)
}
