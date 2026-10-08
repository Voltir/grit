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
    * ([[Visibility.cleared]]). The asker, resolved when the transaction opens, with every
    * account linked to them then: in a direct message ([[grit.core.store.Origin.Direct]]), the
    * person its account is linked to, whoever wrote the turn's first entry; in a job's run,
    * grit; otherwise the author of the turn's first entry when that entry is inbound, grit when
    * it is grit's own, and no one, whose clearance is public, when it has none. In a direct
    * message, the label its room is read up to is also met with that person's clearance now,
    * as for [[Conversation]]. A turn whose conversation is gone reads only what is public.
    */
  case Turn(turn: TurnRef)

  /** Work on one conversation that no person asked for and that keeps what it makes at the
    * conversation's label (closing, settling, triaging, stitching, posting it): up to that
    * label, in its room and beyond; in a direct message, up to that label met with its person's
    * clearance now.
    */
  case Conversation(id: ConversationId)

  /** Public rows alone, and what carries no label (settings, schedules, workflow state). */
  case Public
}

/** The clearance [[grit.dbos]] opens its own transactions at (an edge's inbox, its sweeps,
  * the collector): every row, whatever its label or its room (a direct message's included), and
  * writes at `everything`. Nothing outside
  * `grit.dbos` may name it (enola-intent.yaml).
  */
object Maintenance {
  def clearance(everything: Label): Clearance = Clearance.maintaining(everything)
}
