package grit.core.spend

import grit.core.id.{ConversationId, TurnRef}
import grit.core.store.{StoreError, Tx}

/** What recorded model calls cost, read back from the ledger
  * ([[grit.core.store.UsageLedger]]). The ledger keeps a conversation's calls until its
  * closings are collected (its ledger window). It holds every call an ask made: a job's, and
  * a lifecycle workflow's classifier and closing summary calls, a summary's kept even when a
  * turn came in and its period was not sealed with it. It never holds a turn summary with no
  * text, or a call billed by a step that crashed before recording it.
  */
trait Spending {

  /** The calls recorded on `day`, in every conversation. */
  def on(day: Day)(using Tx^): Either[StoreError, Spend]

  /** The calls recorded for `conversation`: its turns', and its closes'. */
  def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend]

  /** `turn`'s spend: every call its workflow recorded, and its answer's. A turn whose rows the
    * ledger no longer holds, or that recorded none, is [[grit.core.message.Cost.Zero]] and no
    * answer, not an error.
    */
  def turn(turn: TurnRef)(using Tx^): Either[StoreError, TurnSpend]
}
