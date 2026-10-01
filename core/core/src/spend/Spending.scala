package grit.core.spend

import grit.core.id.ConversationId
import grit.core.store.{StoreError, Tx}

/** What recorded model calls cost, read back from the ledger
  * ([[grit.core.store.UsageLedger]]). The ledger keeps a conversation's calls until its
  * closings are collected (its ledger window). It never holds Settle's or the close gate's
  * classifier calls, a closing summary its period did not keep, a turn summary with no text,
  * or a call billed by a step that crashed before recording it.
  */
trait Spending {

  /** The calls recorded on `day`, in every conversation. */
  def on(day: Day)(using Tx^): Either[StoreError, Spend]

  /** The calls recorded for `conversation`: its turns', and its closes'. */
  def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend]
}
