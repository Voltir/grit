package grit.core.edge

import grit.core.id.ConversationId
import grit.core.store.{Origin, StoreError, Tx}

/** Where each conversation happens, as an edge reads it to link back to one. */
trait Origins {

  /** `conversation`'s origin, when the transaction reads it as it reads what the conversation
    * holds ([[grit.core.visibility.Clearance]]); `None` for one it does not read, or no
    * conversation at all.
    */
  def of(conversation: ConversationId)(using Tx^): Either[StoreError, Option[Origin]]
}
