package grit.knowledge.card

import grit.core.document.DocumentKeeper
import grit.core.id.DocKey
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** What a knowledge write hands the keeper of the move it is made in, a keeper believing each
  * document at no more than what the move keeps from ([[grit.core.act.Keeping.keep]]): what is
  * written is believed at that cap met with what it is given here.
  */
private[knowledge] object Believed {

  /** grit's own integrity, the meet's identity, so what is written is believed at exactly its
    * keeper's cap, compartments included. Through a keeper capped at nothing it is grit's own, in
    * every compartment: a raise.
    */
  def input(using Tx^): Integrity = Tx.integrityOf(Principal.Grit)

  /** For a write replacing the current document under `key` at `label`, as this transaction
    * reads it: that document's integrity, or [[input]] when there is none; so the replacement is
    * believed at no more than what it replaces. `Left` when the store fails.
    */
  def replacing(keeper: DocumentKeeper, key: DocKey, label: Label)(using
      Tx^
  ): Either[StoreError, Integrity] =
    keeper.current(key, label).map(_.fold(input)(_.integrity))
}
