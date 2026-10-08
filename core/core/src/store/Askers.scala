package grit.core.store

import grit.core.id.TurnRef
import grit.core.identity.Principal

/** Who a turn answers, resolved as its transactions' clearance is
  * ([[grit.core.visibility.Subject.Turn]]). A working one is built only by the engine and held
  * only by what a deployment's kit builds: a plugin's tool is never given one, so it cannot read
  * who asked.
  */
trait Askers extends caps.Pure {

  /** `turn`'s asker as linked now, by [[grit.core.visibility.Subject.Turn]]'s rule: in a direct
    * message, the person its account is linked to, whoever wrote the turn's first entry
    * (`None` for an account never seen); grit for a job's run, or a turn grit's own entry
    * begins elsewhere; the person whose account wrote its first entry, when that entry is
    * inbound; `None` for a turn with no first entry, or whose conversation is gone.
    */
  def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]]
}
