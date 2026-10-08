package grit.core.act

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{ScheduleId, TurnRef}
import grit.core.spend.Budget
import grit.core.visibility.Subject

/** Who acts, and within what (ADR 0034): every ask, call and keep is made under one. Its
  * moves belong to `turn`: their spend is recorded under its conversation, their requests are
  * made from it, and an edge rings its workflow. What its transactions read, and the least
  * label they write at, are each transaction's clearance, resolved as it opens for [[subject]]
  * (ADR 0030, 0032). `actsFor` is whom its requests record; `allowance`, what its asks may
  * spend; `gates`, what becomes of a call whose tool asks a person first.
  */
final case class Acting(turn: TurnRef, actsFor: ActsFor, allowance: Allowance, gates: Gates)
    extends caps.Pure {

  /** Whom its transactions are opened for: its turn. */
  def subject: Subject = Subject.Turn(turn)
}

/** Whom an acting's requests record as their principal (ADR 0017), read in the transaction
  * that writes each. A call whose principal reads as none is not sent: it is answered `Failed`.
  */
enum ActsFor {

  /** Its turn's asker as linked when the request is written
    * ([[grit.core.store.Askers.of]], [[grit.core.visibility.Subject.Turn]]'s rule): grit for a
    * turn grit's own entry begins; none for a turn with no first entry, whose conversation is
    * gone, or whose direct-message account was never seen.
    */
  case Asker

  /** The principal of the schedule `schedule` (ADR 0029): a declared one's grit, an asked one's
    * the person who asked. A request is not written when the schedule is gone.
    */
  case Scheduled(schedule: ScheduleId)
}

/** What an acting's asks may spend. */
enum Allowance {

  /** Admitted before its first ask, and not checked again: a turn, whose message the inbox
    * took while the day's cap admitted it ([[grit.core.spend.Budget.admits]]).
    */
  case Admitted

  /** Each ask admitted by `budget` first, against what the ledger recorded that day in every
    * conversation ([[grit.core.spend.Spending.on]]); refused, and no model called, once it does
    * not admit it.
    */
  case Daily(budget: Budget)
}

/** What becomes of a call whose tool asks a person first ([[grit.core.tool.Gate.Ask]]). */
enum Gates {

  /** Sent once its turn's asker approves it ([[grit.core.approval.Approval]], answered through
    * the inbox, ADR 0010), waiting up to `within`; declined, or unanswered by then, it is not
    * sent.
    */
  case Asker(within: FiniteDuration)

  /** Never sent: nobody waits on this acting. */
  case Closed
}
