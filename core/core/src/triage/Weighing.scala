package grit.core.triage

import grit.core.id.TurnRef
import grit.core.message.Tokens

/** Live triage's question set put to a message said to grit, rooting a turn: the answers a
  * recipe offers its services by (ADR 0020, 0025), for a message no triage asked about.
  */
trait Weighing extends caps.SharedCapability {

  /** What live triage's set makes of the person's message that is `turn`'s first, asked with
    * the knowledge sources covering its conversation, once its placement has ended when it is
    * an opening; why not ([[Weighing.Unweighed]]).
    */
  def weigh(turn: TurnRef): Either[Weighing.Unweighed, Weighing.Weighed]
}

object Weighing {

  /** Why a message was not weighed. */
  enum Unweighed {

    /** The message or its thread could not be read, or it is not a person's. */
    case Unread

    /** Its opening's placement failed. */
    case PlacementFailed

    /** Its opening's placement did not end within the implementation's bound. */
    case PlacementLate

    /** The classifier failed. */
    case Unavailable

    /** The classifier gave an answer that does not read. */
    case Unreadable

    /** The classifier did not answer within the implementation's bound. */
    case Late
  }

  /** What a call to the classifier answered, `tags`, and grit's `estimate` of its request's
    * input, which its ledger row keeps beside the provider's count.
    */
  final case class Weighed(tags: Tags.Weighed, estimate: Tokens)
}
