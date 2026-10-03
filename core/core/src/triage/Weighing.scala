package grit.core.triage

import grit.core.id.TurnRef
import grit.core.message.Tokens

/** Live triage's question set put to a message said to grit, rooting a turn: the answers a
  * recipe offers its services by (ADR 0020, 0025), for a message no triage asked about.
  */
trait Weighing extends caps.SharedCapability {

  /** What live triage's set makes of the person's message that is `turn`'s first, asked with
    * the knowledge sources covering its conversation, once its placement has ended when it is
    * an opening. Why not, when the message cannot be read or is not a person's, its placement
    * failed or did not end within the implementation's bound, or the classifier failed or gave
    * an answer that does not read.
    */
  def weigh(turn: TurnRef): Either[String, Weighing.Weighed]
}

object Weighing {

  /** What a call to the classifier answered, `tags`, and grit's `estimate` of its request's
    * input, which its ledger row keeps beside the provider's count.
    */
  final case class Weighed(tags: Tags.Weighed, estimate: Tokens)
}
