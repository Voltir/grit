package grit.core.triage

import grit.core.message.Usage
import grit.core.period.Probability

/** What triage made of one heard message ([[grit.core.store.Payload.Heard]]). */
enum Tags {

  /** Its most probable `kind`, with that probability (`kindP`); the probability that someone
    * waits on a reply to it (`waiting`), that it states something worth keeping (`durable`),
    * and that a reply from grit would help (`helps`); the `model` that weighed them, and what
    * the call consumed.
    */
  case Weighed(
      kind: Kind,
      kindP: Probability,
      waiting: Probability,
      durable: Probability,
      helps: Probability,
      model: String,
      usage: Usage
  )

  /** The classifier gave no answer, for `why`. */
  case Unanswered(why: String)
}
