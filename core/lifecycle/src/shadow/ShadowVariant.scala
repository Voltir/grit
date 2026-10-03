package grit.lifecycle.shadow

import java.time.Instant

import grit.core.id.ShadowName
import grit.core.spend.DailyCap
import grit.core.triage.Shadowing

/** A shadow of triage's question, as a deployment declares it: `question` asked of every
  * heard message triaged at or after `since`, of `model` (the deployment's classifier's when
  * `None`), the oldest first, as many each UTC day as `dailyUsd` covers at its calls' recent
  * mean cost, and the rest left for a later day, never dropped. It may pass `dailyUsd` by one
  * sweep's batch's estimating error. What it makes is recorded under `name`
  * ([[grit.core.triage.TriageShadows]]) and never acted on. A name's rows are read as answers
  * to one question, so a name asks one question for its life: a changed wording or question
  * set is declared under a new name.
  */
final case class ShadowVariant(
    name: ShadowName,
    question: ShadowQuestion,
    model: Option[String],
    dailyUsd: DailyCap,
    since: Instant
) {

  /** This variant as the sweep enqueues its shadows. */
  def shadowing: Shadowing = Shadowing(name, since, dailyUsd)
}
