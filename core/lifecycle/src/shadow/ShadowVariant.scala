package grit.lifecycle.shadow

import java.time.Instant

import grit.core.id.ShadowName
import grit.core.spend.DailyCap
import grit.core.triage.Shadowing
import grit.lifecycle.triage.TriageQuestion

/** A shadow of triage's question, as a deployment declares it: asked of every heard message
  * triaged at or after `since`, in `wording`, of `model` (the deployment's classifier's when
  * `None`), the oldest first, as many each UTC day as `dailyUsd` covers at its calls' recent
  * mean cost, and the rest left for a later day, never dropped. It may pass `dailyUsd` by one
  * sweep's batch's estimating error. What it makes is recorded
  * ([[grit.core.triage.TriageShadows]]) and never acted on.
  */
final case class ShadowVariant(
    name: ShadowName,
    wording: TriageQuestion.Wording,
    model: Option[String],
    dailyUsd: DailyCap,
    since: Instant
) {

  /** This variant as the sweep enqueues its shadows. */
  def shadowing: Shadowing = Shadowing(name, since, dailyUsd)
}
