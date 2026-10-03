package grit.lifecycle.shadow

import java.time.Instant

import grit.core.id.ShadowName
import grit.core.spend.DailyCap
import grit.core.triage.Shadowing
import grit.lifecycle.triage.TriageQuestions

/** A shadow of triage's question, as a deployment declares it: `questions` asked of every
  * heard message triaged at or after `since`, each per-source question of the knowledge
  * sources covering the place of its conversation (none when the conversation is not found),
  * of `model` (the deployment's classifier's when `None`), the oldest first, as many each UTC day as `dailyUsd` covers at its calls' recent
  * mean cost, and the rest left for a later day, never dropped. It may pass `dailyUsd` by one
  * sweep's batch's estimating error. What it makes is recorded under `name`
  * ([[grit.core.triage.TriageShadows]]) and never acted on. A name's rows are read as answers
  * to one question set, so a name asks one set for its life: a changed set, or a set in a
  * changed wording, is declared under a new name.
  */
final case class ShadowVariant(
    name: ShadowName,
    questions: TriageQuestions,
    model: Option[String],
    dailyUsd: DailyCap,
    since: Instant
) {

  /** This variant as the sweep enqueues its shadows. */
  def shadowing: Shadowing = Shadowing(name, since, dailyUsd)
}
