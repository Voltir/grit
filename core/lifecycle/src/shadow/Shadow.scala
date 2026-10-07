package grit.lifecycle.shadow

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{EntryId, ShadowName, ShadowRef, TurnRef, WorkflowId}
import grit.core.triage.{Corpora, ShadowAnswers, Shadowed}
import grit.core.visibility.Subject
import grit.lifecycle.triage.{TriageInput, TriageQuestion, TriageRecipe}

/** The shadow workflow: one per declared variant and heard message ([[ShadowRef]]), on a
  * queue of its own, so it never delays a triage. What it makes is recorded and never acted
  * on. Which messages are shadowed, and how many a day, is the sweep's
  * ([[grit.core.triage.Shadowing]]): what is enqueued is already within the cap.
  *
  *   1. `ask` — the heard message's state rebuilt as triage builds it, from the store as it
  *      stands now ([[TriageInput.heard]]), and the variant's question set asked of it once
  *      ([[ShadowVariant]]); nothing is asked when the message or its thread cannot be read,
  *      or when the variant is not declared.
  *   1. `record` — what it made of the message kept
  *      ([[grit.core.triage.TriageShadows.record]]); ignored when the message is gone or
  *      that variant shadowed it already.
  */
object Shadow {

  /** The shadow's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Ask = "ask"
    val Record = "record"
  }

  /** The shadow workflow's body, for the shadow whose workflow id is `workflowId`. Returns
    * what it did, for logs: what the variant made of the message is in the store.
    */
  def body(env: ShadowEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    ShadowRef.fromWorkflowId(workflowId) match {
      case None => s"not a shadow: ${WorkflowId.value(workflowId)}"
      case Some(shadow) =>
        import ShadowJournal.given
        d.step(Step.Ask)(() => ask(env, shadow)) match {
          case Left(why) => s"not asked: $why"
          case Right((entry, row)) =>
            val at = env.clock.now()
            val (shadows, name) = (env.shadows, shadow.name)
            d.transact(Step.Record, Subject.Conversation(shadow.triage.period.conversationId))(
              shadows.record(entry, name, row, at).left.map(ShadowJournal.describe)
            ) match {
              case Left(why) => s"not kept: $why"
              case Right(false) => "ignored: the message is gone or shadowed already"
              case Right(true) => s"kept: ${shown(row)}"
            }
        }
    }

  /** The `ask` step: the heard message that is `shadow`'s triage's turn, and what the variant
    * made of it; why nothing was asked, otherwise.
    */
  private def ask(env: ShadowEnv^, shadow: ShadowRef): Either[String, (EntryId, Shadowed)] =
    env.variants.get(shadow.name) match {
      case None => Left(s"no variant ${ShadowName.value(shadow.name)} is declared")
      case Some(variant) =>
        TriageInput
          .heard(
            env.reads,
            env.rooms,
            env.db.as(Subject.Conversation(shadow.triage.period.conversationId)),
            TurnRef(shadow.triage.period.conversationId, shadow.triage.turn),
            env.tuning,
            TriageRecipe.Shipped
          )
          .map { read =>
            // A conversation not found is at no place, so no source covers it.
            val sources = read.place.fold(Corpora.Empty)(env.sources.at)
            read.entry -> named(env.clock, variant, read.state, sources)
          }
    }

  /** What `variant` made of `state` asked its questions with `sources`. */
  private def named(
      clock: Clock^,
      variant: ShadowAsking^,
      state: TriageQuestion.State,
      sources: Corpora
  ): Shadowed = {
    val questions = variant.questions
    val request = questions.request(state, sources)
    val start = clock.millis()
    val answered = questions.ask(variant.classifier, state, sources)
    val latency = (clock.millis() - start).millis
    answered match {
      case Left(error) => Shadowed.Failed(request.digest, error.kind, latency)
      case Right(a) =>
        Shadowed.Answered(
          request.digest,
          ShadowAnswers.Named(a.value),
          a.usage,
          variant.requested,
          a.model,
          latency
        )
    }
  }

  private def shown(row: Shadowed): String = row match {
    case Shadowed.Answered(_, _, _, _, model, latency) =>
      s"answered by $model in ${latency.toMillis} ms"
    case Shadowed.Failed(_, failure, _) =>
      s"failed: ${grit.core.triage.ShadowedJson.kindWritten(failure)}"
  }
}
