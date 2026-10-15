package grit.lifecycle.shadow

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.{Durable, Flow, Flows, Step, StepName}
import grit.core.id.{EntryId, ShadowName, ShadowRef, TurnRef, WorkflowId}
import grit.core.route.Routed
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
  import ShadowJournal.given

  /** What the variant of `ref` made of its heard message, `entry`. */
  private final case class Made(ref: ShadowRef, entry: EntryId, row: Shadowed)

  private val asked: Step[ShadowEnv, ShadowRef, Either[String, (EntryId, Shadowed)]] =
    Step[ShadowEnv, ShadowRef, Either[String, (EntryId, Shadowed)]](StepName("ask")) { env => ref =>
      ask(env, ref)
    }

  private val record: Step[ShadowEnv, Made, Either[String, Boolean]] =
    Step.transact[ShadowEnv, Made, Either[String, Boolean]](
      StepName("record"),
      m => Subject.Conversation(m.ref.triage.period.conversationId)
    )(env =>
      m =>
        env.shadows
          .record(m.entry, m.ref.name, m.row, env.clock.now())
          .left
          .map(ShadowJournal.describe)
    )

  /** The shadow workflow, from the shadow's workflow id: what it did, for logs, at whichever
    * end; what the variant made of the message is in the store.
    */
  val flow: Flow[ShadowEnv, WorkflowId, String, String] =
    Flow
      .mayEnd(Flow.pass[WorkflowId].map { id =>
        ShadowRef.fromWorkflowId(id).toRight(s"not a shadow: ${WorkflowId.value(id)}")
      })
      .andThen(Flow.decide(asked) {
        case (ref, Right((entry, row))) => Right(Made(ref, entry, row))
        case (_, Left(why)) => Left(s"not asked: $why")
      })
      .andThen(Flow.step(record).keeping)
      .map {
        case (_, Left(why)) => s"not kept: $why"
        case (_, Right(false)) => "ignored: the message is gone or shadowed already"
        case (m, Right(true)) => s"kept: ${shown(m.row)}"
      }

  /** The shadow workflow for the shadow whose workflow id is `workflowId`: [[flow]] run. */
  def body(env: ShadowEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    Flows.run(flow, env, workflowId).merge

  /** The `ask` step: the heard message that is `shadow`'s triage's turn, and what the variant
    * made of it; why nothing was asked, otherwise.
    */
  private def ask(env: ShadowEnv^, shadow: ShadowRef): Either[String, (EntryId, Shadowed)] =
    env.variants.get(shadow.name) match {
      case None => Left(s"no variant ${ShadowName.value(shadow.name)} is declared")
      case Some(variant) =>
        Routed
          .reading(env.db.as(Subject.Conversation(shadow.triage.period.conversationId))) {
            (routed, reads) =>
              TriageInput
                .heard(
                  env.reads,
                  env.rooms,
                  reads,
                  TurnRef(shadow.triage.period.conversationId, shadow.triage.turn),
                  env.tuning,
                  TriageRecipe.Shipped
                )
                .map { read =>
                  // A conversation not found is at no place, so no source covers it.
                  val sources = read.place.fold(Corpora.Empty)(env.sources.at)
                  read.entry -> named(env.clock, variant, routed, read.state, sources)
                }
          }
          .left
          .map(e => s"thread unread: ${ShadowJournal.describe(e)}")
          .flatten
    }

  /** What `variant` made of `state`, read under `routed`, asked its questions with `sources`. */
  private def named(
      clock: Clock^,
      variant: ShadowAsking^,
      routed: Routed,
      state: TriageQuestion.State,
      sources: Corpora
  ): Shadowed = {
    val questions = variant.questions
    val request = questions.request(state, sources)
    val start = clock.millis()
    val answered = questions.ask(variant.classifier.asking(routed), state, sources)
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
