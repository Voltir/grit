package grit.lifecycle.shadow

import grit.act.moves.Judging
import grit.core.act.MoveName
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
  *   1. `ask:shadow` — the heard message's state rebuilt as triage builds it, from the store
  *      as it stands now ([[TriageInput.heard]]), and the variant's question set asked of its
  *      own classifier once ([[ShadowVariant]]), a move ([[grit.act.moves.Judging]]):
  *      admitted against the day's cap, so one over it is `Failed` unavailable; nothing is
  *      asked when the message or its thread cannot be read, or when the variant is not
  *      declared. `ask:shadow:record` — when the classifier replied, its cost in the ledger,
  *      charged to the heard message's turn; a store that fails it keeps nothing.
  *   1. `record` — what it made of the message kept
  *      ([[grit.core.triage.TriageShadows.record]]); ignored when the message is gone or
  *      that variant shadowed it already.
  */
object Shadow {
  import ShadowJournal.given

  /** What the variant of `ref` made of its heard message, `entry`. */
  private final case class Made(ref: ShadowRef, entry: EntryId, row: Shadowed)

  private val asked
      : Flow[ShadowEnv, ShadowRef, Nothing, Judging.Judged[Either[String, (EntryId, Shadowed)]]] =
    Judging.flow[ShadowEnv, ShadowRef, Either[String, (EntryId, Shadowed)]](
      MoveName("shadow"),
      ref =>
        Judging.Charged(
          TurnRef(ref.triage.period.conversationId, ref.triage.turn),
          ref.workflowId,
          Subject.Conversation(ref.triage.period.conversationId)
        )
    )(_.moves, _.budget)(env => ref => ask(env, ref))

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
      .andThen(Flow.mayEnd(asked.keeping.map {
        case (_, Judging.Judged(_, Left(why))) => Left(s"not kept: its cost unrecorded: $why")
        case (ref, Judging.Judged(Right((entry, row)), Right(()))) => Right(Made(ref, entry, row))
        case (_, Judging.Judged(Left(why), Right(()))) => Left(s"not asked: $why")
      }))
      .andThen(Flow.step(record).keeping)
      .map {
        case (_, Left(why)) => s"not kept: $why"
        case (_, Right(false)) => "ignored: the message is gone or shadowed already"
        case (m, Right(true)) => s"kept: ${shown(m.row)}"
      }

  /** The shadow workflow for the shadow whose workflow id is `workflowId`: [[flow]] run. */
  def body(env: ShadowEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    Flows.run(flow, env, workflowId).merge

  /** The `ask:shadow` step: the heard message that is `shadow`'s triage's turn, and what the variant
    * made of it; why nothing was asked, otherwise.
    */
  private def ask(
      env: ShadowEnv^,
      shadow: ShadowRef
  ): Judging.Pose[Either[String, (EntryId, Shadowed)]]^{env} =
    env.variants.get(shadow.name) match {
      case None =>
        Judging.Pose.Made(Left(s"no variant ${ShadowName.value(shadow.name)} is declared"))
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
                .fold(
                  why => Judging.Pose.Made(Left(why)),
                  read => {
                    // A conversation not found is at no place, so no source covers it.
                    val sources = read.place.fold(Corpora.Empty)(env.sources.at)
                    named(read.entry, variant, routed, read.state, sources)
                  }
                )
          }
          .fold(
            e => Judging.Pose.Made(Left(s"thread unread: ${ShadowJournal.describe(e)}")),
            identity
          )
    }

  /** What `variant` made of the heard message `entry`'s `state`, read under `routed`, asked its
    * questions with `sources` of its own classifier.
    */
  private def named(
      entry: EntryId,
      variant: ShadowAsking^,
      routed: Routed,
      state: TriageQuestion.State,
      sources: Corpora
  ): Judging.Pose[Either[String, (EntryId, Shadowed)]]^{variant} = {
    val questions = variant.questions
    val digest = questions.request(state, sources).digest
    val requested = variant.requested
    Judging.Pose.asking(variant.classifier, routed, state, questions.asked(sources)) {
      case (Left(error), latency) => Right((entry, Shadowed.Failed(digest, error.kind, latency)))
      case (Right(a), latency) =>
        Right(
          (
            entry,
            Shadowed.Answered(
              digest,
              ShadowAnswers.Named(a.value),
              a.usage,
              requested,
              a.model,
              latency
            )
          )
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
