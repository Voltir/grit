package grit.lifecycle.shadow

import scala.concurrent.duration.*

import grit.core.classify.{Answers, Classifier, ClassifierError, Request}
import grit.core.durable.Durable
import grit.core.id.{EntryId, ShadowName, ShadowRef, WorkflowId}
import grit.core.triage.{Shadowed, Tags}
import grit.lifecycle.triage.{TriageInput, TriageQuestion, TriageRecipe}

/** The shadow workflow: one per declared variant and heard message ([[ShadowRef]]), on a
  * queue of its own, so it never delays a triage. What it makes is recorded and never acted
  * on. Which messages are shadowed, and how many a day, is the sweep's
  * ([[grit.core.triage.Shadowing]]): what is enqueued is already within the cap.
  *
  *   1. `ask` — the heard message's question rebuilt as triage builds it, from the store as
  *      it stands now ([[TriageInput.build]]), and asked once in the variant's wording;
  *      nothing is asked when the message or its thread cannot be read, when the variant is
  *      not declared, or when its wording repeats a key.
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
            d.transact(Step.Record)(
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
          .build(env.reads, env.rooms, env.db, shadow.triage, env.tuning, TriageRecipe.Shipped)
          .flatMap { (entry, state) =>
            val call = new Call
            val clock = env.clock
            val timed = Classifier.around(variant.classifier) { (request, ask) =>
              val start = clock.millis()
              val answered = ask()
              call.made = Some((request, answered, (clock.millis() - start).millis))
              answered
            }
            val tags = TriageQuestion.judge(timed, variant.wording, state)
            call.made match {
              case None => Left("the variant's wording repeats a key")
              case Some((request, Left(error), latency)) =>
                Right(entry -> Shadowed.Failed(request.digest, error.kind, latency))
              case Some((request, Right(answers), latency)) =>
                tags match {
                  case Tags.Weighed(_, _, _, _, _, _, _) =>
                    Right(
                      entry -> Shadowed.Answered(
                        request.digest,
                        answers.answers,
                        answers.usage,
                        variant.requested,
                        answers.model,
                        latency
                      )
                    )
                  // Answers came back but did not read as triage's: unreadable.
                  case Tags.Unanswered(_) =>
                    Right(
                      entry -> Shadowed
                        .Failed(request.digest, ClassifierError.Kind.Unreadable, latency)
                    )
                }
            }
          }
    }

  /** The one call an `ask` makes, as the classifier was handed it. */
  private final class Call {
    // Written once by the classifier `ask` builds, which is asked only inside that `ask`,
    // and read after it returns: no other code can observe it.
    @caps.unsafe.untrackedCaptures
    var made: Option[(Request, Either[ClassifierError, Answers], FiniteDuration)] = None
  }

  private def shown(row: Shadowed): String = row match {
    case Shadowed.Answered(_, _, _, _, model, latency) =>
      s"answered by $model in ${latency.toMillis} ms"
    case Shadowed.Failed(_, failure, _) =>
      s"failed: ${grit.core.triage.ShadowedJson.kindWritten(failure)}"
  }
}
