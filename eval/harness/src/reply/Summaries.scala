package grit.eval.harness.reply

import java.time.Instant

import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.{Cost, Message, Tokens}
import grit.core.provider.ModelRequest
import grit.core.store.Payload
import grit.dbos.internal.Reader
import grit.eval.harness.capture.Capture
import grit.turn.{Turn, TurnSummary}

/** A turn's summary, to set beside other models': the turn's workflow, the summary it kept
  * live (`None` when its entry holds none), and the request its summary step asks
  * ([[TurnSummary.of]]), rebuilt over its conversation as it stood when that step started.
  */
final case class Summarised(workflow: WorkflowId, live: Option[String], request: ModelRequest)

/** Turns' summaries asked again of other models, for a person to read side by side. */
object Summaries {

  /** Each turn `reader`'s database recorded before `until` that kept a summary entry
    * ([[TurnSummary.id]]), oldest first, at most `first`; a turn that recorded no start of its
    * summary step is left out. `Left` naming what could not be read, never a message's text.
    */
  def of(reader: Reader^, until: Instant, first: Int): Either[String, Vector[Summarised]] =
    reader.turns(until).left.map(e => s"turn workflows unread: ${Capture.kind(e)}").flatMap {
      workflows =>
        workflows.foldLeft[Either[String, Vector[Summarised]]](Right(Vector.empty)) {
          case (Right(done), (w, _)) if done.size < first =>
            one(reader, w).map(done ++ _)
          case (other, _) => other
        }
    }

  private def one(reader: Reader^, workflow: WorkflowId): Either[String, Option[Summarised]] = {
    val name = WorkflowId.value(workflow)
    TurnRef.fromWorkflowId(workflow).fold(Right(None)) { turn =>
      for {
        steps <- reader.steps(workflow).left.map(e => s"steps of $name unread: ${Capture.kind(e)}")
        kept <- reader.all
          .read(reader.entries.get(TurnSummary.id(turn)))
          .left
          .map(e => s"summary of $name unread: ${Capture.kind(e)}")
        started = steps.find(_.name == Turn.Step.Summarise).flatMap(_.started)
        made <- (kept, started) match {
          case (Some(entry), Some(at)) =>
            reader.all
              .read(AsOf(reader, at).entries.list(turn.conversationId))
              .left
              .map(e => s"entries of $name unread: ${Capture.kind(e)}")
              .map(all =>
                Some(
                  Summarised(
                    workflow,
                    entry.payload match {
                      case Payload.Summary(text) => Some(text)
                      case _ => None
                    },
                    TurnSummary.of(all, turn)
                  )
                )
              )
          case _ => Right(None)
        }
      } yield made
    }
  }

  /** `cases` as markdown for `capture`: for each, its workflow, its live summary, then each of
    * `models`' answer to its request, its text as written, in `models`' order, with its tokens in and out and its
    * cost as the answer reports it (`unpriced` when it reports none); `not asked` for a model
    * with no answer to it. It holds the summaries' text: a review for a person, never printed.
    */
  def review(
      capture: String,
      cases: Vector[Summarised],
      models: Vector[(String, Map[ModelRequest, Message.Assistant])]
  ): String = {
    def said(m: Message.Assistant) = {
      val text = TurnSummary.text(m).getOrElse("(no text)")
      val cost = m.usage.costUsd.fold("unpriced")(Cost.cents)
      s"$text\n\n(${Tokens.value(m.usage.input)} in, ${Tokens.value(m.usage.output)} out, $cost)"
    }
    (s"# Summaries: capture $capture" +: cases.map { c =>
      val asked = models.map((model, answers) =>
        s"**$model**\n\n${answers.get(c.request).fold("not asked")(said)}"
      )
      (s"## ${WorkflowId.value(c.workflow)}" +: s"**live**\n\n${c.live.getOrElse("none")}" +: asked)
        .mkString("\n\n")
    }).mkString("", "\n\n", "\n")
  }
}
