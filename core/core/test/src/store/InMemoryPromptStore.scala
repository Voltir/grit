package grit.core.store

import grit.core.id.WorkflowId
import grit.core.prompt.{Fragment, FragmentId, SystemPrompt}

/** An in-memory [[PromptStore]] for tests, keeping [[StoreContract]]. It ignores the `Tx`. */
final class InMemoryPromptStore extends PromptStore {

  @caps.unsafe.untrackedCaptures
  var fragments = Vector.empty[Fragment]

  @caps.unsafe.untrackedCaptures
  // Each turn's ids as a List: a Vector inside a tuple trips capture checking's reach
  // capabilities in the lambdas below (docs/capture-checking.md).
  var turns = Vector.empty[(WorkflowId, List[FragmentId])]

  def keep(more: Vector[Fragment])(using Tx^): Either[StoreError, Unit] = {
    more.foreach(f => if (!fragments.exists(_.id == f.id)) fragments = fragments :+ f)
    Right(())
  }

  def record(workflow: WorkflowId, prompt: SystemPrompt)(using Tx^): Either[StoreError, Unit] =
    keep(prompt.fragments).map { _ =>
      if (!turns.exists(_._1 == workflow)) turns = turns :+ (workflow, prompt.ids.toList)
    }

  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Option[SystemPrompt]] =
    turns.find(_._1 == workflow) match {
      case None => Right(None)
      case Some((_, ids)) => prompt(ids.toVector).map(Some(_))
    }

  def prompt(ids: Vector[FragmentId])(using Tx^): Either[StoreError, SystemPrompt] =
    ids
      .foldLeft[Either[StoreError, Vector[Fragment]]](Right(Vector.empty)) { (acc, id) =>
        acc.flatMap(done =>
          fragments
            .find(_.id == id)
            .map(done :+ _)
            .toRight(StoreError.Invalid(s"prompt fragment ${FragmentId.value(id)} is not kept"))
        )
      }
      .map(SystemPrompt.of)

  def forget(workflows: Vector[WorkflowId])(using Tx^): Either[StoreError, Unit] = {
    turns = turns.filterNot(t => workflows.contains(t._1))
    Right(())
  }
}
