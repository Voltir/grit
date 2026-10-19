package grit.eval.harness.reply

import grit.core.document.Document
import grit.core.id.{ConversationId, WorkflowId}
import grit.core.stitch.Link
import grit.knowledge.plugin.Leaves

/** What the duplication measure found over windows drawn again: how many were drawn, how many
  * held a topic, and how many held a topic with one of its own leaves ([[Leaves]]), of each
  * kind and of either, with the workflows of the last (`twice`); and each window not drawn,
  * with why.
  */
final case class Duplicated(
    windows: Int,
    withTopic: Int,
    withStrand: Int,
    withStanding: Int,
    twice: Vector[WorkflowId],
    undrawn: Vector[(WorkflowId, String)]
) {

  /** As lines to print: counts and workflow ids, never a document's text. */
  def lines: Vector[String] =
    Vector(
      s"windows: $windows drawn, ${undrawn.size} not; with a topic $withTopic; with a topic " +
        s"and its own leaf ${twice.size} (its strand's document $withStrand, a Standing " +
        s"document of its strand $withStanding)"
    ) ++ twice.map(w => s"  twice: ${WorkflowId.value(w)}") ++
      undrawn.map((w, why) => s"  not drawn: ${WorkflowId.value(w)}: $why")
}

object Duplication {

  /** The roots of the strands the topics among `documents` were written from, each once, whose
    * links [[of]] needs.
    */
  def roots(documents: Vector[Document]): Vector[ConversationId] =
    Leaves.roots(documents).flatMap(_._2).distinct

  /** `windows`, each turn's window's documents or why it was not drawn, counted: a strand's
    * conversations are those `links` name ([[Leaves.pairs]]).
    */
  def of(
      windows: Vector[(WorkflowId, Either[String, Vector[Document]])],
      links: Vector[Link]
  ): Duplicated = {
    val drawn = windows.collect { case (w, Right(ds)) => (w, ds, Leaves.pairs(ds, links)) }
    def holding(kind: Leaves.Kind) = drawn.count(_._3.exists(_.kind == kind))
    Duplicated(
      drawn.size,
      drawn.count((_, ds, _) => Leaves.roots(ds).nonEmpty),
      holding(Leaves.Kind.Strand),
      holding(Leaves.Kind.Standing),
      drawn.collect { case (w, _, own) if own.nonEmpty => w },
      windows.collect { case (w, Left(why)) => (w, why) }
    )
  }
}
