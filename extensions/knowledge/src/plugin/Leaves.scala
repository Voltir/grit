package grit.knowledge.plugin

import grit.core.document.Document
import grit.core.id.{ConversationId, DocKey}
import grit.core.stitch.{Link, Strand}
import grit.knowledge.ref.TopicKey
import grit.knowledge.standing.Standing
import grit.knowledge.strand.StrandDocument
import grit.knowledge.topic.{Lineage, Topic}

/** A topic's own leaves: the documents that say again what a topic was written from, the
  * document of a strand it was written from ([[StrandDocument.key]]) and the Standing documents
  * of that strand's conversations ([[Standing.conversation]]). A window holding a topic and one
  * of its own leaves says one matter twice.
  */
object Leaves {

  /** Which of a topic's own leaves a document is. */
  enum Kind derives CanEqual {
    case Strand, Standing
  }

  /** The topic keyed `topic` and its own leaf keyed `leaf`, of `kind`. */
  final case class Own(topic: DocKey, leaf: DocKey, kind: Kind)

  /** Each topic among `documents` (a key [[TopicKey.read]] reads, data [[Topic.read]] reads),
    * by its key, with the roots of the strands its passages were written from, each once.
    */
  def roots(documents: Vector[Document]): Vector[(DocKey, Vector[ConversationId])] =
    documents.flatMap(d =>
      TopicKey
        .read(d.key)
        .flatMap(k => Topic.read(k, d.data).toOption)
        .map(t =>
          d.key -> t.passages
            .flatMap(_.from.tags)
            .collect { case Lineage.Tag.Strand(root, _, _) => root }
            .distinct
        )
    )

  /** Each topic among `documents` with each of its own leaves among them, in `documents`'
    * order: a root's strand is its conversations among `links` ([[Strand.of]]).
    */
  def pairs(documents: Vector[Document], links: Vector[Link]): Vector[Own] =
    roots(documents).flatMap { (found: (DocKey, Vector[ConversationId])) =>
      val (topic, rs) = found
      val strands = rs.map(StrandDocument.key).toSet
      val conversations = rs.flatMap(r => Strand.of(r, links)).toSet
      documents.flatMap(d =>
        if (strands.contains(d.key)) Some(Own(topic, d.key, Kind.Strand))
        else
          Standing
            .conversation(d.key)
            .filter(conversations.contains)
            .map(_ => Own(topic, d.key, Kind.Standing))
      )
    }
}
