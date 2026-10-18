package grit.knowledge.topic

import grit.core.id.{ConversationId, DocumentVersion}
import grit.core.place.Place
import grit.core.stitch.StrandDigest

/** What a passage or a claim was written from, each once in the order first named. */
final case class Lineage private (tags: Vector[Lineage.Tag]) extends caps.Pure

object Lineage {

  /** One thing a passage or a claim was written from. */
  enum Tag extends caps.Pure {

    /** A strand of `room` as its digest was when written. */
    case Strand(root: ConversationId, room: Place, digest: StrandDigest)

    /** A card's version. */
    case Card(version: DocumentVersion)

    /** Another topic's version. */
    case Topic(version: DocumentVersion)
  }

  /** `tags`, each once where first named. */
  def of(tags: Vector[Tag]): Lineage = Lineage(tags.distinct)
}
