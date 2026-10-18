package grit.knowledge.topic

import grit.knowledge.ref.TopicKey

/** What a topic's writer says of a span: a link to another topic, or a claim. */
enum TopicNote extends caps.Pure {

  /** The span is about the topic `to`. */
  case Link(to: TopicKey)

  /** The span states `claim`. */
  case Claimed(claim: Claim)
}

/** A claim: its id within its topic, a `key` naming what it is about across topics when the
  * writer gave one, how certain it is, how it bears on other claims, and what it was made from.
  */
final case class Claim(
    id: ClaimId,
    key: Option[ClaimKey],
    certainty: Certainty,
    relations: Vector[Relation],
    from: Lineage
) extends caps.Pure

/** How sure a claim's writer is of it. */
enum Certainty extends caps.Pure {
  case Firm, Likely, Doubtful
}

/** How a claim bears on `to`, a claim of this topic or another. */
final case class Relation(kind: Relation.Kind, to: ClaimRef) extends caps.Pure

object Relation {
  enum Kind extends caps.Pure {
    case Supersedes, Contradicts, Confirms
  }
}

/** The claim `claim` of the topic `topic`. */
final case class ClaimRef(topic: TopicKey, claim: ClaimId) extends caps.Pure

/** A claim's id within its topic: `c` then a number from 1, no leading zero. */
opaque type ClaimId = String

object ClaimId {

  /** `text` as an id, or why not. */
  def of(text: String): Either[String, ClaimId] =
    Either.cond(Form.matches(text), text, s"a claim's id is c and a number from 1: $text")

  def value(id: ClaimId): String = id

  private val Form = "c[1-9][0-9]*".r
}

/** What a claim is about, named alike across topics: lower-case words and digits joined by `-`. */
opaque type ClaimKey = String

object ClaimKey {

  /** `text` as a key, or why not. */
  def of(text: String): Either[String, ClaimKey] =
    Either.cond(Form.matches(text), text, s"a claim's key is lower-case words joined by -: $text")

  def value(key: ClaimKey): String = key

  private val Form = "[a-z0-9]+(-[a-z0-9]+)*".r
}
