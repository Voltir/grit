package grit.core.triage

import grit.core.id.KnowledgeSourceName
import grit.core.place.Place

/** Something a deployment says could supply what a message asks for, named `name`,
  * described to the classifier by `line` (a noun phrase: "the team's GitHub repository:
  * code, issues and pull requests"), for conversations within `within`.
  */
final case class KnowledgeSource(name: KnowledgeSourceName, line: String, within: Place)

/** A deployment's catalog of knowledge sources, in the order declared, no two of one name. */
final case class KnowledgeSources private (all: Vector[KnowledgeSource]) {

  /** Those whose `within` holds `place`, in order. */
  def at(place: Place): KnowledgeSources =
    new KnowledgeSources(all.filter(s => place.within(s.within)))
}

object KnowledgeSources {

  val Empty: KnowledgeSources = new KnowledgeSources(Vector.empty)

  /** `all`, or the name two of them share. */
  def of(all: Vector[KnowledgeSource]): Either[KnowledgeSourceName, KnowledgeSources] = {
    val names = all.map(_.name)
    names.diff(names.distinct).headOption.toLeft(new KnowledgeSources(all))
  }
}
