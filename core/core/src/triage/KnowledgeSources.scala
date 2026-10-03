package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.id.KnowledgeSourceName
import grit.core.place.{Place, Service}

/** Something a deployment says could supply what a message asks for, named `name`,
  * described to the classifier by `line` (a noun phrase: "the team's GitHub repository:
  * code, issues and pull requests"), for conversations within `within`; `supplies`, the
  * service whose tools can reach it, if any.
  */
final case class KnowledgeSource(
    name: KnowledgeSourceName,
    line: String,
    within: Place,
    supplies: Option[Service] = None
)

/** A deployment's catalog of knowledge sources, in the order declared, no two of one name. */
final case class KnowledgeSources private (all: Vector[KnowledgeSource]) {

  /** Those whose `within` holds `place`, in order. */
  def at(place: Place): KnowledgeSources =
    new KnowledgeSources(all.filter(s => place.within(s.within)))

  /** Each service some source supplies, with those sources, in the catalog's order. */
  def supplied: VectorMap[Service, Vector[KnowledgeSourceName]] =
    all.foldLeft(VectorMap.empty[Service, Vector[KnowledgeSourceName]])((acc, s) =>
      s.supplies.fold(acc)(service =>
        acc.updated(service, acc.getOrElse(service, Vector.empty) :+ s.name)
      )
    )
}

object KnowledgeSources {

  val Empty: KnowledgeSources = new KnowledgeSources(Vector.empty)

  /** `all`, or the name two of them share. */
  def of(all: Vector[KnowledgeSource]): Either[KnowledgeSourceName, KnowledgeSources] = {
    val names = all.map(_.name)
    names.diff(names.distinct).headOption.toLeft(new KnowledgeSources(all))
  }
}
