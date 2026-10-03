package grit.core.recipe

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.period.Probability
import grit.core.place.Service
import grit.core.triage.{Gate, Tags}

/** What a turn's model is offered of the tools of the services its conversation links
  * ([[grit.core.place.WorksIn]], [[grit.core.place.Reaches]]).
  */
enum Offering {

  /** Every one. */
  case All

  /** A service's only when one of the knowledge sources supplying it reads at least `at`
    * ([[Tags.V2.source]]). A service no source supplies, or whose root's answers do not
    * answer its sources' questions, is offered.
    */
  case BySource(at: Probability)

  /** The gate a service supplied by `sources` is offered by; `None` when it is offered
    * whatever the answers.
    */
  def gate(sources: Vector[KnowledgeSourceName]): Option[Gate] = this match {
    case All => None
    case BySource(at) =>
      sources.map(Tags.V2.source(_, at)) match {
        case first +: rest => Some(if (rest.isEmpty) first else Gate.AnyOf(first, rest))
        case _ => None
      }
  }
}

object Offering {

  /** Each of `services`, in order, as `offering` decides it from the root's `answers`
    * (`None`: the root was not weighed, or triage gave no answer), with the sources
    * `supplied` says supply it.
    */
  def decide(
      offering: Offering,
      supplied: VectorMap[Service, Vector[KnowledgeSourceName]],
      services: Vector[Service],
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Vector[ServiceOffer] =
    services.map { service =>
      val sources = supplied.getOrElse(service, Vector.empty)
      val verdict = offering.gate(sources) match {
        case None => ServiceOffer.Verdict.Ungated
        case Some(gate) =>
          answers.fold(ServiceOffer.Verdict.Unweighed)(a =>
            ServiceOffer.Verdict.Checked(gate.check(a))
          )
      }
      ServiceOffer(service, sources, verdict)
    }
}

/** A service as offering decided it: its supplying `sources`, and `verdict`. */
final case class ServiceOffer(
    service: Service,
    sources: Vector[KnowledgeSourceName],
    verdict: ServiceOffer.Verdict
) {

  /** Whether its tools are withheld: only when its gate failed. */
  def withheld: Boolean = verdict match {
    case ServiceOffer.Verdict.Checked(Gate.Checked.Fails(_, _)) => true
    case ServiceOffer.Verdict.Checked(Gate.Checked.Passes | Gate.Checked.Unread(_)) |
        ServiceOffer.Verdict.Ungated | ServiceOffer.Verdict.Unweighed =>
      false
  }
}

object ServiceOffer {

  enum Verdict {

    /** No gate reads it. */
    case Ungated

    /** It is gated, and the root has no answers: offered. */
    case Unweighed

    /** Its gate's result: withheld on `Fails`, offered otherwise. */
    case Checked(result: Gate.Checked)
  }
}
