package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

/** What a gate reads of a question set's answers: a yes/no's probability of yes, or a choice
  * key's probability (0 when the answer weighs it not at all), each clamped to [0, 1].
  */
enum Reading {
  case Yes(name: QuestionName)
  case Key(name: QuestionName, key: String)

  /** Its probability in `answers`; `None` when its question is not answered there, or is
    * answered in the other kind.
    */
  private[triage] def in(answers: VectorMap[QuestionName, Answer]): Option[Probability] =
    this match {
      case Yes(name) =>
        answers.get(name).collect { case Answer.YesNo(p) => Probability.clamped(p) }
      case Key(name, key) =>
        answers.get(name).collect { case Answer.Choice(_, weights, _) =>
          Probability.clamped(weights.find(_.key == key).fold(0.0)(_.probability))
        }
    }
}

/** A reading at least `p`, or below `p`. */
enum Bound {
  case AtLeast(on: Reading, p: Probability)
  case Below(on: Reading, p: Probability)
}

/** Draft when every bound holds. */
final case class Gate(bounds: Vector[Bound]) {

  /** Whether `answers` pass; `None` when one a bound reads is missing or not of its kind. */
  def drafts(answers: VectorMap[QuestionName, Answer]): Option[Boolean] =
    bounds.foldLeft(Option(true)) { (passed, bound) =>
      val holds = bound match {
        case Bound.AtLeast(on, p) => on.in(answers).map(_ >= p)
        case Bound.Below(on, p) => on.in(answers).map(v => !(v >= p))
      }
      passed.flatMap(all => holds.map(all && _))
    }
}
