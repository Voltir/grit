package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

/** What a gate reads of a question set's answers, as a probability in [0, 1]. */
enum Reading {

  /** A yes/no's probability of yes. */
  case Yes(name: QuestionName)

  /** A choice's weight on `key`; 0 when its answer weighs `key` not at all. */
  case Key(name: QuestionName, key: String)

  /** 1 when `key` is a choice's most weighted key (the first of them, on a tie), else 0, as
    * when no key weighs above 0; the choice the answer reports is not read.
    */
  case Chosen(name: QuestionName, key: String)

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
      case Chosen(name, key) =>
        answers.get(name).collect { case Answer.Choice(_, weights, _) =>
          // As a choice's Decision reads it: a weight not a finite non-negative number is
          // none, and maxByOption keeps the first of equals.
          val top = weights
            .filter(w => w.probability > 0 && !w.probability.isInfinite)
            .maxByOption(_.probability)
          Probability.clamped(if (top.exists(_.key == key)) 1.0 else 0.0)
        }
    }
}

/** A reading at least `p`, or below `p`. */
enum Bound {
  case AtLeast(on: Reading, p: Probability)
  case Below(on: Reading, p: Probability)

  private[triage] def reading: Reading = this match {
    case AtLeast(on, _) => on
    case Below(on, _) => on
  }

  /** Whether it holds where its reading is `read`. */
  private[triage] def holds(read: Probability): Boolean = this match {
    case AtLeast(_, p) => read >= p
    case Below(_, p) => !(read >= p)
  }
}

/** Draft when every bound holds. */
final case class Gate(bounds: Vector[Bound]) {

  /** `answers` against every bound, in order: `Unread`, with the reading of the first bound
    * `answers` do not answer in its question's kind, when no bound before it fails;
    * otherwise `Fails`, with every bound they fail and what each read there (a bound unread
    * after the first failure is passed over); `Passes` when every one holds.
    */
  def check(answers: VectorMap[QuestionName, Answer]): Gate.Checked = {
    val read = bounds.map(b => b -> b.reading.in(answers))
    def failed(from: Vector[(Bound, Option[Probability])]) =
      from.collect { case (b, Some(v)) if !b.holds(v) => Gate.Failed(b, v) }
    read.zipWithIndex
      .collectFirst {
        case ((b, None), _) => Gate.Checked.Unread(b.reading)
        case ((b, Some(v)), i) if !b.holds(v) =>
          Gate.Checked.Fails(Gate.Failed(b, v), failed(read.drop(i + 1)))
      }
      .getOrElse(Gate.Checked.Passes)
  }

  /** [[check]] as a draft: `Some(true)` when `answers` pass, `Some(false)` when a bound
    * fails before any is unread, `None` when one is unread first.
    */
  def drafts(answers: VectorMap[QuestionName, Answer]): Option[Boolean] =
    check(answers) match {
      case Gate.Checked.Passes => Some(true)
      case Gate.Checked.Fails(_, _) => Some(false)
      case Gate.Checked.Unread(_) => None
    }
}

object Gate {

  /** `bound`, failed, reading `read` there. */
  final case class Failed(bound: Bound, read: Probability)

  /** What [[Gate.check]] found. */
  enum Checked {
    case Passes

    /** `first` failed first; `rest`, every later bound that failed, in order. */
    case Fails(first: Failed, rest: Vector[Failed])

    case Unread(reading: Reading)
  }
}
