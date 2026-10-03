package grit.core.triage

import scala.collection.immutable.{ListSet, VectorMap}

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

/** A test of a question set's answers that explains every failure: one bound, every one of
  * some gates, or any one of them. Built only from bounds, it reads nothing but the answers,
  * and only the readings [[reads]] lists.
  */
enum Gate {

  /** Passes where `bound` holds. */
  case Holds(bound: Bound)

  /** Passes where every one of `gates` passes; with none, passes everything. */
  case All(gates: Vector[Gate])

  /** Passes where `first` or any of `rest` passes. */
  case AnyOf(first: Gate, rest: Vector[Gate])

  /** Every reading its bounds read, each once, in the order its bounds are written. */
  def reads: ListSet[Reading] = this match {
    case Holds(bound) => ListSet(bound.reading)
    case All(gates) => gates.foldLeft(ListSet.empty[Reading])(_ ++ _.reads)
    case AnyOf(first, rest) => rest.foldLeft(first.reads)(_ ++ _.reads)
  }

  /** `answers` against it, by one rule at every level. `Holds` is `Unread` when `answers` do
    * not answer its reading in its question's kind. `All` `Fails` when any part fails, and
    * `AnyOf` when every part fails, with every bound its parts failed, in order, and what
    * each read; `All` `Passes` when every part passes, and `AnyOf` when any part does.
    * Otherwise `Unread`, with its first unread part's reading. Answering more never turns
    * `Passes` or `Fails` into anything else.
    */
  def check(answers: VectorMap[QuestionName, Answer]): Gate.Checked = this match {
    case Holds(bound) =>
      bound.reading.in(answers) match {
        case None => Gate.Checked.Unread(bound.reading)
        case Some(read) if bound.holds(read) => Gate.Checked.Passes
        case Some(read) => Gate.Checked.Fails(Gate.Failed(bound, read), Vector.empty)
      }
    case All(gates) =>
      gates.foldLeft[Gate.Checked](Gate.Checked.Passes)((acc, g) => Gate.and(acc, g.check(answers)))
    case AnyOf(first, rest) =>
      rest.foldLeft(first.check(answers))((acc, g) => Gate.or(acc, g.check(answers)))
  }

  /** [[check]] as a draft: `Some(true)` on `Passes`, `Some(false)` on `Fails`, `None` on
    * `Unread`.
    */
  def drafts(answers: VectorMap[QuestionName, Answer]): Option[Boolean] =
    check(answers) match {
      case Gate.Checked.Passes => Some(true)
      case Gate.Checked.Fails(_, _) => Some(false)
      case Gate.Checked.Unread(_) => None
    }
}

object Gate {

  /** Passes everything. */
  val Open: Gate = All(Vector.empty)

  /** Every one of these bounds, in order. */
  def bounds(first: Bound, rest: Bound*): Gate = All((first +: rest.toVector).map(Holds(_)))

  /** Every one of `gates`, in order. */
  def all(gates: Gate*): Gate = All(gates.toVector)

  /** Any one of these, in order. */
  def either(first: Gate, second: Gate, rest: Gate*): Gate = AnyOf(first, second +: rest.toVector)

  /** `bound`, failed, reading `read` there. */
  final case class Failed(bound: Bound, read: Probability)

  /** What [[Gate.check]] found. */
  enum Checked {
    case Passes

    /** `first` failed first; `rest`, every later bound that failed, in order. */
    case Fails(first: Failed, rest: Vector[Failed])

    case Unread(reading: Reading)
  }

  /** Two parts of an `All`, `earlier` and `later`, as one. */
  private def and(earlier: Checked, later: Checked): Checked = (earlier, later) match {
    case (Checked.Fails(first, rest), Checked.Fails(f, r)) => Checked.Fails(first, (rest :+ f) ++ r)
    case (fails @ Checked.Fails(_, _), _) => fails
    case (_, fails @ Checked.Fails(_, _)) => fails
    case (unread @ Checked.Unread(_), _) => unread
    case (Checked.Passes, other) => other
  }

  /** Two parts of an `AnyOf`, `earlier` and `later`, as one. */
  private def or(earlier: Checked, later: Checked): Checked = (earlier, later) match {
    case (Checked.Passes, _) | (_, Checked.Passes) => Checked.Passes
    case (unread @ Checked.Unread(_), _) => unread
    case (_, unread @ Checked.Unread(_)) => unread
    case (Checked.Fails(first, rest), Checked.Fails(f, r)) => Checked.Fails(first, (rest :+ f) ++ r)
  }
}
