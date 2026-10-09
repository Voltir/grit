package grit.models

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.message.{Tokens, Usage}

/** A [[Classifier]] that calls no model, for tests and the gate: [[StubClassifier.answers]],
  * at no cost.
  */
final class StubClassifier extends Classifier {

  protected def answer(
      state: ujson.Value,
      questions: Vector[Question]
  ): Either[ClassifierError, Answers] = Right(StubClassifier.answers(state, questions))
}

object StubClassifier {

  /** The model id a stub answer records. */
  val Model = "grit/stub-classifier"

  /** Answers from markers in the state's `new_message` string (the field `grit.turn`'s
    * topic states send):
    *
    *   - A yes/no question: the probability after `~` (`~0.1`, `~0.5`), or 0.9 without one.
    *   - A choice: the key that follows `~back:` (to the next `~back:` or the end of the
    *     message) at 0.9, the rest sharing 0.1; with several markers, the first naming one of
    *     its keys; without one naming a key, the last key.
    *   - A score: the level whose number (0 the first) follows the first `~level:` at 0.9,
    *     the rest sharing 0.1, its position the weights' mean; without one naming a level,
    *     the last level.
    */
  def answers(state: ujson.Value, questions: Vector[Question]): Answers = {
    val message = state.objOpt.flatMap(_.get("new_message")).flatMap(_.strOpt).getOrElse("")
    Answers(
      questions.map {
        case Question.YesNo(_, _, _) => Answer.YesNo(probability(message))
        case c: Question.Choice =>
          val keys = c.keys.map(_.name)
          val pick = backs(message)
            .find(keys.contains)
            .getOrElse(c.rest.lastOption.getOrElse(c.second).name)
          val rest = 0.1 / (keys.size - 1)
          val ps = keys.map(k => Answer.Weight(k, if (k == pick) 0.9 else rest))
          Answer.Choice(pick, ps, Answer.confidence(ps.map(_.probability)))
        case s: Question.Score =>
          val n = s.levels.size
          val pick = Level
            .findFirstMatchIn(message)
            .flatMap(_.group(1).toIntOption)
            .filter(_ < n)
            .getOrElse(n - 1)
          val ps = Vector.tabulate(n)(i => if (i == pick) 0.9 else 0.1 / (n - 1))
          val position = ps.zipWithIndex.map((p, i) => p * i).sum
          Answer.Score(position, ps, Answer.scoreConfidence(ps))
      },
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
      Model
    )
  }

  private val Level = """~level:(\d+)""".r

  private val Marked = """~(0(?:\.\d+)?|1(?:\.0+)?)(?![\d.])""".r

  /** The p(yes) `message` names after `~`, or 0.9. */
  def probability(message: String): Double =
    Marked.findFirstMatchIn(message).flatMap(_.group(1).toDoubleOption).getOrElse(0.9)

  /** Every option key after a `~back:` in `message`, in order, each to the next marker or
    * the end, trimmed.
    */
  def backs(message: String): Vector[String] =
    message.split("~back:", -1).toVector.drop(1).map(_.trim).filter(_.nonEmpty)
}
