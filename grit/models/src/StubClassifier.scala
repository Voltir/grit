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
    *   - A choice: the key that follows `~back:` (to the end of the message) at 0.9, the rest
    *     sharing 0.1; without that marker, or naming no key, the last key.
    */
  def answers(state: ujson.Value, questions: Vector[Question]): Answers = {
    val message = state.objOpt.flatMap(_.get("new_message")).flatMap(_.strOpt).getOrElse("")
    Answers(
      questions.map {
        case Question.YesNo(_, _, _) => Answer.YesNo(probability(message))
        case c: Question.Choice =>
          val keys = c.keys.map(_.name)
          val pick = back(message)
            .filter(keys.contains)
            .getOrElse(c.rest.lastOption.getOrElse(c.second).name)
          val rest = 0.1 / (keys.size - 1)
          val ps = keys.map(k => Answer.Weight(k, if (k == pick) 0.9 else rest))
          Answer.Choice(pick, ps, Answer.confidence(ps.map(_.probability)))
      },
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
      Model
    )
  }

  private val Marked = """~(0(?:\.\d+)?|1(?:\.0+)?)(?![\d.])""".r

  /** The p(yes) `message` names after `~`, or 0.9. */
  def probability(message: String): Double =
    Marked.findFirstMatchIn(message).flatMap(_.group(1).toDoubleOption).getOrElse(0.9)

  /** The option key after `~back:` in `message`, trimmed, if any. */
  def back(message: String): Option[String] = {
    val at = message.indexOf("~back:")
    Option.when(at >= 0)(message.drop(at + 6).trim).filter(_.nonEmpty)
  }
}
