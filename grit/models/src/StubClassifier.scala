package grit.models

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question, QuestionId}
import grit.core.message.{Tokens, Usage}

/** A [[Classifier]] that calls no model, for tests and the gate: it answers from markers in
  * the state's `new_message` string, at no cost.
  *
  *   - A yes/no question: the probability after `~` (`~0.1`, `~0.5`), or 0.9 without one.
  *   - A choice: the option whose key follows `~back:` (to the end of the message) at 0.9,
  *     the rest sharing 0.1; without that marker, or naming no option, the last option.
  */
final class StubClassifier extends Classifier {

  def answer(
      state: ujson.Value,
      questions: Vector[(QuestionId, Question)]
  ): Either[ClassifierError, Answers] = {
    val message = state.objOpt.flatMap(_.get("new_message")).flatMap(_.strOpt).getOrElse("")
    val answers = questions.map { (id, q) =>
      id -> (q match {
        case Question.Noul(_, _, _) => Right(Answer.Noul(StubClassifier.probability(message)))
        case Question.Choice(_, criteria) =>
          val keys = criteria.map(_._1)
          val back = StubClassifier.back(message).filter(keys.contains)
          back.orElse(keys.lastOption) match {
            case None => Left(ClassifierError.Invalid("a choice with no options"))
            case Some(pick) =>
              val rest = if (keys.size > 1) 0.1 / (keys.size - 1) else 0.0
              val ps =
                keys.map(k => k -> (if (k == pick) (if (keys.size > 1) 0.9 else 1.0) else rest))
              Answer.choice(ps).toRight(ClassifierError.Invalid("no options"))
          }
      })
    }
    answers
      .foldLeft[Either[ClassifierError, Map[QuestionId, Answer]]](Right(Map.empty)) {
        case (acc, (id, a)) => acc.flatMap(m => a.map(m.updated(id, _)))
      }
      .map(
        Answers(
          _,
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
          StubClassifier.Model
        )
      )
  }
}

object StubClassifier {

  /** The model id a stub answer records. */
  val Model = "grit/stub-classifier"

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
