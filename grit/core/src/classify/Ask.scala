package grit.core.classify

/** One option of a typed choice: the value it stands for, the key the model sees, and what
  * it means (`None` when the key says enough).
  */
final case class Criterion[C](value: C, key: String, description: Option[String])

/** A typed choice's answer: the most probable value, a probability for every option in the
  * question's order (summing to 1), and the classifier's confidence (see [[Answer.Choice]]).
  */
final case class Decision[C](choice: C, probabilities: Vector[(C, Double)], confidence: Double) {

  /** The probability of `c`; 0 for a value that was not an option. */
  def probability(c: C): Double =
    probabilities.collectFirst { case (v, p) if v == c => p }.getOrElse(0.0)

  /** The probability of [[choice]]. */
  def top: Double = probability(choice)
}

/** Questions to ask together, and how to read their answers into a `T`. Built with
  * [[Ask.choice]] and [[Ask.noul]], combined with `zip` and `map`; [[Classifier.ask]]
  * rejects a combination that uses one id twice, or a choice with fewer than two distinct
  * keys.
  */
final case class Ask[T](
    questions: Vector[(QuestionId, Question)],
    read: Map[QuestionId, Answer] -> Either[ClassifierError, T]
) {

  def map[U](f: T -> U): Ask[U] = Ask(questions, answers => read(answers).map(f))

  /** Both sets of questions, in one request. */
  def zip[U](other: Ask[U]): Ask[(T, U)] =
    Ask(
      questions ++ other.questions,
      answers => read(answers).flatMap(t => other.read(answers).map(u => (t, u)))
    )
}

object Ask {

  /** Which of `criteria` fits: at least two, with distinct keys, or [[Classifier.ask]]
    * rejects it as `Invalid`.
    */
  def choice[C](
      id: QuestionId,
      instructions: String,
      criteria: Vector[Criterion[C]]
  ): Ask[Decision[C]] = {
    val question = Question.Choice(instructions, criteria.map(c => (c.key, c.description)))
    Ask(
      Vector(id -> question),
      answers =>
        for {
          answer <- answers.get(id).toRight(unanswered(id))
          decision <- answer match {
            case Answer.Choice(choice, probabilities, confidence) =>
              for {
                chosen <- criteria
                  .find(_.key == choice)
                  .toRight(unreadable(id, s"chose $choice, not an option"))
                ps = criteria.map(c =>
                  c.value -> probabilities
                    .collectFirst { case (k, p) if k == c.key => p }
                    .getOrElse(0.0)
                )
                _ <- Either.cond(
                  ps.exists(_._2 > 0),
                  (),
                  unreadable(id, "no option has probability")
                )
              } yield Decision(chosen.value, ps, confidence)
            case Answer.Noul(_) => Left(unreadable(id, "answered yes/no to a choice"))
          }
        } yield decision
    )
  }

  /** The probability that the answer to `instructions` is yes. */
  def noul(
      id: QuestionId,
      instructions: String,
      yes: Option[String],
      no: Option[String]
  ): Ask[Double] =
    Ask(
      Vector(id -> Question.Noul(instructions, yes, no)),
      answers =>
        answers.get(id).toRight(unanswered(id)).flatMap {
          case Answer.Noul(p) => Right(p)
          case Answer.Choice(_, _, _) => Left(unreadable(id, "answered a choice to a yes/no"))
        }
    )

  private def unanswered(id: QuestionId): ClassifierError =
    ClassifierError.Unreadable(s"${QuestionId.value(id)}: no answer")

  private def unreadable(id: QuestionId, why: String): ClassifierError =
    ClassifierError.Unreadable(s"${QuestionId.value(id)}: $why")
}
