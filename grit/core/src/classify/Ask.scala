package grit.core.classify

/** One option of a typed choice: the value it stands for, the key the model sees, and what
  * it means (`None` when the key says enough).
  */
final case class Criterion[C](value: C, key: String, description: Option[String])

/** A choice's answer: the most probable criterion's value, every criterion's value with its
  * probability in the question's order (summing to 1), and the classifier's confidence (see
  * [[Answer.Choice]]).
  */
final case class Decision[C](
    choice: C,
    probabilities: Vector[Decision.Weight[C]],
    confidence: Double
) {

  /** Summed over the criteria that stand for `c`; 0 for none. */
  def probability(c: C): Double = probabilities.filter(_.value == c).map(_.probability).sum

  def top: Double = probability(choice)
}

object Decision {
  final case class Weight[C](value: C, probability: Double)
}

/** Questions about a state of type `S`, asked together, and how their answers read into a
  * `T`. Built with [[Ask.choice]] and [[Ask.yesNo]], combined with `zip` and `map`.
  */
final class Ask[S, T] private (
    val questions: Vector[Question],
    reader: Vector[Answer] -> Either[ClassifierError, T]
) {

  /** `answers` holds exactly one answer per question, in order. */
  private[classify] def read(answers: Vector[Answer]): Either[ClassifierError, T] =
    reader(answers)

  def map[U](f: T -> U): Ask[S, U] = new Ask(questions, answers => reader(answers).map(f))

  /** Both sets of questions, in one request. */
  def zip[U](other: Ask[S, U]): Ask[S, (T, U)] = {
    val n = questions.size
    new Ask(
      questions ++ other.questions,
      answers => {
        val (mine, theirs) = answers.splitAt(n)
        read(mine).flatMap(t => other.read(theirs).map(u => (t, u)))
      }
    )
  }
}

object Ask {

  /** Two criteria of one choice given the same `key`. */
  final case class DuplicateKey(key: String)

  /** Which criterion fits the state, read back into its value. */
  def choice[S, C](
      instructions: String,
      first: Criterion[C],
      second: Criterion[C],
      rest: Criterion[C]*
  ): Either[DuplicateKey, Ask[S, Decision[C]]] = {
    val criteria = first +: second +: rest.toVector
    val keys = criteria.map(_.key)
    keys.diff(keys.distinct).headOption.map(DuplicateKey(_)).toLeft {
      def key(c: Criterion[C]) = Question.Key(c.key, c.description)
      val question =
        Question.Choice(instructions, key(first), key(second), rest.toVector.map(key))
      new Ask[S, Decision[C]](
        Vector(question),
        answers =>
          one(instructions, answers).flatMap {
            case Answer.Choice(choice, probabilities, confidence) =>
              for {
                chosen <- criteria
                  .find(_.key == choice)
                  .toRight(unreadable(instructions, s"chose $choice, not an option"))
                weights = criteria.map(c =>
                  Decision.Weight(
                    c.value,
                    probabilities.find(_.key == c.key).fold(0.0)(_.probability)
                  )
                )
                _ <- Either.cond(
                  weights.exists(_.probability > 0),
                  (),
                  unreadable(instructions, "no option has probability")
                )
              } yield Decision(chosen.value, weights, confidence)
            case Answer.YesNo(_) => Left(unreadable(instructions, "answered yes/no to a choice"))
          }
      )
    }
  }

  /** The probability that the answer to `instructions` is yes. `yes` and `no` say what each
    * means, where the instructions alone do not.
    */
  def yesNo[S](instructions: String, yes: Option[String], no: Option[String]): Ask[S, Double] =
    new Ask(
      Vector(Question.YesNo(instructions, yes, no)),
      answers =>
        one(instructions, answers).flatMap {
          case Answer.YesNo(p) => Right(p)
          case Answer.Choice(_, _, _) =>
            Left(unreadable(instructions, "answered a choice to a yes/no"))
        }
    )

  private def one(instructions: String, answers: Vector[Answer]): Either[ClassifierError, Answer] =
    answers.headOption.toRight(unreadable(instructions, "no answer"))

  private def unreadable(instructions: String, why: String): ClassifierError =
    ClassifierError.Unreadable(s"\"${instructions.take(60)}\": $why")
}
