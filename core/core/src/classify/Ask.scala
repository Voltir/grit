package grit.core.classify

/** One option of a typed choice: the value it stands for, the key the model sees, and what
  * it means (`None` when the key says enough).
  */
final case class Criterion[C](value: C, key: String, description: Option[String])

/** A choice's answer: every criterion's value with its probability, in the question's order,
  * summing to 1 (within rounding); `choice` is the value of the most probable criterion (the
  * first, on a tie); and the classifier's `confidence` (see [[Answer.Choice]]).
  */
final case class Decision[C] private (
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

  /** `weights` normalised to sum to 1, a NaN, infinite or negative one counted as 0; `None`
    * when none is left positive.
    */
  private[classify] def of[C](
      weights: Vector[Weight[C]],
      confidence: Double
  ): Option[Decision[C]] =
    normal(weights).map(n => new Decision(n.top, n.weights, confidence))

  /** Weights summing to 1, and the most probable one's value (the first, on a tie). */
  private[classify] final case class Normal[C](weights: Vector[Weight[C]], top: C)

  /** `weights` normalised as [[of]] says; `None` when none is left positive. */
  private[classify] def normal[C](weights: Vector[Weight[C]]): Option[Normal[C]] = {
    val clean = weights.map(w => w.copy(probability = Answer.mass(w.probability)))
    val total = clean.map(_.probability).sum
    val normal = clean.map(w => w.copy(probability = w.probability / total))
    Option
      .when(total > 0)(normal)
      .flatMap(_.maxByOption(_.probability))
      .map(top => Normal(normal, top.value))
  }
}

/** One level of a typed score: the value it stands for, and what it means as the model is
  * told it.
  */
final case class Level[L](value: L, description: String)

/** A score's answer: its `position` along the levels as the classifier reported it (0 the
  * first, between two when its weight spreads); each level's value with its probability, in
  * the levels' order, summing to 1 (within rounding); and the classifier's `confidence`.
  */
final case class Scored[L] private (
    position: Double,
    probabilities: Vector[Decision.Weight[L]],
    confidence: Double
)(
    /** The most probable level's value (the first, on a tie). */
    val likeliest: L
) {

  /** Summed over the levels that stand for `l`; 0 for none. */
  def probability(l: L): Double = probabilities.filter(_.value == l).map(_.probability).sum
}

object Scored {

  /** `weights` normalised to sum to 1, a NaN, infinite or negative one counted as 0; `None`
    * when none is left positive.
    */
  private[classify] def of[L](
      position: Double,
      weights: Vector[Decision.Weight[L]],
      confidence: Double
  ): Option[Scored[L]] =
    Decision.normal(weights).map(n => new Scored(position, n.weights, confidence)(n.top))
}

/** Questions about a state of type `S`, asked together, and how their answers read into a
  * `T`. Built with [[Ask.choice]], [[Ask.score]] and [[Ask.yesNo]], combined with `zip` and
  * `map`.
  */
final class Ask[S, T] private (
    val questions: Vector[Question],
    reader: Vector[Answer] -> Either[ClassifierError, T]
) {

  /** `T` read from `answers`, as a classifier's reply to these questions is read:
    * `Unreadable` when there is not exactly one answer per question, in order, or one does
    * not read.
    */
  def read(answers: Vector[Answer]): Either[ClassifierError, T] =
    Ask.counted(questions, answers).toLeft(answers).flatMap(reader)

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

  /** Which criterion fits the state, read back into its value. Reading is `Unreadable` when
    * the answer is yes/no, its choice is no criterion's key, or no criterion's key has positive
    * probability; weights on other keys are dropped.
    */
  def choice[S, C](
      instructions: String,
      first: Criterion[C],
      second: Criterion[C],
      rest: Criterion[C]*
  ): Either[DuplicateKey, Ask[S, Decision[C]]] = {
    val criteria = first +: second +: rest.toVector
    def key(c: Criterion[C]) = Question.Key(c.key, c.description)
    Question.choice(instructions, key(first), key(second), rest.map(key)*).map { question =>
      new Ask[S, Decision[C]](
        Vector(question),
        answers =>
          one(instructions, answers).flatMap {
            case Answer.Choice(choice, probabilities, confidence) =>
              for {
                _ <- Either.cond(
                  criteria.exists(_.key == choice),
                  (),
                  unreadable(instructions, s"chose $choice, not an option")
                )
                decision <- Decision
                  .of(
                    criteria.map(c =>
                      Decision.Weight(
                        c.value,
                        probabilities.find(_.key == c.key).fold(0.0)(_.probability)
                      )
                    ),
                    confidence
                  )
                  .toRight(unreadable(instructions, "no option has probability"))
              } yield decision
            case Answer.YesNo(_) => Left(unreadable(instructions, "answered yes/no to a choice"))
            case Answer.Score(_, _, _) =>
              Left(unreadable(instructions, "answered a score to a choice"))
          }
      )
    }
  }

  /** Where the state falls among `first`, `second`, `rest`, read back into their values.
    * Reading is `Unreadable` when the answer is not a score, it weighs other than one weight
    * per level, no level has positive weight, or its position is not a number from 0 to the
    * last level.
    */
  def score[S, L](
      instructions: String,
      first: Level[L],
      second: Level[L],
      rest: Level[L]*
  ): Either[Question.TooManyLevels, Ask[S, Scored[L]]] = {
    val levels = first +: second +: rest.toVector
    Question
      .score(instructions, first.description, second.description, rest.map(_.description)*)
      .map { question =>
        new Ask[S, Scored[L]](
          Vector(question),
          answers =>
            one(instructions, answers).flatMap {
              case Answer.Score(position, probabilities, confidence) =>
                for {
                  _ <- Either.cond(
                    probabilities.size == levels.size,
                    (),
                    unreadable(instructions, weighs(probabilities.size, levels.size))
                  )
                  _ <- Either.cond(
                    position >= 0 && position <= levels.size - 1,
                    (),
                    unreadable(instructions, s"at $position, off the levels")
                  )
                  scored <- Scored
                    .of(
                      position,
                      levels.zip(probabilities).map((l, p) => Decision.Weight(l.value, p)),
                      confidence
                    )
                    .toRight(unreadable(instructions, "no level has weight"))
                } yield scored
              case Answer.Choice(_, _, _) =>
                Left(unreadable(instructions, "answered a choice to a score"))
              case Answer.YesNo(_) => Left(unreadable(instructions, "answered yes/no to a score"))
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
          case Answer.Score(_, _, _) =>
            Left(unreadable(instructions, "answered a score to a yes/no"))
        }
    )

  /** `question`'s answer as the classifier gave it; `Unreadable` when it is not of the
    * question's kind, a choice names no key of the question's, or a score weighs other than
    * one weight per level.
    */
  def answer[S](question: Question): Ask[S, Answer] =
    question match {
      case c: Question.Choice =>
        new Ask(
          Vector(c),
          answers =>
            one(c.instructions, answers).flatMap {
              case a @ Answer.Choice(choice, _, _) =>
                Either.cond(
                  c.keys.exists(_.name == choice),
                  a,
                  unreadable(c.instructions, s"chose $choice, not an option")
                )
              case Answer.YesNo(_) =>
                Left(unreadable(c.instructions, "answered yes/no to a choice"))
              case Answer.Score(_, _, _) =>
                Left(unreadable(c.instructions, "answered a score to a choice"))
            }
        )
      case y: Question.YesNo =>
        new Ask(
          Vector(y),
          answers =>
            one(y.instructions, answers).flatMap {
              case a @ Answer.YesNo(_) => Right(a)
              case Answer.Choice(_, _, _) =>
                Left(unreadable(y.instructions, "answered a choice to a yes/no"))
              case Answer.Score(_, _, _) =>
                Left(unreadable(y.instructions, "answered a score to a yes/no"))
            }
        )
      case s: Question.Score =>
        new Ask(
          Vector(s),
          answers =>
            one(s.instructions, answers).flatMap {
              case a @ Answer.Score(_, probabilities, _) =>
                Either.cond(
                  probabilities.size == s.levels.size,
                  a,
                  unreadable(s.instructions, weighs(probabilities.size, s.levels.size))
                )
              case Answer.Choice(_, _, _) =>
                Left(unreadable(s.instructions, "answered a choice to a score"))
              case Answer.YesNo(_) =>
                Left(unreadable(s.instructions, "answered yes/no to a score"))
            }
        )
    }

  /** `Unreadable` when `answers` is not one answer per question. */
  private[classify] def counted(
      questions: Vector[Question],
      answers: Vector[Answer]
  ): Option[ClassifierError] =
    Option.when(answers.size != questions.size)(
      ClassifierError.Unreadable(s"${answers.size} answers to ${questions.size} questions")
    )

  /** `Unreadable` for the first answer not of its question's kind, `answers` paired with
    * `questions` by position.
    */
  private[classify] def kinds(
      questions: Vector[Question],
      answers: Vector[Answer]
  ): Option[ClassifierError] =
    questions
      .zip(answers)
      .flatMap {
        case (_: Question.Choice, Answer.Choice(_, _, _)) => None
        case (_: Question.Score, Answer.Score(_, _, _)) => None
        case (_: Question.YesNo, Answer.YesNo(_)) => None
        case (c: Question.Choice, a) =>
          Some(unreadable(c.instructions, s"answered ${kind(a)} to a choice"))
        case (s: Question.Score, a) =>
          Some(unreadable(s.instructions, s"answered ${kind(a)} to a score"))
        case (y: Question.YesNo, a) =>
          Some(unreadable(y.instructions, s"answered ${kind(a)} to a yes/no"))
      }
      .headOption

  private def kind(a: Answer): String = a match {
    case Answer.Choice(_, _, _) => "a choice"
    case Answer.Score(_, _, _) => "a score"
    case Answer.YesNo(_) => "yes/no"
  }

  private def weighs(weights: Int, levels: Int): String = s"weighs $weights levels of $levels"

  private def one(instructions: String, answers: Vector[Answer]): Either[ClassifierError, Answer] =
    answers.headOption.toRight(unreadable(instructions, "no answer"))

  private def unreadable(instructions: String, why: String): ClassifierError =
    ClassifierError.Unreadable(s"\"${instructions.take(60)}\": $why")
}
