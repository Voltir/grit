package grit.models

import grit.core.classify.{Answer, Question}

/** What a classifier's answers hold, whichever classifier gave them: a choice weighs each of
  * its question's keys, in order, summing to 1, and chooses one of them; a score weighs each
  * level, summing to 1, its position the weights' mean level (Σ i·pᵢ); a confidence and a
  * yes/no's probability are each from 0 to 1. [[StubClassifier]] stands in for Jev, so its
  * answers and Jev's documented ones are both held to it (`AnswersSpecTests`).
  */
object AnswersSpec {

  /** Within this of each other, two sums of floating weights are the same. */
  private val Tolerance = 1e-9

  /** Each way `answer`, to `question`, breaks the spec; none when it keeps it. */
  def broken(question: Question, answer: Answer): Vector[String] = {
    def unit(what: String, x: Double) =
      Option.when(!(x >= 0 && x <= 1))(s"$what $x is not from 0 to 1")
    def sums(ps: Vector[Double]) =
      Option.when(math.abs(ps.sum - 1) > Tolerance)(s"weights sum to ${ps.sum}")
    (question, answer) match {
      case (c: Question.Choice, Answer.Choice(choice, weights, confidence)) =>
        val keys = c.keys.map(_.name)
        Vector(
          Option.when(weights.map(_.key) != keys)(
            s"weighs ${weights.map(_.key).mkString(",")}, not ${keys.mkString(",")}"
          ),
          sums(weights.map(_.probability)),
          Option.when(!keys.contains(choice))(s"chose $choice, not a key"),
          unit("confidence", confidence)
        ).flatten
      case (s: Question.Score, Answer.Score(position, weights, confidence)) =>
        val mean = weights.zipWithIndex.map((p, i) => p * i).sum
        Vector(
          Option.when(weights.size != s.levels.size)(
            s"weighs ${weights.size} levels of ${s.levels.size}"
          ),
          sums(weights),
          Option.when(math.abs(position - mean) > Tolerance)(s"at $position, its mean $mean"),
          unit("confidence", confidence)
        ).flatten
      case (_: Question.YesNo, Answer.YesNo(yes)) => unit("p(yes)", yes).toVector
      case (q, a) => Vector(s"answered $a to $q")
    }
  }
}
