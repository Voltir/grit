package grit.eval.harness.log

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Question}
import grit.core.message.Usage
import grit.eval.harness.corpus.{CaseId, Digest, Failure}

/** Which of a case's calls a row is: triage's question, or stitching's. */
enum Suite {
  case Triage, Stitch
}

object Suite {

  /** `s`'s written name: `triage` or `stitch`. */
  def written(s: Suite): String = s.toString.toLowerCase

  /** The suite written `name`; `None` for no suite's. */
  def read(name: String): Option[Suite] = values.find(written(_) == name)
}

/** One request of a run, generic over what came back (`A`: a classifier's answers here, a
  * provider's reply's measures later): the case and repeat, the request's digest and cache
  * key, the model requested and the one reported (`None` unless answered), the outcome, what
  * it consumed, how long it took, and whether the cache answered (its usage and latency then
  * the original call's; its cost not spent again).
  */
final case class Row[A](
    suite: Suite,
    id: CaseId,
    repeat: Int,
    request: Digest,
    key: CacheKey,
    requested: String,
    reported: Option[String],
    outcome: Outcome[A],
    usage: Usage,
    latency: FiniteDuration,
    cached: Boolean
)

/** What became of one request of a run. */
enum Outcome[+A] {

  /** It was answered, with this. */
  case Answered(value: A)

  /** It was asked and failed, of this kind (never the words of why). */
  case Failed(failure: Failure)

  /** It was not asked: the run's spend cap stopped it. */
  case Skipped
}

/** A classifier's answer to one question as a log keeps it: by position, never by key, so no
  * question's words reach the log. The question it answers, which the request's digest pins,
  * names the keys ([[Weights.answer]]).
  */
enum Weights {

  /** A choice: the index of the key `chosen`, and each key's probability, in the question's
    * key order.
    */
  case Choice(chosen: Int, probabilities: Vector[Double], confidence: Double)

  /** A yes/no question: the probability of yes. */
  case YesNo(yes: Double)
}

object Weights {

  given Codec[Vector[Weights]] = LogJson.weights

  /** `a`, an answer to `q`, by position; `None` when it is not of `q`'s kind, or names a key
    * `q` does not have.
    */
  def of(q: Question, a: Answer): Option[Weights] = (q, a) match {
    case (c: Question.Choice, Answer.Choice(choice, probabilities, confidence)) =>
      val keys = c.keys.map(_.name)
      Option
        .when(keys.contains(choice))(keys.indexOf(choice))
        .map(i =>
          Choice(
            i,
            keys.map(k => probabilities.find(_.key == k).fold(0.0)(_.probability)),
            confidence
          )
        )
    case (_: Question.YesNo, Answer.YesNo(yes)) => Some(YesNo(yes))
    case _ => None
  }

  /** `w` as an answer to `q`, its keys `q`'s; `None` when it is not of `q`'s kind, or its
    * positions are not `q`'s keys'.
    */
  def answer(q: Question, w: Weights): Option[Answer] = (q, w) match {
    case (c: Question.Choice, Choice(chosen, probabilities, confidence)) =>
      val keys = c.keys.map(_.name)
      Option
        .when(probabilities.size == keys.size)(keys.lift(chosen))
        .flatten
        .map(choice =>
          Answer.Choice(
            choice,
            keys.zip(probabilities).map(Answer.Weight(_, _)),
            confidence
          )
        )
    case (_: Question.YesNo, YesNo(yes)) => Some(Answer.YesNo(yes))
    case _ => None
  }
}
