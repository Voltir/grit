package grit.turn

import grit.core.act.MoveName
import grit.core.classify.{Answered, Ask, ClassifierError, StateJson}
import grit.core.context.{Briefs, Results, Retrieved}
import grit.core.period.Probability

/** How a turn judges what its search found (ADR 0040): its first [[Judged]] of the
  * [[Recorded]] results, [[PerCall]] to a classifier call, each passing at [[Pass]] or more,
  * each shown cut to [[Chars]].
  */
object TurnRelevance {

  /** How many candidates one classifier call judges. */
  val PerCall = 10

  /** How many of the results, from the best, are judged; none after them is ever packed. */
  val Judged = 30

  /** How many of the results, from the best, a turn records. */
  val Recorded = 60

  /** The probability of yes at which a candidate passes. */
  val Pass: Probability = Probability.clamped(0.5)

  /** How many characters of the turn's message, and of each candidate, the judge is shown. */
  val Chars = 800

  /** Why a candidate found by the search, but gone before it was briefed, is unjudged. */
  val Gone = "gone before it was briefed"

  /** What a candidate was judged. */
  enum Verdict {

    /** Relevant, at `p`, [[Pass]] or more. */
    case Passed(p: Probability)

    /** Not relevant: `p`, under [[Pass]]. */
    case Rejected(p: Probability)

    /** Not judged, for `why`: the classifier's failure as it said it, or [[Gone]]. */
    case Unjudged(why: String)
  }

  /** What one call is shown: `{"message": ..., "candidates": [...]}`, the turn's own messages
    * and each candidate, as [[Briefs]] gives them.
    */
  final case class State(message: String, candidates: Vector[String])

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj("message" -> s.message, "candidates" -> ujson.Arr.from(s.candidates))
  )

  /** The judgments a turn makes of `results`, in order, each its move's name, `relevance-{n}`,
    * and the candidates it judges: the n-th [[PerCall]] of the first [[Judged]]; empty when
    * `results` has none there.
    */
  def batches(results: Results): Vector[(MoveName, Vector[Retrieved])] = {
    val judged = results.ranked.take(Judged).map(_._1)
    Names.zipWithIndex.map((name: MoveName, n: Int) =>
      name -> judged.slice(n * PerCall, (n + 1) * PerCall)
    )
  }

  /** `briefs` as one call shows them. */
  def state(briefs: Briefs): State = State(briefs.message, briefs.found.map(_._2))

  /** One yes/no question per candidate of `state`, in order, read as each one's probability
    * of yes; `None` when it has none, so nothing is asked.
    */
  def ask(state: State): Option[Ask[State, Vector[Probability]]] =
    state.candidates.indices.toVector.map(question) match {
      case first +: rest => Some(Ask.all(first, rest))
      case _ => None
    }

  /** Each of `batch`'s verdict, in its order, from `answered`, the answers to [[ask]] of
    * `briefs`' [[state]]: each found candidate passed or rejected by its probability, all
    * unjudged when `answered` is a failure, and one `briefs` left out unjudged as [[Gone]].
    */
  def verdicts(
      batch: Vector[Retrieved],
      briefs: Briefs,
      answered: Either[ClassifierError, Answered[Vector[Probability]]]
  ): Vector[(Retrieved, Verdict)] = {
    val judged: Map[Retrieved, Verdict] = answered match {
      case Left(error) => briefs.found.map(_._1 -> Verdict.Unjudged(error.said)).toMap
      case Right(a) =>
        briefs.found
          .map(_._1)
          .zip(a.value)
          .map((r: Retrieved, p: Probability) =>
            r -> (if (p >= Pass) Verdict.Passed(p) else Verdict.Rejected(p))
          )
          .toMap
    }
    batch.map(r => r -> judged.getOrElse(r, Verdict.Unjudged(Gone)))
  }

  /** The results `verdicts` admit to the window, in `results`' order: of the first [[Judged]],
    * each not rejected, whether passed or unjudged. Fail-open is the window's rule alone: a gate
    * on relevance reads `Passed`, never this.
    */
  def admitted(results: Results, verdicts: Vector[(Retrieved, Verdict)]): Results = {
    val rejected = verdicts.collect { case (r, Verdict.Rejected(_)) => r }.toSet
    Results(results.ranked.take(Judged).filterNot(found => rejected.contains(found._1)))
  }

  /** The text of the question asked of candidate `n`. */
  private def question(n: Int): Ask[State, Probability] =
    Ask
      .yesNo[State](
        s"Would candidates[$n] help answer the message? Yes only when it holds facts, " +
          "decisions or context a reply to the message needs; no when it is only on a " +
          "related subject.",
        None,
        None
      )
      .map(Probability.clamped)

  /** The name of each batch's move, in order: as many as [[Judged]] / [[PerCall]]. */
  private[turn] val Names: Vector[MoveName] =
    Vector(MoveName("relevance-0"), MoveName("relevance-1"), MoveName("relevance-2"))
}
