package grit.core.classify

/** One question as a classifier receives it, in plain text as it reaches the model. Built
  * through [[Ask]], or as itself ([[Question.choice]], [[Question.score]], `YesNo`) for
  * [[Ask.answer]].
  */
sealed trait Question

object Question {

  /** A choice among `first`, `second` and `rest`, in that order; the key two of them share,
    * otherwise.
    */
  def choice(
      instructions: String,
      first: Key,
      second: Key,
      rest: Key*
  ): Either[Ask.DuplicateKey, Question.Choice] = {
    val keys = (first +: second +: rest.toVector).map(_.name)
    keys
      .diff(keys.distinct)
      .headOption
      .map(Ask.DuplicateKey(_))
      .toLeft(Choice(instructions, first, second, rest.toVector))
  }

  /** Pick one of the keys. No two share a name. */
  final case class Choice private[classify] (
      instructions: String,
      first: Key,
      second: Key,
      rest: Vector[Key]
  ) extends Question {
    def keys: Vector[Key] = first +: second +: rest
  }

  /** Where the state falls among the levels `first`, `second`, `rest`, in that order, from
    * the least to the most; at most [[MaxLevels]] in all, as Jev accepts.
    */
  def score(
      instructions: String,
      first: String,
      second: String,
      rest: String*
  ): Either[Question.TooManyLevels, Question.Score] = {
    val count = 2 + rest.size
    Either.cond(
      count <= MaxLevels,
      Score(instructions, first, second, rest.toVector),
      TooManyLevels(count)
    )
  }

  /** 10. */
  val MaxLevels: Int = 10

  /** More than [[MaxLevels]] levels: `count`. */
  final case class TooManyLevels(count: Int)

  /** A score among the levels, the first the least. */
  final case class Score private[classify] (
      instructions: String,
      first: String,
      second: String,
      rest: Vector[String]
  ) extends Question {
    def levels: Vector[String] = first +: second +: rest
  }

  /** Yes or no, answered as the probability of yes. `yes` and `no` say what each means. */
  final case class YesNo(instructions: String, yes: Option[String], no: Option[String])
      extends Question

  /** An option of a choice: the key the model sees and what it means (`None` when the key
    * says enough).
    */
  final case class Key(name: String, description: Option[String])
}

/** A classifier's answer to one [[Question]], of the question's kind. */
enum Answer {

  /** As the classifier reported it, unchecked: `choice` should name a key and `probabilities`
    * weigh the keys; [[Ask.choice]] reads it into a [[Decision]]. `confidence` is how
    * concentrated the weights are: 1 when all on one key, 0 when spread evenly; it is not the
    * probability of `choice`.
    */
  case Choice(choice: String, probabilities: Vector[Answer.Weight], confidence: Double)

  /** As the classifier reported it, unchecked: `score` should be a position along the
    * levels (0 the first, n−1 the last, between two when its weight spreads), and
    * `probabilities` should hold one weight per level, in order; [[Ask.score]] reads it into
    * a [[Scored]]. `confidence` is how concentrated the weights are, as the classifier
    * reported it: 1 when all on one level, falling as the weight spreads further from the
    * likeliest.
    */
  case Score(score: Double, probabilities: Vector[Double], confidence: Double)

  /** The probability that the answer is yes. */
  case YesNo(yes: Double)
}

object Answer {

  final case class Weight(key: String, probability: Double)

  /** Jev's `confidence` for a Choice over `probabilities`: `(n · max − 1) / (n − 1)` for `n`
    * options, clamped to [0, 1]. Jev documents only the three-option case; this general form
    * matches every documented example to the docs' two-place rounding (`ClassifyTests`).
    */
  def confidence(probabilities: Vector[Double]): Double = {
    val n = probabilities.size
    probabilities.maxOption.filter(_ => n >= 2).fold(1.0) { top =>
      math.min(1.0, math.max(0.0, (n * top - 1) / (n - 1)))
    }
  }

  /** grit's reading of Jev's confidence for a Score, for an answer no Jev reply gave (a
    * stub's): `max(0, 1 − Σ pᵢ·|i − m| / u)`, where `m` is the likeliest level (the first,
    * on a tie) and `u` is the same sum for even weights; 1 for one level. It matches Jev's two
    * documented Score answers to their rounding (`ClassifyTests`); whether Jev reads the
    * likeliest level at an end as surer, as this does, is unconfirmed. Not Jev's own figure:
    * an answer Jev gave keeps the confidence it reported.
    */
  def scoreConfidence(probabilities: Vector[Double]): Double = {
    val n = probabilities.size
    val likeliest = probabilities.zipWithIndex.maxByOption(_._1).fold(0)(_._2)
    def spread(weights: Vector[Double]): Double =
      weights.zipWithIndex.map((p, i) => p * math.abs(i - likeliest)).sum
    val even = spread(Vector.fill(n)(1.0 / n))
    if (n < 2) 1.0 else math.max(0.0, 1 - spread(probabilities) / even)
  }

  /** A Choice over `probabilities` (keys in the question's order), normalised to sum to 1, a
    * NaN, infinite or negative one counted as 0; its choice the most probable key (the first,
    * on a tie). `None` when there is no probability mass.
    */
  def choice(probabilities: Vector[Weight]): Option[Answer.Choice] = {
    val clean = probabilities.map(w => w.copy(probability = mass(w.probability)))
    val total = clean.map(_.probability).sum
    val normal = clean.map(w => w.copy(probability = w.probability / total))
    Option
      .when(total > 0)(normal)
      .flatMap(_.maxByOption(_.probability))
      .map(top => Answer.Choice(top.key, normal, confidence(normal.map(_.probability))))
  }

  /** `p` as probability mass: itself, or 0 when NaN, infinite or negative. */
  private[classify] def mass(p: Double): Double = if (p >= 0 && !p.isInfinite) p else 0.0
}
