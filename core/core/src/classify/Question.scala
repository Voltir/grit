package grit.core.classify

/** One question as a classifier receives it, in plain text as it reaches the model. Built
  * through [[Ask]], or as itself ([[Question.choice]], `YesNo`) for [[Ask.answer]].
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
