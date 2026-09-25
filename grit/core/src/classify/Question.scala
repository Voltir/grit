package grit.core.classify

/** The name a caller gives one question of a request; its answer comes back under it. The
  * classifier never shows it to the model.
  */
opaque type QuestionId = String

object QuestionId {
  def apply(value: String): QuestionId = value
  def value(id: QuestionId): String = id
}

/** One question a [[Classifier]] answers about a state, in the classifier's own terms:
  * option keys and descriptions are plain text, as they reach the model. [[Ask]] is the typed
  * way to build one.
  */
enum Question {

  /** Pick one of `criteria`: each an option key and what it means (`None` when the key says
    * enough). Keys are distinct and there are at least two.
    */
  case Choice(instructions: String, criteria: Vector[(String, Option[String])])

  /** Yes or no, answered as the probability of yes. `yes` and `no` say what each means. */
  case Noul(instructions: String, yes: Option[String], no: Option[String])
}

object Question {

  /** Whether `q` is a choice with fewer than two options, or with a key used twice. */
  def malformed(q: Question): Boolean = q match {
    case Choice(_, criteria) =>
      criteria.size < 2 || criteria.map(_._1).distinct.size != criteria.size
    case Noul(_, _, _) => false
  }
}

/** A classifier's answer to one [[Question]], of the question's kind. */
enum Answer {

  /** `probabilities` has one entry per option key, and they sum to 1 (within rounding).
    * `choice` is the most probable key. `confidence` is how concentrated `probabilities` is:
    * 1 when all of it is on one option, 0 when it is spread evenly; it is not the probability
    * of `choice`.
    */
  case Choice(choice: String, probabilities: Vector[(String, Double)], confidence: Double)

  /** The probability that the answer is yes. */
  case Noul(yes: Double)
}

object Answer {

  /** Jev's `confidence` for a Choice over `probabilities`: `(n · max − 1) / (n − 1)` for `n`
    * options, clamped to [0, 1]. Jev documents only the three-option case; this general form
    * matches every documented example to the docs' two-place rounding (`ClassifyTests`).
    */
  def confidence(probabilities: Vector[Double]): Double = {
    val n = probabilities.size
    if (n < 2) 1.0
    else {
      val top = probabilities.foldLeft(0.0)(math.max)
      math.min(1.0, math.max(0.0, (n * top - 1) / (n - 1)))
    }
  }

  /** A Choice answer over `probabilities` (keys in the question's order), normalised to sum
    * to 1, its choice the most probable key (the first, on a tie). `None` when there are no
    * keys or no probability mass.
    */
  def choice(probabilities: Vector[(String, Double)]): Option[Answer.Choice] = {
    val clean = probabilities.map((k, p) => (k, if (p.isNaN || p < 0) 0.0 else p))
    val total = clean.map(_._2).sum
    Option
      .when(clean.nonEmpty && total > 0)(clean.map((k, p) => (k, p / total)))
      .flatMap { normal =>
        normal
          .foldLeft(Option.empty[(String, Double)]) {
            case (None, kp) => Some(kp)
            case (Some(best), kp) => Some(if (kp._2 > best._2) kp else best)
          }
          .map((top, _) => Answer.Choice(top, normal, confidence(normal.map(_._2))))
      }
  }
}
