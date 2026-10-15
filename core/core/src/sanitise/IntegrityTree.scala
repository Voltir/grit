package grit.core.sanitise

import grit.core.flow.{Endorse, Tainted, Trusted}
import grit.core.period.Probability
import grit.core.visibility.Integrity

/** What the integrity tree decided of one text, and why, in reason codes. */
enum Verdict extends caps.Pure {

  /** To be raised from below `Outside` to it: nothing gated it; `noticed`, what the classifier
    * noticed that does not gate.
    */
  case Raised(noticed: Vector[Reason])

  /** As it came: nothing to raise, or not enough to raise it on. */
  case Kept(reasons: Vector[Reason])

  /** As it came, and suspected of steering its reader. */
  case Suspicious(reasons: Vector[Reason])
}

/** What the tree made of a text: the verdict, and `text`, at `Outside` when the verdict is
  * `Raised` and otherwise as it came.
  */
final case class Vetted(text: Tainted[String], verdict: Verdict)

/** Code's decision over what a text holds, what the classifier answered of it and where it came
  * from: whether a text believed below `Outside` may be believed at it.
  */
object IntegrityTree {

  // Changing it changes how a text is cut into windows, so which vet moves a run makes: a job
  // that vets must bump its version with it, or a run in flight replays to different moves.
  /** 3,000: the most characters of one text the tree judges, as many as the classifier's
    * thresholds were measured on.
    */
  val Bound: Int = 3000

  /** 0.3: a genre under it raises. */
  val RaiseBelow: Probability = Probability.clamped(0.3)

  /** 0.5: a genre at or above it is suspicious; any other answer at or above it is noticed. */
  val SuspectFrom: Probability = Probability.clamped(0.5)

  /** The verdict on a text believed at `source`, by `flags` and the classifier's `answers`
    * (`None`: not asked, unavailable or uncleared): `Suspicious` on any flag or a genre at least
    * [[SuspectFrom]]; `Raised` when `source` is below `Outside`, no flag fired and genre is under
    * [[RaiseBelow]]; otherwise `Kept`, [[Reason.Unjudged]] when `source` is below `Outside`,
    * nothing fired and there are no answers. Directive, identity, act and pressure at least
    * [[SuspectFrom]] add their reasons and never gate.
    */
  def decide(source: Integrity, flags: HardFlags, answers: Option[Suspicions]): Verdict = {
    val below = !source.dominates(Integrity.Outside)
    val unjudged = below && flags.fired.isEmpty
    val judged = answers.fold(if (unjudged) Vector(Reason.Unjudged) else Vector()) { s =>
      Vector(
        Option.when(s.directive >= SuspectFrom)(Reason.Directive),
        Option.when(s.identity >= SuspectFrom)(Reason.Identity),
        Option.when(s.act >= SuspectFrom)(Reason.Act),
        Option.when(s.pressure >= SuspectFrom)(Reason.Pressure),
        Option.when(s.genre >= RaiseBelow)(Reason.OutOfPlace)
      ).flatten
    }
    if (flags.fired.nonEmpty) Verdict.Suspicious(flags.fired ++ judged)
    else
      answers match {
        case Some(s) if s.genre >= SuspectFrom => Verdict.Suspicious(judged)
        case Some(s) if below && !(s.genre >= RaiseBelow) =>
          Verdict.Raised(judged)
        case _ => Verdict.Kept(judged)
      }
  }

  /** `text` judged by [[decide]], its flags measured from it ([[HardFlags.of]]), and raised by
    * `sanitiser` only when the verdict is `Raised`. `answers` are the classifier's to
    * [[Suspicions.questions]] about this `text`.
    */
  def vet(text: Tainted[String], answers: Option[Suspicions])(using
      sanitiser: Endorse.Sanitiser^
  ): Vetted = {
    val verdict = decide(text.integrity, HardFlags.of(Trusted.asIs(text).extract), answers)
    verdict match {
      case Verdict.Raised(_) => Vetted(sanitiser.sanitised(text), verdict)
      case Verdict.Kept(_) | Verdict.Suspicious(_) => Vetted(text, verdict)
    }
  }

  /** `text` cut into pieces of at most [[Bound]] characters, in order, joined back the same:
    * each ends at its last line end when that falls in its second half, and otherwise at
    * [[Bound]], never inside a character. Empty for an empty text.
    */
  def windows(text: String): Vector[String] = {
    @scala.annotation.tailrec
    def go(from: Int, done: Vector[String]): Vector[String] =
      if (from >= text.length) done
      else if (text.length - from <= Bound) done :+ text.substring(from)
      else {
        val limit = from + Bound
        val lineEnd = text.lastIndexOf('\n', limit - 1)
        val end =
          if (lineEnd >= from + Bound / 2) lineEnd + 1
          else if (Character.isHighSurrogate(text.charAt(limit - 1))) limit - 1
          else limit
        go(end, done :+ text.substring(from, end))
      }
    go(0, Vector.empty)
  }
}
