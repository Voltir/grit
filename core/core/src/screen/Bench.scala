package grit.core.screen

import java.util.Locale

import grit.core.either.Traversal
import grit.core.period.Probability

/** `checks`' examples, each screened by its signals alone. */
object Bench {

  /** The screen an example is asked under: no floor, `checks`' signals. */
  def screen(checks: Checks): Screen = Screen(Vector.empty, checks.signals, Vector.empty)

  /** The example at `at`, counted from 0 over the accepts then the refuses, or `None` outside
    * them. Text: read only in a step, recorded nowhere.
    */
  def example(checks: Checks, at: Int): Option[(kind: Benched.Kind, text: Screen.Text)] = {
    val all = checks.examples.accept.map((Benched.Kind.Accept, _)) ++
      checks.examples.refuse.map((Benched.Kind.Refuse, _))
    all.lift(at).map((kind, text) => (kind = kind, text = Screen.Text(text)))
  }

  /** The verdict over each example's `Screened`, in order. */
  def verdict(screened: Vector[(Benched.Kind, Screened)]): Benched =
    Benched(screened.zipWithIndex.map { case ((kind, s), i) =>
      val at = screened.take(i).count(_._1 == kind)
      val (must, p, signal) = deciding(s)
      Benched.Example(kind, at, signal, must, p)
    })

  /** `screened`'s deciding signal: its `must`, its answer and its position. */
  private def deciding(screened: Screened): (Must, Probability, Int) = {
    val signals = screened.signals.zip(screened.shape.musts).zipWithIndex.map {
      case ((p, must), at) => (must, p, at)
    }
    // Failing ones first, then the furthest past (or nearest) its threshold; the first on a tie.
    // Never the fallback: `Screen.read`, a Screened's only maker, refuses a shape of no signal.
    signals
      .sortBy((must, p, _) => (!Benched.fails(must, p), -Benched.margin(must, p)))
      .headOption
      .getOrElse((Must.Absent, Probability.Zero, 0))
  }
}

/** A bench's verdict: per example its [[Benched.Example]], and whether it `passed`: every
  * accept passed its screen and every refuse was refused (so, too, when it holds none).
  */
final case class Benched private[screen] (examples: Vector[Benched.Example]) extends caps.Pure {
  def passed: Boolean = examples.forall(_.held)

  /** The first example that failed, when one did. */
  def line: Option[Benched.Line] = examples.find(!_.held).map(Benched.Line(_))
}

object Benched {
  enum Kind extends caps.Pure {
    case Accept, Refuse
  }

  /** One example: its `kind`, its position `at` among its kind's (from 0), and its deciding
    * signal (from 0) with that signal's `must` and answer `p`. The deciding signal is the one
    * furthest toward failing: of the signals that fail, the one furthest past its threshold;
    * when none fails, the one nearest to it (an absent answer's distance below
    * [[Screen.Infers]], a present one's at or above [[Screen.States]]); the first on a tie.
    */
  final case class Example(kind: Kind, at: Int, signal: Int, must: Must, p: Probability)
      extends caps.Pure {

    /** Whether it went as its kind asks: an accept passed, a refuse was refused. */
    def held: Boolean = kind match {
      case Kind.Accept => !fails(must, p)
      case Kind.Refuse => fails(must, p)
    }
  }

  /** A failed example, by position counted from 1: "accept example 1 refused by signal 4
    * (0.63)", "refuse example 2 passed (signal 3, 0.21)".
    */
  final case class Line private[screen] (example: Example) extends caps.Pure {
    def said: String = {
      val e = example
      val p = "%.2f".formatLocal(Locale.ROOT, Probability.value(e.p))
      e.kind match {
        case Kind.Accept => s"accept example ${e.at + 1} refused by signal ${e.signal + 1} ($p)"
        case Kind.Refuse => s"refuse example ${e.at + 1} passed (signal ${e.signal + 1}, $p)"
      }
    }
  }

  /** A bench as [[write]] wrote its `examples`, read back with the row's `passed`; or why not,
    * naming positions alone, `passed` disagreeing with them among the reasons.
    */
  def read(passed: Boolean, json: ujson.Value): Either[String, Benched] =
    json.arrOpt
      .toRight("examples: not a list")
      .flatMap(entries =>
        Traversal.traverse(entries.toVector.zipWithIndex)((entry, i) =>
          example(entry).toRight(s"example ${i + 1}: not of its shape")
        )
      )
      .map(Benched(_))
      .filterOrElse(_.passed == passed, "passed disagrees with its examples")

  /** Its examples, `[{"at", "kind", "must", "p", "signal"}…]`. */
  def write(b: Benched): ujson.Value =
    ujson.Arr.from(b.examples.map { e =>
      ujson.Obj(
        "at" -> e.at,
        "kind" -> kindName(e.kind),
        "must" -> e.must.written,
        "p" -> Probability.value(e.p),
        "signal" -> e.signal
      )
    })

  /** Whether an answer `p` to a signal of `must` fails it. */
  private[screen] def fails(must: Must, p: Probability): Boolean = must match {
    case Must.Absent => p >= Screen.Infers
    case Must.Present => !(p >= Screen.States)
  }

  /** How far `p` lies toward failing past its threshold: positive or 0 when it fails as an
    * absent answer, positive when it fails as a present one.
    */
  private[screen] def margin(must: Must, p: Probability): Double = must match {
    case Must.Absent => Probability.value(p) - Probability.value(Screen.Infers)
    case Must.Present => Probability.value(Screen.States) - Probability.value(p)
  }

  private def kindName(k: Kind): String = k match {
    case Kind.Accept => "accept"
    case Kind.Refuse => "refuse"
  }

  private def example(entry: ujson.Value): Option[Example] =
    for {
      fields <- entry.objOpt
      if fields.keySet == Set("at", "kind", "must", "p", "signal")
      at <- fields.get("at").flatMap(_.numOpt).flatMap(whole)
      kind <- fields.get("kind").flatMap(_.strOpt).flatMap(k => Kind.values.find(kindName(_) == k))
      must <- fields.get("must").flatMap(_.strOpt).flatMap(m => Must.values.find(_.written == m))
      p <- fields.get("p").flatMap(_.numOpt).flatMap(Probability.of)
      signal <- fields.get("signal").flatMap(_.numOpt).flatMap(whole)
    } yield Example(kind, at, signal, must, p)

  private def whole(d: Double): Option[Int] =
    Option.when(d >= 0 && d.isWhole && d <= Int.MaxValue)(d.toInt)
}
