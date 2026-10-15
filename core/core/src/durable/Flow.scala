package grit.core.durable

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.boundary

import Flows.{At, Unreadable, Walk}

/** A durable program from an `I`, as a pure value: ends early with an `E` or passes an `O`,
  * its steps' bodies acting only through the `R` a run hands them.
  */
sealed trait Flow[-R, -I, +E, +O] {

  /** This flow run over a larger environment: each body is handed the `R` that `get` finds in
    * the `R2` a run hands it. It records exactly what this flow records.
    */
  // A member, not an extension: an extension infers `R` again from `get`, as a capturing type
  // that `r2` cannot be charged to (docs/capture-checking.md).
  def over[R2](get: (r2: R2^) -> R^{r2}): Flow[R2, I, E, O] =
    Flow.Over[R, R2, I, E, O](this, get)

  private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, O]

  /** Where `history`, from `at` on, leaves this flow run from `in`. */
  private[durable] def walk(
      in: I,
      at: At,
      history: Vector[StepRecord]
  ): Either[Unreadable, Walk[E, O]]
}

/** What a [[Flow.program]] makes its steps through, each recorded in order after the last. */
sealed trait Steps[+R] extends caps.ExclusiveCapability {

  /** `flow` from `in`, made next: how it ended, or what it passed. */
  def run[I, E, O](flow: Flow[R, I, E, O], in: I): Either[E, O]
}

object Flow {

  /** The flow of one step. */
  def step[R, I, O](s: Step[R, I, O]): Flow[R, I, Nothing, O] = One(s)

  /** Ends with the `Left` `f` passes, else passes its `Right`; records nothing of its own. */
  def mayEnd[R, I, E, O](f: Flow[R, I, E, Either[E, O]]): Flow[R, I, E, O] = MayEnd(f)

  /** `s`, then `f` over its input and output: ends with a `Left`, else passes the `Right`. */
  def decide[R, I, E, O, P](s: Step[R, I, O])(f: (I, O) -> Either[E, P]): Flow[R, I, E, P] =
    mayEnd(step(s).keeping.map((i, o) => f(i, o)))

  /** Its input, unchanged: no step, nothing recorded. */
  def pass[I]: Flow[Any, I, Nothing, I] = Pass[I]()

  /** `left` over a `Left`, `right` over a `Right`: both are in the description. */
  def either[R1, R2, A, B, E, O](
      left: Flow[R1, A, E, O],
      right: Flow[R2, B, E, O]
  ): Flow[R1 & R2, Either[A, B], E, O] =
    Split[R1 & R2, A, B, E, O](left, right)

  /** `f`, its end passed as a `Left`. */
  def attempt[R, I, E, O](f: Flow[R, I, E, O]): Flow[R, I, Nothing, Either[E, O]] = Attempt(f)

  /** `round` from its input, then from each `Left` it passes, until a `Right`, which the loop
    * passes; the state crosses rounds on the wire alone.
    */
  def loop[R, S, E, O](round: Flow[R, S, E, Either[S, O]]): Flow[R, S, E, O] = Loop(round)

  /** `one` over each of `items`, in order, their outputs gathered; ends with the first that
    * ends.
    */
  def each[R, I, E, A, O](items: I -> Vector[A])(one: Flow[R, A, E, O]): Flow[R, I, E, Vector[O]] =
    Each(items, one)

  /** `after` for a run that reaches here after the patch `name` shipped, `before` for one whose
    * history already holds a step here (ADR 0004); a taken patch records its marker
    * ([[StepRecord.marker]]).
    */
  def patch[R, I, E, O](name: String)(
      before: Flow[R, I, E, O],
      after: Flow[R, I, E, O]
  ): Flow[R, I, E, O] =
    Patch(name, before, after)

  /** A wait for a message sent to this workflow on `topic`, up to `within`: the message, or
    * `None`. The runtime records two steps, [[StepRecord.Received]] and then
    * [[StepRecord.Slept]]; a reader names both `label`.
    */
  def recv[I](
      label: StepName[I],
      topic: I -> String,
      within: I -> FiniteDuration
  ): Flow[Any, I, Nothing, Option[String]] =
    Recv(label, topic, within)

  /** Code that makes its steps one at a time through `steps`, each decided from the last's
    * result: a planner. A read runs `body` again over the history, so it may depend only on its
    * input and its steps' results, which its type enforces; a history its code would not have
    * made, as an older planner's may be, reads as `Diverged`. [[Flows.names]] sees only
    * `family`.
    */
  def program[R, I, O](family: String)(
      body: (steps: Steps[R]^) -> I ->{steps} O
  ): Flow[R, I, Nothing, O] =
    Program(family, body)

  extension [R, I, E, O](f: Flow[R, I, E, O]) {

    /** `f`, then `next` over what `f` passed. */
    def andThen[R2, E2 >: E, O2](next: Flow[R2, O, E2, O2]): Flow[R & R2, I, E2, O2] =
      AndThen[R & R2, I, O, E2, O2](f, next)

    /** `g` on the wire: records nothing, so pure. */
    def map[O2](g: O -> O2): Flow[R, I, E, O2] = Mapped(f, g)

    /** Its input beside its output. */
    def keeping: Flow[R, I, E, (I, O)] = Keeping(f)
  }

  /** The families `f` can record in the order a run reaches them, repeats kept, each marked
    * every run's when `every` says a finishing run reaches `f` and `f` records it whenever so.
    */
  private[durable] def families(f: Flow[?, ?, ?, ?], every: Boolean): Vector[Flows.Family] =
    f match {
      case One(s) => Vector(Flows.Family(s.named.family, s.named.keyed, every))
      case AndThen(first, next) => families(first, every) ++ families(next, every && !ends(first))
      case Mapped(g, _) => families(g, every)
      case MayEnd(g) => families(g, every)
      case Keeping(g) => families(g, every)
      case Pass() => Vector.empty
      case Split(left, right) => families(left, false) ++ families(right, false)
      case Attempt(g) => families(g, every)
      case Loop(round) => families(round, every)
      case Each(_, one) => families(one, false)
      case Patch(name, before, after) =>
        Flows.Family(StepRecord.marker(name), false, false) +:
          (families(before, false) ++ families(after, false))
      case Recv(label, _, _) => Vector(Flows.Family(label.family, label.keyed, every))
      case Over(g, _) => families(g, every)
      case Program(family, _) => Vector(Flows.Family(family, true, false))
    }

  /** Whether `f` may end early. */
  private def ends(f: Flow[?, ?, ?, ?]): Boolean = f match {
    case One(_) | Pass() | Attempt(_) | Recv(_, _, _) | Program(_, _) => false
    case MayEnd(_) => true
    case AndThen(first, next) => ends(first) || ends(next)
    case Mapped(g, _) => ends(g)
    case Keeping(g) => ends(g)
    case Split(left, right) => ends(left) || ends(right)
    case Loop(round) => ends(round)
    case Each(_, one) => ends(one)
    case Patch(_, before, after) => ends(before) || ends(after)
    case Over(g, _) => ends(g)
  }

  /** `walked` with `g` over the result it ended with, if it did. */
  private def ending[E, O, E2, O2](walked: Either[Unreadable, Walk[E, O]])(
      g: Either[E, O] -> Either[E2, O2]
  ): Either[Unreadable, Walk[E2, O2]] =
    walked.map {
      case Walk.Stopped(next, at) => Walk.Stopped(next, at)
      case Walk.Done(result, at) => Walk.Done(g(result), at)
    }

  private final case class One[R, I, O](step: Step[R, I, O]) extends Flow[R, I, Nothing, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[Nothing, O] =
      Right(step.perform(env, in))
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[Nothing, O]] = {
      val name = step.name(in)
      history.lift(at.step) match {
        case None => Right(Walk.Stopped(name, at))
        case Some(r) if r.name != name => Left(Unreadable.Diverged(at.step, name, r.name))
        case Some(r) =>
          r.output match {
            case None => Left(Unreadable.Unrecorded(at.step, name))
            case Some(raw) =>
              step.decode(raw) match {
                case Left(why) => Left(Unreadable.Undecodable(at.step, name, why))
                case Right(o) => Right(Walk.Done(Right(o), at.past(step.observe(in))))
              }
          }
      }
    }
  }

  private final case class AndThen[R, I, M, E, O](first: Flow[R, I, E, M], next: Flow[R, M, E, O])
      extends Flow[R, I, E, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, O] =
      first.run(env, in) match {
        case Left(e) => Left(e)
        case Right(m) => next.run(env, m)
      }
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] =
      first.walk(in, at, history).flatMap {
        case Walk.Stopped(name, k) => Right(Walk.Stopped(name, k))
        case Walk.Done(Left(e), k) => Right(Walk.Done(Left(e), k))
        case Walk.Done(Right(m), k) => next.walk(m, k, history)
      }
  }

  private final case class Mapped[R, I, E, M, O](f: Flow[R, I, E, M], g: M -> O)
      extends Flow[R, I, E, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, O] =
      f.run(env, in).map(g)
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] =
      ending(f.walk(in, at, history))(_.map(g))
  }

  private final case class MayEnd[R, I, E, O](f: Flow[R, I, E, Either[E, O]])
      extends Flow[R, I, E, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, O] =
      f.run(env, in).flatMap(identity)
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] =
      ending(f.walk(in, at, history))(_.flatMap(identity))
  }

  private final case class Keeping[R, I, E, O](f: Flow[R, I, E, O]) extends Flow[R, I, E, (I, O)] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, (I, O)] =
      f.run(env, in).map(o => (in, o))
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, (I, O)]] =
      ending(f.walk(in, at, history))(_.map(o => (in, o)))
  }

  private final case class Pass[I]() extends Flow[Any, I, Nothing, I] {
    private[durable] def run(env: Any^, in: I)(using d: Durable^): Either[Nothing, I] = Right(in)
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[Nothing, I]] = Right(Walk.Done(Right(in), at))
  }

  private final case class Split[R, A, B, E, O](left: Flow[R, A, E, O], right: Flow[R, B, E, O])
      extends Flow[R, Either[A, B], E, O] {
    private[durable] def run(env: R^, in: Either[A, B])(using d: Durable^): Either[E, O] =
      in match {
        case Left(a) => left.run(env, a)
        case Right(b) => right.run(env, b)
      }
    private[durable] def walk(
        in: Either[A, B],
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] =
      in match {
        case Left(a) => left.walk(a, at, history)
        case Right(b) => right.walk(b, at, history)
      }
  }

  private final case class Attempt[R, I, E, O](f: Flow[R, I, E, O])
      extends Flow[R, I, Nothing, Either[E, O]] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[Nothing, Either[E, O]] =
      Right(f.run(env, in))
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[Nothing, Either[E, O]]] =
      ending(f.walk(in, at, history))(Right(_))
  }

  private final case class Loop[R, S, E, O](round: Flow[R, S, E, Either[S, O]])
      extends Flow[R, S, E, O] {
    private[durable] def run(env: R^, in: S)(using d: Durable^): Either[E, O] = {
      @tailrec
      def from(s: S): Either[E, O] = round.run(env, s) match {
        case Left(e) => Left(e)
        case Right(Left(next)) => from(next)
        case Right(Right(o)) => Right(o)
      }
      from(in)
    }
    private[durable] def walk(
        in: S,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] = {
      @tailrec
      def from(s: S, k: At): Either[Unreadable, Walk[E, O]] = round.walk(s, k, history) match {
        case Left(why) => Left(why)
        case Right(Walk.Stopped(name, j)) => Right(Walk.Stopped(name, j))
        case Right(Walk.Done(Left(e), j)) => Right(Walk.Done(Left(e), j))
        case Right(Walk.Done(Right(Left(next)), j)) => from(next, j)
        case Right(Walk.Done(Right(Right(o)), j)) => Right(Walk.Done(Right(o), j))
      }
      from(in, at)
    }
  }

  private final case class Each[R, I, E, A, O](items: I -> Vector[A], one: Flow[R, A, E, O])
      extends Flow[R, I, E, Vector[O]] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, Vector[O]] = {
      val all = items(in)
      @tailrec
      def from(j: Int, done: Vector[O]): Either[E, Vector[O]] =
        all.lift(j) match {
          case None => Right(done)
          case Some(a) =>
            one.run(env, a) match {
              case Left(e) => Left(e)
              case Right(o) => from(j + 1, done :+ o)
            }
        }
      from(0, Vector.empty)
    }
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, Vector[O]]] = {
      val all = items(in)
      @tailrec
      def from(j: Int, k: At, done: Vector[O]): Either[Unreadable, Walk[E, Vector[O]]] =
        all.lift(j) match {
          case None => Right(Walk.Done(Right(done), k))
          case Some(a) =>
            one.walk(a, k, history) match {
              case Left(why) => Left(why)
              case Right(Walk.Stopped(name, n)) => Right(Walk.Stopped(name, n))
              case Right(Walk.Done(Left(e), n)) => Right(Walk.Done(Left(e), n))
              case Right(Walk.Done(Right(o), n)) => from(j + 1, n, done :+ o)
            }
        }
      from(0, at, Vector.empty)
    }
  }

  private final case class Patch[R, I, E, O](
      name: String,
      before: Flow[R, I, E, O],
      after: Flow[R, I, E, O]
  ) extends Flow[R, I, E, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[E, O] =
      if (d.patch(name)) after.run(env, in) else before.run(env, in)
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] = {
      val marker = StepRecord.marker(name)
      history.lift(at.step) match {
        case None => Right(Walk.Stopped(marker, at))
        case Some(r) if r.name == marker => after.walk(in, at.past(None), history)
        case Some(_) => before.walk(in, at, history)
      }
    }
  }

  private final case class Recv[I](
      label: StepName[I],
      topic: I -> String,
      within: I -> FiniteDuration
  ) extends Flow[Any, I, Nothing, Option[String]] {
    private[durable] def run(env: Any^, in: I)(using d: Durable^): Either[Nothing, Option[String]] =
      Right(d.recv(topic(in), within(in)))
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[Nothing, Option[String]]] = {
      val name = label(in)
      (history.lift(at.step), history.lift(at.step + 1)) match {
        case (None, _) => Right(Walk.Stopped(name, at))
        case (Some(r), _) if r.name != StepRecord.Received =>
          Left(Unreadable.Diverged(at.step, StepRecord.Received, r.name))
        case (Some(_), None) => Right(Walk.Stopped(name, at.past(None)))
        case (Some(_), Some(s)) if s.name != StepRecord.Slept =>
          Left(Unreadable.Diverged(at.step + 1, StepRecord.Slept, s.name))
        case (Some(r), Some(_)) => Right(Walk.Done(Right(r.output), at.past(None).past(None)))
      }
    }
  }

  private[durable] final case class Over[R, R2, I, E, O](
      f: Flow[R, I, E, O],
      get: (r2: R2^) -> R^{r2}
  ) extends Flow[R2, I, E, O] {
    private[durable] def run(env: R2^, in: I)(using d: Durable^): Either[E, O] =
      f.run(get(env), in)
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[E, O]] = f.walk(in, at, history)
  }

  private final case class Program[R, I, O](
      family: String,
      body: (steps: Steps[R]^) -> I ->{steps} O
  ) extends Flow[R, I, Nothing, O] {
    private[durable] def run(env: R^, in: I)(using d: Durable^): Either[Nothing, O] = {
      val steps = new Steps[R] {
        def run[I2, E2, O2](flow: Flow[R, I2, E2, O2], next: I2): Either[E2, O2] =
          flow.run(env, next)
      }
      Right(body(steps)(in))
    }

    // The program's code runs again, its steps read from the history in turn. Where the
    // history stops or cannot be read, the mark is set and the code is cut short; the mark,
    // not what the code returns, is the reading, so code that catches the cut cannot hide it.
    private[durable] def walk(
        in: I,
        at: At,
        history: Vector[StepRecord]
    ): Either[Unreadable, Walk[Nothing, O]] = {
      var cursor = at
      var mark: Option[Either[Unreadable, Walk[Nothing, O]]] = None
      val ran = boundary[Either[Unreadable, Walk[Nothing, O]]] {
        val steps = new Steps[R] {
          def run[I2, E2, O2](flow: Flow[R, I2, E2, O2], next: I2): Either[E2, O2] =
            mark match {
              case Some(m) => boundary.break(m)
              case None =>
                flow.walk(next, cursor, history) match {
                  case Right(Walk.Done(result, k)) =>
                    cursor = k
                    result
                  case Right(Walk.Stopped(name, k)) =>
                    val m = Right(Walk.Stopped(name, k))
                    mark = Some(m)
                    boundary.break(m)
                  case Left(why) =>
                    mark = Some(Left(why))
                    boundary.break(Left(why))
                }
            }
        }
        val out = body(steps)(in)
        Right(Walk.Done(Right(out), cursor))
      }
      mark.getOrElse(ran)
    }
  }
}
