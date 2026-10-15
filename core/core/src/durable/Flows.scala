package grit.core.durable

/** The interpreters of a [[Flow]]: [[run]] over a `Durable`, [[read]] over a history, and
  * [[names]] over the description alone.
  */
object Flows {

  /** `flow` from `in` on `d`, its bodies handed `env`: each body at most once, recorded under
    * its name, in order. A history that holds another name at a step's place is refused by the
    * runtime, which throws, ending the workflow in that error.
    */
  def run[R, I, E, O](flow: Flow[R, I, E, O], env: R^, in: I)(using d: Durable^): Either[E, O] =
    flow.run(env, in)

  /** `flow` read over `history` from `in`, with no environment: where a run stands (the next
    * step as a watcher names it, a wait by its label) or how it ended, and what its steps'
    * inputs showed. No step's body runs; a program's own code does, over the results the history
    * holds. A history that ends where a patch is reads as taking it, as a run reaching it now
    * would: its next step is the patch's marker.
    */
  def read[R, I, E, O](
      flow: Flow[R, I, E, O],
      in: I,
      history: Vector[StepRecord]
  ): Either[Unreadable, Reading[E, O]] =
    flow.walk(in, At(0, Vector.empty), history).map {
      case Walk.Stopped(next, at) => Reading(Read.Next(next, at.step), at.observed)
      case Walk.Done(result, at) => Reading(Read.Ended(result, at.step), at.observed)
    }

  /** The families `flow` can record, in the order a run first reaches each, each listed once. A
    * wait is listed by its label, a taken patch by its marker, and a program by its family
    * alone, as keyed and never every run's.
    */
  def names(flow: Flow[?, ?, ?, ?]): Vector[Family] = {
    val reached = Flow.families(flow, true)
    reached.map(_.name).distinct.map { name =>
      val all = reached.filter(_.name == name)
      Family(name, all.exists(_.keyed), all.exists(_.every))
    }
  }

  /** A name a flow records, or the family of the names it records under keys.
    *
    * @param every
    *   recorded by every run that finishes, whether it ends early or passes: a step in a loop's
    *   first round is; a step in an arm of `either`, in either branch of a patch or its marker,
    *   in an `each` (whose items may be none), or after a flow that may end early, is not
    */
  final case class Family(name: String, keyed: Boolean, every: Boolean)

  /** A place in a history, and what the steps before it showed. */
  private[durable] final case class At(step: Int, observed: Vector[Observed]) {

    /** The place after this one's record, which showed `shown`. */
    def past(shown: Option[Observed]): At = At(step + 1, observed ++ shown)
  }

  /** Where a history leaves a flow, as a read walks it. */
  private[durable] enum Walk[+E, +O] {

    /** Before `next`, at `at`. */
    case Stopped(next: String, at: At) extends Walk[Nothing, Nothing]

    /** Ended so, at `at`. */
    case Done(result: Either[E, O], at: At)
  }

  /** Where a history leaves a flow, and what the inputs of the steps it holds showed, in order. */
  final case class Reading[+E, +O](at: Read[E, O], observed: Vector[Observed])

  /** Where a history leaves a flow. */
  enum Read[+E, +O] {

    /** The history ends before `step`, which a run records next as its `at`-th (from 0). */
    case Next(step: String, at: Int) extends Read[Nothing, Nothing]

    /** The flow ended so after the history's first `steps` records. */
    case Ended(result: Either[E, O], steps: Int)
  }

  /** Why a history cannot be read as a flow's. */
  enum Unreadable {

    /** The history holds `recorded` as its `at`-th step, where the flow records `name`. */
    case Diverged(at: Int, name: String, recorded: String)

    /** The `at`-th step's output does not read under its codec, for `why`. */
    case Undecodable(at: Int, name: String, why: String)

    /** The `at`-th step recorded no output: it threw. */
    case Unrecorded(at: Int, name: String)
  }
}
