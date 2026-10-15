package grit.core.durable

import grit.core.id.WorkflowId

/** Steps and flows the flow suites share, over an environment that tags what its bodies return. */
object FlowFixtures {

  /** What the suites' bodies act through: a tag they put in what they return. */
  final class Env(val tag: String)

  /** A larger environment, holding the one a flow's bodies act through beside another. */
  final class Wide(val inner: Env, val other: Env)

  /** The fixed name `name`, which the suites never take from the runtime's. */
  def named(name: String): StepName[Any] =
    StepName(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The family `family`, keyed by `key`. */
  def keyed[I](family: String)(key: I -> String): StepName[I] =
    StepName.keyed(family)(key).fold(e => throw new java.lang.AssertionError(e), identity)

  /** A step over `name`, its output its env's tag, `name` and its input. */
  def tagged(name: String): Step[Env, String, String] =
    Step[Env, String, String](named(name))(r => in => s"${r.tag}$name($in)")

  val a: Step[Env, String, String] = tagged("a")
  val b: Step[Env, String, String] = tagged("b")

  /** A step over a list's next item, keyed by the item. */
  val item: Step[Env, String, String] =
    Step[Env, String, String](keyed[String]("item")(i => i))(r => in => s"${r.tag}($in)")

  /** Ends with the item "stop", else passes what `item` returned. */
  val stopping: Flow[Env, String, String, String] =
    Flow.decide(item)((in, out) => if (in == "stop") Left(s"stopped at $out") else Right(out))

  /** A step of `family` keyed by its input's length, appending its env's tag. */
  def grown(family: String): Step[Env, String, String] =
    Step[Env, String, String](keyed[String](family)(_.length.toString))(r => in => in + r.tag)

  /** Grows its input by `grown("round")` until it is three long. */
  val growing: Flow[Env, String, Nothing, String] =
    Flow.loop(Flow.step(grown("round")).map(s => if (s.length < 3) Left(s) else Right(s)))

  /** A planner that grows its input by `grown("grow")`, one step at a time, until it is three
    * long.
    */
  val planner: Flow[Env, String, Nothing, String] =
    Flow.program[Env, String, String]("plan") { steps => start =>
      def from(s: String): String =
        steps.run(Flow.step(grown("grow")), s) match {
          case Right(next) if next.length < 3 => from(next)
          case Right(done) => done
        }
      from(start)
    }

  /** `flow` run once from `in` over `env` on `durable`: what it came to, and what it recorded. */
  def ran[R, I, E, O](
      flow: Flow[R, I, E, O],
      env: R^,
      in: I,
      durable: InMemoryDurable = new InMemoryDurable()
  ): (String, Vector[StepRecord]) = {
    val id = WorkflowId("flow")
    val out = durable.run(id)(_ => Flows.run(flow, env, in).toString)
    (out, durable.history(id).map(recorded))
  }

  /** `step` as a reader of a history sees it. */
  def recorded(step: InMemoryDurable.Step): StepRecord = step match {
    case InMemoryDurable.Step(name, InMemoryDurable.Outcome.Output(v)) =>
      StepRecord(name, Some(v), None)
    case InMemoryDurable.Step(name, _) => StepRecord(name, None, None)
  }
}
