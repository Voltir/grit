package grit.core.durable

/** The interpreters of a [[Flow]]: [[run]] over a `Durable`. */
object Flows {

  /** `flow` from `in` on `d`, its bodies handed `env`: each body at most once, recorded under
    * its name, in order. A history that holds another name at a step's place is refused by the
    * runtime, which throws, ending the workflow in that error.
    */
  def run[R, I, E, O](flow: Flow[R, I, E, O], env: R^, in: I)(using d: Durable^): Either[E, O] =
    flow.run(env, in)
}
