package grit.core.durable

import scala.compiletime.ops.string.Matches
import scala.compiletime.{constValue, constValueOpt, error}

import grit.core.store.Tx
import grit.core.visibility.Subject

/** What a step is recorded as: `name`, or `family:key` for the key its input gives. Made from a
  * literal alone; one the runtime keeps for itself (`DBOS.…`) does not compile.
  */
sealed trait StepName[-I] {

  /** The name recorded for `in`. */
  def apply(in: I): String

  /** The fixed name, or the family a key completes. */
  def family: String

  /** Whether the recorded name is the family and a key. */
  def keyed: Boolean
}

object StepName {

  /** The fixed name `name`. */
  inline def apply[N <: String & Singleton](inline name: N): StepName[Any] = {
    unreserved[N]
    Fixed(name)
  }

  /** `family:key` over an `I`: `keyed[I](family)(key)`, the key from the input. */
  def keyed[I]: Keying[I] = new Keying[I]

  /** A keyed name over an `I`, waiting for its family. */
  final class Keying[I] private[StepName] () {

    /** The family `family`, waiting for its key. */
    inline def apply[N <: String & Singleton](inline family: N): Family[I] = {
      unreserved[N]
      familyOf[I](family)
    }
  }

  private def familyOf[I](family: String): Family[I] = new Family[I](family)

  /** A keyed name's family, waiting for its key. */
  final class Family[I] private[StepName] (family: String) {

    /** `family:key`, the key from the input. */
    def apply(key: I -> String): StepName[I] = Keyed(family, key)
  }

  /** Refuses, at compile time, a name that is not a literal, and the prefix of every name the
    * runtime records for itself: a patch's marker, a wait.
    */
  private inline def unreserved[N <: String]: Unit =
    inline constValueOpt[N] match {
      case None => error("a step name is a literal")
      case Some(_) =>
        inline if (constValue[Matches[N, "DBOS\\..*"]])
          error("'" + constValue[N] + "' is a name the runtime keeps for itself")
    }

  private final case class Fixed(family: String) extends StepName[Any] {
    def apply(in: Any): String = family
    def keyed: Boolean = false
  }

  private final case class Keyed[I](family: String, key: I -> String) extends StepName[I] {
    def apply(in: I): String = s"$family:${key(in)}"
    def keyed: Boolean = true
  }
}

/** A fact a step's input shows a reader of its history; each workflow declares its own cases. */
trait Observed extends caps.Pure

/** One durable operation, as a pure value: recorded under its name, its output journaled, its
  * body acting only through the environment `R` a run hands it. Run by [[Flows.run]], read by
  * [[Flows.read]], which runs no step's body.
  */
sealed trait Step[-R, -I, +O] {

  /** The name the step is recorded under, from the input it runs over. */
  def name(in: I): String

  private[durable] def named: StepName[I]

  /** The body, once, on `d`, handed `env`, recorded under [[name]]. */
  private[durable] def perform(env: R^, in: I)(using d: Durable^): O

  /** `raw`, a recorded output, read under this step's codec. */
  private[durable] def decode(raw: String): Either[String, O]

  /** What the step's input shows a reader. */
  private[durable] def observe(in: I): Option[Observed]
}

object Step {

  /** A plain step: `body` over the wire value, its output recorded; its input shown to a reader
    * as `observe` says.
    */
  def apply[R, I, O](name: StepName[I], observe: I -> Option[Observed] = (_: I) => None)(
      body: (r: R^) -> I ->{r} O
  )(using j: Journaled[O]): Step[R, I, O] =
    Plain[R, I, O](name, observe, body, j)

  /** As [[apply]], in one transaction opened for `subject` and committed with its record; the
    * `Tx` never leaves `body`. A failed statement aborts it and the step throws, which the
    * runtime keeps as the workflow's result: a body that may race a key works under a savepoint.
    */
  def transact[R, I, O](
      name: StepName[I],
      subject: I -> Subject,
      observe: I -> Option[Observed] = (_: I) => None
  )(body: (r: R^) -> I ->{r} Tx^ ?->{r} O)(using j: Journaled[O]): Step[R, I, O] =
    Transact[R, I, O](name, subject, observe, body, j)

  private final case class Plain[R, I, O](
      named: StepName[I],
      shown: I -> Option[Observed],
      body: (r: R^) -> I ->{r} O,
      journaled: Journaled[O]
  ) extends Step[R, I, O] {
    def name(in: I): String = named(in)
    private[durable] def perform(env: R^, in: I)(using d: Durable^): O =
      d.step(name(in))(() => body(env)(in))(using journaled)
    private[durable] def decode(raw: String): Either[String, O] = journaled.decode(raw)
    private[durable] def observe(in: I): Option[Observed] = shown(in)
  }

  private final case class Transact[R, I, O](
      named: StepName[I],
      subject: I -> Subject,
      shown: I -> Option[Observed],
      body: (r: R^) -> I ->{r} Tx^ ?->{r} O,
      journaled: Journaled[O]
  ) extends Step[R, I, O] {
    def name(in: I): String = named(in)
    private[durable] def perform(env: R^, in: I)(using d: Durable^): O =
      d.transact(name(in), subject(in))(body(env)(in))(using journaled)
    private[durable] def decode(raw: String): Either[String, O] = journaled.decode(raw)
    private[durable] def observe(in: I): Option[Observed] = shown(in)
  }
}
