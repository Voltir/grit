package grit.core.durable

import scala.caps.unsafe.untrackedCaptures
import scala.concurrent.duration.FiniteDuration

import grit.core.id.WorkflowId
import grit.core.store.Tx
import grit.dbos.sql.TestTx

/** An in-memory stand-in for DBOS's workflow semantics, for tests of code written against
  * [[Durable]], keeping [[DurableContract]]: one journal per workflow id, kept across runs
  * of the same id.
  *
  *   - A workflow that returned is not run again; its output is returned.
  *   - A workflow that threw is not run again either; its exception is rethrown, as DBOS
  *     rethrows an `ERROR` workflow's recorded error.
  *   - A run after a crash runs the body again. Each recorded step returns its recorded
  *     output, or rethrows its recorded exception, without running its body.
  *   - A step whose name differs from the one recorded at its position throws
  *     [[InMemoryDurable.UnexpectedStep]].
  *   - A step body that throws [[InMemoryDurable.Crash]] is left unrecorded, as if the
  *     process died inside it.
  *   - A stream write outside a step body throws [[InMemoryDurable.WriteOutsideStep]]:
  *     DBOS records it as an operation, shifting every later step's position. The one
  *     place it is stricter than DBOS ([[Divergence.WriteOutsideStep]]).
  *   - `patch` and `deprecatePatch` record and read DBOS's own marker,
  *     `DBOS.patch-{name}`, so a history captured from Postgres replays here unchanged.
  *   - `recv` never waits: it takes the oldest message [[send]] left, or none, and records
  *     it as DBOS does, `DBOS.recv` (its output the message, or none) then `DBOS.sleep`.
  *
  * `transact` hands its body a [[TestTx]], so pair it with in-memory stores. Their writes
  * are not rolled back when the body throws.
  *
  * A patch named in `unpatched` is never taken where the journal has nothing yet: its
  * `patch` is false, as for a workflow that passed the change before it shipped. For
  * testing the old branch of a patch.
  */
final class InMemoryDurable(unpatched: Set[String] = Set.empty) {
  import InMemoryDurable.*

  private enum Recorded {
    case Output(value: String)
    case Threw(error: Throwable)
    case Marker
  }

  @untrackedCaptures
  private var journals = Map.empty[WorkflowId, Vector[(String, Recorded)]]

  @untrackedCaptures
  private var outputs = Map.empty[WorkflowId, Either[Throwable, String]]

  /** Each workflow's streams, by key: every piece written, in order, across its runs. */
  @untrackedCaptures
  private var streams = Map.empty[(WorkflowId, String), Vector[String]]

  /** The pieces written to stream `key` of workflow `id`, by every run, in order. */
  def streamed(id: WorkflowId, key: String): Vector[String] =
    streams.getOrElse((id, key), Vector.empty)

  private def append(id: WorkflowId, key: String, piece: String): Unit =
    if (!stepping(id)) throw WriteOutsideStep(id, key)
    else streams = streams.updated((id, key), streamed(id, key) :+ piece)

  /** The workflows whose step body is running now. Kept here, not on the run, so a
    * [[StreamWriter]] need not capture the [[Durable]] it came from.
    */
  @untrackedCaptures
  private var stepping = Set.empty[WorkflowId]

  /** Each workflow's messages not yet received, by topic, oldest first. */
  @untrackedCaptures
  private var mail = Map.empty[(WorkflowId, String), Vector[String]]

  /** The idempotency keys of every message sent. */
  @untrackedCaptures
  private var sent = Set.empty[String]

  /** The workflows a [[run]] or [[replay]] has started. */
  @untrackedCaptures
  private var started = Set.empty[WorkflowId]

  /** Sends `message` to workflow `id` on `topic`, for its [[Durable.recv]]; ignored when a
    * message was already sent under `key`, as DBOS's `send` ignores a repeated idempotency
    * key. Throws [[InMemoryDurable.NoSuchWorkflow]] if no [[run]] or [[replay]] of `id` has
    * started, as DBOS refuses a send to a workflow it has no record of.
    */
  def send(id: WorkflowId, topic: String, message: String, key: Option[String] = None): Unit =
    if (!started.contains(id)) throw NoSuchWorkflow(id)
    else if (!key.exists(sent.contains)) {
      key.foreach(k => sent = sent + k)
      mail = mail.updated((id, topic), mail.getOrElse((id, topic), Vector.empty) :+ message)
    }

  /** The messages sent to workflow `id` on `topic` that it has not received, oldest first. */
  def unreceived(id: WorkflowId, topic: String): Vector[String] =
    mail.getOrElse((id, topic), Vector.empty)

  /** How far the last run of each workflow got through its journal. */
  @untrackedCaptures
  private var reached = Map.empty[WorkflowId, Int]

  /** Runs the workflow `id`, or returns its output, or rethrows its exception, if a run of
    * it already returned or threw.
    */
  def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): String = {
    started += id
    outputs.get(id) match {
      case Some(Right(output)) => output
      case Some(Left(error)) => throw error
      case None =>
        val output =
          try body(id)(using new Run(id))
          catch {
            case crash: Crash => throw crash
            case e: Exception =>
              outputs = outputs.updated(id, Left(e))
              throw e
          }
        outputs = outputs.updated(id, Right(output))
        output
    }
  }

  /** The names of the steps recorded for `id`, in order. */
  def recordedSteps(id: WorkflowId): Vector[String] =
    journals.getOrElse(id, Vector.empty).map(_._1)

  /** The journal of `id`, in the form a history fixture keeps. */
  def history(id: WorkflowId): Vector[Step] =
    journals.getOrElse(id, Vector.empty).map {
      case (name, Recorded.Output(value)) => Step(name, Outcome.Output(value))
      case (name, Recorded.Threw(recorded: RecordedError)) =>
        Step(name, Outcome.Threw(recorded.message))
      case (name, Recorded.Threw(error)) => Step(name, Outcome.Threw(Option(error.getMessage)))
      case (name, Recorded.Marker) => Step(name, Outcome.Marker)
    }

  /** Runs `body` as a resumption of a workflow that recorded `steps`, as the current build
    * would after a restart. `Left` names how the body failed to follow the history: a step
    * named differently at a recorded position, an output it cannot read, or an end before
    * the last recorded step. A body that goes on past the history runs its new steps.
    */
  def replay(id: WorkflowId, steps: Vector[Step])(
      body: WorkflowId => Durable^ ?=> String
  ): Either[String, String] = {
    val seeded = steps.map {
      case Step(name, Outcome.Output(value)) => name -> Recorded.Output(value)
      case Step(name, Outcome.Threw(message)) => name -> Recorded.Threw(new RecordedError(message))
      case Step(name, Outcome.Marker) => name -> Recorded.Marker
    }
    journals = journals.updated(id, seeded)
    outputs = outputs.removed(id)
    val outcome =
      try Right(run(id)(body))
      catch {
        case e: UnexpectedStep => Left(e.getMessage)
        case e: UnreadableJournal => Left(e.getMessage)
        case e: RecordedError => Right(s"threw the recorded error: ${e.getMessage}")
      }
    outcome.flatMap { output =>
      val got = reached.getOrElse(id, 0)
      if (got < steps.size)
        Left(s"ended after $got of ${steps.size} recorded steps; next was '${steps(got).name}'")
      else Right(output)
    }
  }

  private final class Run(workflowId: WorkflowId) extends Durable {

    @untrackedCaptures
    private var next = 0

    def step[A: Journaled](name: String)(body: () => A): A =
      record(name, () => body())

    def stream(key: String): StreamWriter = {
      val id = workflowId
      new StreamWriter {
        def write(piece: String): Unit = append(id, key, piece)
      }
    }

    def transact[A: Journaled](name: String)(body: (Tx^) ?=> A): A =
      record(name, () => body(using TestTx.fake))

    def patch(name: String): Boolean = {
      val marker = patchMarker(name)
      journal.lift(next) match {
        case None if unpatched.contains(name) => false
        case None =>
          journals = journals.updated(workflowId, journal :+ (marker -> Recorded.Marker))
          advance()
          true
        case Some((recorded, _)) if recorded == marker =>
          advance()
          true
        case Some(_) => false
      }
    }

    def deprecatePatch(name: String): Unit =
      journal.lift(next) match {
        case Some((recorded, _)) if recorded == patchMarker(name) => advance()
        case _ => ()
      }

    def recv(topic: String, timeout: FiniteDuration): Option[String] = {
      val position = next
      val got = journal.lift(position) match {
        case Some((Recv, Recorded.Output(message))) => Some(message)
        case Some((Recv, Recorded.Marker)) => None
        case Some((Recv, Recorded.Threw(error))) => throw error
        case Some((recorded, _)) => throw UnexpectedStep(workflowId, position, Recv, recorded)
        case None =>
          val waiting = unreceived(workflowId, topic)
          mail = mail.updated((workflowId, topic), waiting.drop(1))
          val first = waiting.headOption
          val recorded = first.fold(Recorded.Marker)(Recorded.Output(_))
          journals = journals.updated(workflowId, journal :+ (Recv -> recorded))
          first
      }
      advance()
      journal.lift(next) match {
        case Some((Sleep, _)) => ()
        case Some((recorded, _)) => throw UnexpectedStep(workflowId, next, Sleep, recorded)
        case None =>
          val end = Recorded.Output(timeout.toMillis.toString)
          journals = journals.updated(workflowId, journal :+ (Sleep -> end))
      }
      advance()
      got
    }

    private def journal: Vector[(String, Recorded)] = journals.getOrElse(workflowId, Vector.empty)

    private def advance(): Unit = {
      next += 1
      reached = reached.updated(workflowId, next)
    }

    private def record[A](name: String, body: () => A)(using j: Journaled[A]): A = {
      val position = next
      advance()
      journal.lift(position) match {
        case Some((recorded, _)) if recorded != name =>
          throw UnexpectedStep(workflowId, position, name, recorded)
        case Some((_, Recorded.Threw(error))) => throw error
        case Some((_, Recorded.Output(value))) => decode(name, value)
        case Some((_, Recorded.Marker)) =>
          throw UnexpectedStep(workflowId, position, name, "a patch marker")
        case None =>
          stepping += workflowId
          val outcome =
            try Recorded.Output(j.encode(body()))
            catch {
              case crash: Crash => throw crash
              case e: Exception => Recorded.Threw(e)
            } finally stepping -= workflowId
          journals = journals.updated(workflowId, journal :+ (name -> outcome))
          outcome match {
            case Recorded.Output(value) => decode(name, value)
            case Recorded.Threw(error) => throw error
            case Recorded.Marker => sys.error("unreachable: a step never records a marker")
          }
      }
    }

    // A fresh output is decoded too, so a first run returns exactly what a replay would.
    private def decode[A](name: String, value: String)(using j: Journaled[A]): A =
      j.decode(value) match {
        case Right(a) => a
        case Left(reason) => throw UnreadableJournal(workflowId, name, reason)
      }
  }
}

object InMemoryDurable {

  /** One recorded step, by name. */
  final case class Step(name: String, outcome: Outcome)

  /** What a recorded step came to. */
  enum Outcome {

    /** It returned; its encoded output. */
    case Output(value: String)

    /** It threw an exception with this message, which DBOS records as absent when the
      * exception had none.
      */
    case Threw(message: Option[String])

    /** A patch marker (`patch`), which records neither. */
    case Marker
  }

  /** The operation DBOS records for what a `recv` received (`NotificationsDAO.recv`). */
  val Recv = "DBOS.recv"

  /** The operation DBOS records for the end of a `recv`'s wait, after [[Recv]]. */
  val Sleep = "DBOS.sleep"

  /** DBOS's step name for the patch `name` (`DBOSExecutor.patch`). */
  def patchMarker(name: String): String = s"DBOS.patch-$name"

  /** Thrown from a step body to stand for the process dying inside it. */
  final class Crash extends RuntimeException("simulated crash")

  /** A recorded step's error, rethrown on replay of a loaded history. */
  final class RecordedError(val message: Option[String])
      extends RuntimeException(message.getOrElse("(no message)"))

  /** A stream write made outside a step body, which DBOS records as an operation of its
    * own (`DBOSExecutor.writeStream`).
    */
  final case class WriteOutsideStep(workflowId: WorkflowId, key: String)
      extends RuntimeException(
        s"workflow ${WorkflowId.value(workflowId)} wrote stream '$key' outside a step"
      )

  /** A send to a workflow no run has started, which DBOS refuses
    * (`DBOSNonExistentWorkflowException`).
    */
  final case class NoSuchWorkflow(workflowId: WorkflowId)
      extends RuntimeException(s"workflow ${WorkflowId.value(workflowId)} has not started")

  /** A run reached a step under a different name than the one recorded at its position. */
  final case class UnexpectedStep(
      workflowId: WorkflowId,
      position: Int,
      name: String,
      recorded: String
  ) extends RuntimeException(
        s"workflow ${WorkflowId.value(workflowId)} step $position: ran '$name', recorded '$recorded'"
      )
}
