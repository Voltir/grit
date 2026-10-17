package grit.kit.rehearse

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.inbox.InboxError
import grit.core.stitch.{Opening, Placements}
import grit.core.store.Jot
import grit.dbos.engine.{Rehearsing, Starts}

/** The runner of one rehearsal: launched, it records the situation's message through the
  * session by `arrives`, then runs what that starts in the order the engine starts it (the
  * opening's placement, then the triage or the turn), then each turn a body it ran started, in
  * the order started, each once; every body as a [[Rehearsal]] over the session's `jot`,
  * telling `told`, which is told [[Rehearsal.Event.Running]] before each. A body that throws ends there and the rest still run; a body stopped
  * ([[Stop]]) ends the rehearsal, and nothing after it runs. Of the bodies it is launched with
  * it runs only the stitch, triage and turn ones, and keeps none past `launched`. Launched
  * once; built and read inside one rehearsal's block.
  */
final class RehearsalRunner(
    arrives: (r: Rehearsing^) -> Either[InboxError, Starts],
    told: Rehearsal.Told^
) extends Rehearsing.Runner {

  // Pure data only, and never a body: what keeps a body past `launched` would be a hole
  // capture checking does not close (a mutable collection is a pure type).
  private val arrival = mutable.ArrayBuffer[Either[InboxError, Starts]]()
  private val record = mutable.ArrayBuffer[RehearsalRunner.Ran]()
  private val queued = mutable.Queue[TurnRef]()
  private val taken = mutable.Set[TurnRef]()
  private val placed = mutable.Map[Opening, Either[String, String]]()

  /** What `arrives` came to; `None` before it is launched. */
  def arrived: Option[Either[InboxError, Starts]] = synchronized(arrival.headOption)

  /** Each workflow it ran, in the order run. */
  def ran: Vector[RehearsalRunner.Ran] = synchronized(record.toVector)

  def launched(
      session: Rehearsing^,
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      settle: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      stitch: WorkflowId => Durable^ ?=> String,
      shadow: WorkflowId => Durable^ ?=> String,
      run: WorkflowId => Durable^ ?=> String
  ): Unit = runs(session.jot, arrives(session), turn, triage, stitch)

  /** [[launched]]'s run, once `arrival` is known, writing through `writes`. */
  private[rehearse] def runs(
      writes: Jot^,
      arrival: Either[InboxError, Starts],
      turn: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      stitch: WorkflowId => Durable^ ?=> String
  ): Unit = {
    synchronized(this.arrival += arrival)
    def ran(id: WorkflowId, body: WorkflowId => Durable^ ?=> String): Option[String] = {
      told(Rehearsal.Event.Running(id))
      val came =
        try RehearsalRunner.Came.Returned(body(id)(using new Rehearsal(id, writes, told)))
        catch {
          case Stop(step) => RehearsalRunner.Came.Stopped(step)
          case NonFatal(e) => RehearsalRunner.Came.Threw(e.toString)
        }
      synchronized(record += RehearsalRunner.Ran(id, came))
      came match {
        case RehearsalRunner.Came.Returned(result) => Some(result)
        case _ => None
      }
    }
    def place(opening: Option[Opening]): Unit = opening.foreach { o =>
      val id = o.ref.workflowId
      val result = ran(id, stitch).toRight(s"the placement ${WorkflowId.value(id)} failed")
      synchronized(placed.update(o, result))
    }
    def next(): Option[TurnRef] =
      synchronized(Option.when(!stopped && queued.nonEmpty)(queued.dequeue()))
    def turns(): Unit = next() match {
      case None => ()
      case Some(t) =>
        ran(t.workflowId, turn)
        turns()
    }
    arrival match {
      case Left(_) => ()
      case Right(Starts.Heard(opening, heard)) =>
        place(opening)
        heard.foreach(t => if (!stopped) ran(t.workflowId, triage))
        turns()
      case Right(Starts.Said(opening, said)) =>
        place(opening)
        synchronized(take(said))
        turns()
    }
  }

  /** Queues `turn` unless it was queued before. */
  private def take(turn: TurnRef): Unit = if (taken.add(turn)) queued.enqueue(turn)

  /** Whether a body it ran was stopped. */
  private def stopped: Boolean = synchronized(record.exists(_.came match {
    case RehearsalRunner.Came.Stopped(_) => true
    case _ => false
  }))

  def started(turn: TurnRef): Either[InboxError, Unit] = synchronized {
    if (stopped) Left(InboxError.Invalid("the rehearsal stopped at the turn's own model call"))
    else Right(take(turn))
  }

  val placements: Placements = new Placements {
    def awaited(opening: Opening): Either[String, String] =
      RehearsalRunner.this
        .synchronized(placed.get(opening))
        .getOrElse(
          Left(s"${WorkflowId.value(opening.ref.workflowId)} was not placed in this rehearsal")
        )
    def awaitedWithin(
        opening: Opening,
        within: FiniteDuration,
        clock: Clock^
    ): Either[Placements.Unplaced, String] =
      awaited(opening).left.map(Placements.Unplaced.Failed(_))
  }
}

object RehearsalRunner {

  /** What the workflow `workflow` came to, run in a rehearsal. */
  final case class Ran(workflow: WorkflowId, came: Came)

  /** How a rehearsed workflow's body ended. */
  enum Came {

    /** It returned `result`. */
    case Returned(result: String)

    /** It threw, `why`. */
    case Threw(why: String)

    /** It was stopped at the turn's own model call, in the step `step`. */
    case Stopped(step: String)
  }
}
