package grit.turn

import grit.core.durable.{FlowFixtures, Flows, History, StepRecord}
import grit.core.id.TurnRef

import utest.*

/** The read gate for the turn: each history of the current epoch, read over the turn's flow from
  * its ref with no environment, names at every prefix the record its run made next (a wait by
  * its label), and as a whole reads as ended; and a watcher is shown that step as the one the
  * turn is in ([[Turn.running]]). A flow whose description differs from what its runs recorded
  * fails here, though its replay may pass.
  */
object TurnReadTests extends TestSuite {

  private def histories: Vector[(String, Either[String, History])] = History.recorded(Turn.Epoch)

  /** The two records a wait leaves, which a reader names by the wait's label. */
  private val Waits = Set(StepRecord.Received, StepRecord.Slept)

  /** The histories a recorder stopped while their turn was in flight, at a crash it did not
    * resume: each reads as a run whose next step is not recorded yet.
    */
  private val InFlight = Set(
    "crashed-before-append.json",
    "crashed-before-summary-append.json",
    "crashed-recording-window.json",
    "hosted-rung-crashed-before-tool.json",
    "loop-crashed-in-tool.json",
    "routed-crashed-after-route.json",
    "topical-crashed-placing.json"
  )

  /** Where `history` reads otherwise than as its run recorded it, or a watcher would be shown
    * another step than the one it was in. A wait's records are named by its label: a person's
    * time to answer (`call:n:j:wait`), or, for an edge's, the step that gives the call up if
    * the edge stays silent (`call:n:j:expire`, `call:n:j:abandon`).
    */
  private def misread(turn: TurnRef, history: History, inFlight: Boolean): Vector[String] = {
    val records: Vector[StepRecord] = history.steps.map(FlowFixtures.recorded)
    def expected(k: Int, named: String): Boolean = named == label(records.map(_.name), k)
    val prefixes = records.indices.toVector.flatMap { k =>
      val read = Flows.read(TurnFlow.flow, turn, records.take(k)).map(_.at) match {
        case Right(Flows.Read.Next(step, at)) if at == k && expected(k, step) => None
        case other => Some(s"at $k, before ${records(k).name}: $other")
      }
      val shown = Turn.running(turn, records.take(k))
      val running = Option.when(!expected(k, shown))(
        s"at $k, before ${records(k).name}: shown running $shown"
      )
      read.toVector ++ running.toVector
    }
    val whole = Flows.read(TurnFlow.flow, turn, records).map(_.at) match {
      case Right(Flows.Read.Ended(_, steps)) if !inFlight && steps == records.size => None
      case Right(Flows.Read.Next(_, at)) if inFlight && at == records.size => None
      case other => Some(s"whole: $other")
    }
    prefixes ++ whole
  }

  private val Approve = "(call:\\d+:\\d+):approve".r
  private val Dispatch = "(call:\\d+:\\d+):dispatch".r
  private val Expired = "(call:\\d+:\\d+):expire".r
  private val Sent = "(call:\\d+):send".r
  private val Answered = "(call:\\d+):(\\d+)".r

  /** The name a reader gives `names(k)`: its own, or for a wait's record, by the step before
    * the wait: after a person is asked to approve, its `wait`; after a dispatch or a send, the
    * `expire` of the call the edge answers next; after an expiry, that call's `abandon`.
    */
  private def label(names: Vector[String], k: Int): String =
    if (!Waits(names(k))) names(k)
    else
      names.take(k).filterNot(Waits).lastOption match {
        case Some(Approve(call)) => s"$call:wait"
        case Some(Dispatch(call)) => s"$call:expire"
        case Some(Expired(call)) => s"$call:abandon"
        case Some(Sent(call)) => s"$call:0:expire"
        case Some(Answered(call, j)) => s"$call:${j.toInt + 1}:expire"
        case other => s"a wait after $other"
      }

  val tests = Tests {
    test(
      "every turn history of the current epoch names its next record at every prefix, shown as the running step, and as a whole has ended"
    ) {
      val read = histories.flatMap {
        case (path, Left(why)) => Vector(s"$path: $why")
        case (path, Right(history)) =>
          TurnRef.fromWorkflowId(history.id) match {
            case None => Vector(s"$path: not a turn's history")
            case Some(turn) =>
              misread(turn, history, InFlight(path)).map(m => s"$path: $m")
          }
      }
      assert(histories.nonEmpty)
      read ==> Vector.empty
    }
  }
}
