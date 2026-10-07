package grit.core.edge

import grit.core.id.{CallSlot, EdgeId}
import grit.core.place.Place
import grit.core.prompt.FragmentId
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, ToolSetId}

/** The engine's side of hosted tool calls (ADR 0017): each call's request, written once under
  * its slot's key, and where it stands.
  */
trait ToolRequests {

  /** Writes each of `requests` unless a request has its key, and wakes the edges serving
    * their workspaces when the transaction commits. A request this transaction may not send
    * ([[ToolRequests.refusal]]) is written already answered with that refusal, and no edge
    * opens it.
    */
  def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit]

  /** Expires the request at `slot` if no edge has claimed it, and says where it stands.
    * `StoreError.Invalid` when no request has its key.
    */
  def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState]

  /** Expires the request at `slot` unless it was answered: [[RequestState.Answered]] when the
    * answer came first, [[RequestState.Expired]] otherwise; an edge's later answer then fails.
    * `StoreError.Invalid` when no request has its key.
    */
  def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState]

  /** The outcome the request at `slot` was answered with; `None` while it is open or claimed,
    * or once it expired. `StoreError.Invalid` when no request has its key.
    */
  def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]]
}

object ToolRequests {

  /** What `request` is answered with when this transaction may not send it, naming the place
    * it may not reach and never a label; `None` when it may. It may not when its destination
    * is one the transaction does not write to ([[Tx.writesTo]]); when it has none and its
    * workspace is a service the transaction does not send to ([[Tx.sendsTo]]); or when the
    * transaction does not read from its workspace ([[Tx.readsFrom]]).
    */
  def refusal(request: ToolRequest)(using Tx^): Option[Outcome] = {
    val refused: Option[String] = request.destination match {
      case Some(to) if !Tx.writesTo(to) => Some(s"may not write to ${to.written}")
      case None if request.workspace.service.exists(s => !Tx.sendsTo(s)) =>
        Some(s"may not send to ${request.workspace.written}")
      case _ if !Tx.readsFrom(request.workspace) =>
        Some(s"may not read from ${request.workspace.written}")
      case _ => None
    }
    refused.map(why => Outcome.Failed(s"Nothing was sent: this conversation $why."))
  }
}

/** Where a request stands for the turn that made it. */
enum RequestState {

  /** No edge will run it: nobody claimed it in time, or the turn stopped waiting. */
  case Expired

  /** An edge claimed it and has not answered. */
  case Claimed

  /** An edge ran it: what it came to. */
  case Answered(outcome: Outcome)
}

/** What a live edge offers in one place: its tool set there, and the instruction files it
  * read there (the Place layer's fragments, farthest first).
  */
final case class Advert(edge: EdgeId, tools: ToolSetId, instructions: Vector[FragmentId])

/** The engine's view of which edges are serving. */
trait EdgeDirectory {

  /** What the live edge serving `place` offers there; the lowest edge id among several, so
    * the choice is the same whoever asks. `None` when no live edge serves it.
    */
  def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]]
}
