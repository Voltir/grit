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
    * their workspaces when the transaction commits.
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
