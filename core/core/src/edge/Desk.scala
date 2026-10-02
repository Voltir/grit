package grit.core.edge

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{CallSlot, PrincipalId}
import grit.core.place.Place
import grit.core.prompt.Fragment
import grit.core.tool.{Outcome, ToolSet}

/** An edge's side of the engine's tool requests, on a connection of its own: its
  * registration is live for as long as the desk is open, and the requests it may see are
  * those in the places it registered.
  */
trait Desk extends caps.SharedCapability {

  def registration: Registration

  /** Waits up to `within` for a request to arrive in a registered place: true when woken by
    * one, false on timeout. Never loses one: a caller lists [[open]] after either.
    */
  def await(within: FiniteDuration): Boolean

  /** The requests no edge has claimed in the registered places, oldest first. */
  def open(): Either[DeskError, Vector[ToolRequest]]

  /** Claims `request` for this desk's session: false when another claim won, or the turn
    * stopped waiting for it.
    */
  def claim(request: ToolRequest): Either[DeskError, Boolean]

  /** Answers the request at `slot`, which this session claimed, with `outcome`, and rings the
    * turn waiting on it ([[Desk.Doorbell]]); false, and nothing rung, when the turn stopped
    * waiting first. Repeating it is harmless.
    */
  def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean]

  /** The claimed requests in the registered places whose claiming session is gone (its edge
    * not live, or live under a later session), settled by their [[grit.core.tool.Retry]]:
    * `Interrupt` ones answered [[Outcome.Interrupted]] and never run again; `Rerun` ones
    * claimed by this session in the same statement, and returned to be run. A request the
    * turn already stopped waiting for is left as it is.
    */
  def orphans(): Either[DeskError, Vector[ToolRequest]]

  /** Replaces what this edge offers in `place`, a registered place: `tools`, and the Place
    * layer's fragments read there, kept by id.
    */
  def advertise(
      place: Place,
      tools: ToolSet,
      instructions: Vector[Fragment]
  ): Either[DeskError, Unit]
}

object Desk {

  /** What [[Desk.answer]] sends the turn waiting on the request: a wake-up, never the outcome,
    * which the turn reads from the request.
    */
  val Doorbell: String = "answered"
}

/** Where an edge registers the places it hosts (ADR 0017). */
trait Desks {

  /** Registers an edge in this process acting for `principal`, hosting `places`, and opens
    * its desk: live until it or the engine's link closes. `Left` when the database cannot be
    * reached, or the engine running it is of another compatibility epoch than this grit.
    */
  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^]
}

/** Why a desk could not do what was asked: `why`, in the database's words. */
final case class DeskError(why: String)
