package grit.turn

import scala.concurrent.duration.*

import grit.act.phase.{Awaited, Calling, Gated, WaitSteps}
import grit.core.act.Gates
import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.{PrincipalId, ToolCallId}
import grit.core.place.{Directory, Place}
import grit.core.store.Tx
import grit.core.tool.{Bound, Outcome, ToolName}
import grit.core.visibility.Subject

/** How a turn settles a hosted call (ADR 0017): as a request addressed to its tool's place in
  * the offer, its workspace or a service it reaches, which an edge serving it claims, runs
  * and answers. The turn waits to be rung as it waits for an approval, then reads the answer
  * from the request and keeps it as the call's result, as it keeps an in-step call's
  * ([[TurnTools.Settling]]). Each phase is [[Calling]]'s, its failures recorded as
  * [[TurnFailure.Store]].
  */
object TurnHosted {

  /** The requests for `calls`, a round's free hosted calls by their slot and call id, each
    * addressed to its tool's place in `offer` ([[TurnOffer.placeOf]]), grouped by place in the
    * order each place is first called; a call whose tool has no place is not requested.
    */
  def requests(
      calls: Vector[(TurnTools.Slot, ToolCallId, Bound.Hosted)],
      offer: TurnOffer
  ): Vector[(Place, Vector[ToolRequest])] =
    calls.foldLeft(Vector.empty[(Place, Vector[ToolRequest])]) { case (done, (slot, _, hosted)) =>
      offer.placeOf(hosted.tool).fold(done) { place =>
        val q = request(slot, hosted, place, Permit.Free)
        done.span(_._1 != place) match {
          case (before, (at, sent) +: after) => (before :+ (at -> (sent :+ q))) ++ after
          case _ => done :+ (place -> Vector(q))
        }
      }
    }

  /** A `dispatch` or `reach` step: those of `requests` addressed to `place` sent to the edge
    * serving it ([[Calling.dispatch]]).
    */
  def dispatch(hosting: TurnHosting, place: Place, requests: Vector[ToolRequest])(using
      Tx^
  ): Either[TurnFailure, Boolean] =
    Calling
      .dispatch(hosting.requests, hosting.edges, place, requests)
      .left
      .map(e => TurnFailure.Store(e.toString))

  /** The call at `slot`, `hosted`, bound to a hosted tool addressed to `place`
    * ([[TurnOffer.placeOf]]), settled: asked about first when it asks (`ask:n:j`, then the
    * wait), and sent then (`dispatch:n:j`); a free one was sent with its round, and `sent`
    * says whether an edge was serving its place then. Its outcome, the answer, or
    * why there is none, is kept by `tool:n:j` through `settling`; once the edge rang, that
    * step reads the answer from the request ([[Calling.answer]]).
    */
  def settle(
      hosting: TurnHosting,
      slot: TurnTools.Slot,
      call: ToolCallId,
      hosted: Bound.Hosted,
      place: Option[Place],
      sent: Boolean,
      answerWithin: FiniteDuration,
      settling: TurnTools.Settling^,
      clock: Clock^
  )(using d: Durable^): Either[TurnFailure, TurnTools.Settled] = {
    import TurnJournal.given
    val named = ToolName.value(hosted.tool)
    val shown = hosted.shown
    val outcome: Either[TurnFailure, Awaited[TurnFailure]] =
      Calling.gate(Gates.Asker(answerWithin), hosted) match {
        case Gated.Free =>
          Right(if (sent) awaited(hosting, slot, place) else Awaited.Known(unserved(place)))
        case Gated.Refused(o) => Right(Awaited.Known(o))
        case Gated.Ask(asked, within) =>
          val entries = settling.entries
          d.transact(slot.askStep, Subject.Turn(slot.turn))(
            TurnTools.ask(entries, slot, call, asked, clock.now())
          ).flatMap { _ =>
            Calling.approval(call, within) match {
              case Approval.Declined(reason) => Right(Awaited.Known(Outcome.Declined(reason)))
              case Approval.TimedOut => Right(Awaited.Known(Outcome.Unanswered))
              case Approval.Approved =>
                for {
                  to <- place.toRight(TurnFailure.Store(s"$named has no workspace to go to"))
                  one = request(slot, hosted, to, Permit.Approved)
                  went <- d.transact(TurnHostedSteps.dispatchOne(slot), Subject.Turn(slot.turn))(
                    dispatch(hosting, to, Vector(one))
                  )
                } yield
                  if (went) awaited(hosting, slot, place)
                  else Awaited.Known(unserved(place))
            }
          }
      }
    val requests = hosting.requests
    outcome.flatMap {
      case Awaited.Known(o) =>
        d.step(slot.step)(() => settling.answer(slot, call, shown, o, clock.now()))
      case Awaited.Unread(failure) =>
        d.step(slot.step)(() =>
          settling.answer(slot, call, shown, unreadable(failure), clock.now())
        )
      case Awaited.Rung(cs) =>
        d.step(slot.step)(() =>
          settling.answerFrom(slot, call, shown, clock.now())(
            Calling.answer[TurnFailure](requests, cs).fold(unreadable, identity)
          )
        )
    }
  }

  /** The request of the call at `slot` waited on ([[Calling.await]]), its silent edge's steps
    * `expire:n:j` and `abandon:n:j`.
    */
  private def awaited(hosting: TurnHosting, slot: TurnTools.Slot, workspace: Option[Place])(using
      d: Durable^
  ): Awaited[TurnFailure] = {
    import TurnJournal.given
    Calling.await(
      hosting.requests,
      slot.call,
      workspace,
      WaitSteps(TurnHostedSteps.expire(slot), TurnHostedSteps.abandon(slot)),
      Subject.Turn(slot.turn)
    )
  }

  private def unreadable(failure: TurnFailure): Outcome =
    Outcome.Failed(s"The request could not be read: $failure")

  /** Why a call no edge was serving has no answer. */
  private def unserved(workspace: Option[Place]): Outcome =
    Outcome.Failed(
      (workspace.flatMap(_.directory), workspace.flatMap(_.service)) match {
        case (Some(dir), _) =>
          s"No edge is serving ${Directory.value(dir)} right now, so this call did not run."
        case (None, Some(service)) =>
          s"No edge is serving ${service.name} right now, so this call did not run."
        case (None, None) =>
          "No edge is serving this conversation's directory right now, so this call did not run."
      }
    )

  private def request(
      slot: TurnTools.Slot,
      hosted: Bound.Hosted,
      place: Place,
      permit: Permit
  ): ToolRequest =
    Calling.request(slot.call, hosted, place, permit, PrincipalId.Local)
}

/** The names of the steps a hosted call takes besides the loop's own. */
private object TurnHostedSteps {
  def dispatchOne(slot: TurnTools.Slot): String = Turn.Step.dispatchOne(slot.round, slot.index)
  def expire(slot: TurnTools.Slot): String = Turn.Step.expire(slot.round, slot.index)
  def abandon(slot: TurnTools.Slot): String = Turn.Step.abandon(slot.round, slot.index)
}
