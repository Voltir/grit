package grit.turn

import scala.concurrent.duration.*

import grit.act.phase.{Awaited, Calling, Gated, WaitSteps}
import grit.core.act.Gates
import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.edge.Permit
import grit.core.id.{ToolCallId, TurnRef}
import grit.core.place.{Directory, Place}
import grit.core.store.{Askers, StoreError, Tx}
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

  /** `calls`, a round's free hosted calls by their slot and call id, each addressed to its
    * tool's place in `offer` ([[TurnOffer.placeOf]]), grouped by place in the order each place
    * is first called; a call whose tool has no place is left out.
    */
  def requests(
      calls: Vector[(TurnTools.Slot, ToolCallId, Bound.Hosted)],
      offer: TurnOffer
  ): Vector[(Place, Vector[(TurnTools.Slot, Bound.Hosted)])] =
    calls.foldLeft(Vector.empty[(Place, Vector[(TurnTools.Slot, Bound.Hosted)])]) {
      case (done, (slot, _, hosted)) =>
        offer.placeOf(hosted.tool).fold(done) { place =>
          val q = (slot, hosted)
          done.span(_._1 != place) match {
            case (before, (at, sent) +: after) => (before :+ (at -> (sent :+ q))) ++ after
            case _ => done :+ (place -> Vector(q))
          }
        }
    }

  /** A `dispatch`, `reach` or `dispatch:n:j` step: `calls`, `turn`'s calls addressed to
    * `place`, let through by `permit`, sent to the edge serving it as requests made for the
    * turn's asker, read in this transaction ([[Calling.asker]]): true. False, and nothing sent,
    * when the turn has no asker, or no live edge serves `place` ([[Calling.dispatch]]).
    */
  def dispatch(
      hosting: TurnHosting,
      turn: TurnRef,
      place: Place,
      calls: Vector[(TurnTools.Slot, Bound.Hosted)],
      permit: Permit
  )(using Tx^): Either[TurnFailure, Boolean] =
    Calling
      .asker(turn, hosting.askers)
      .flatMap(
        _.fold[Either[StoreError, Boolean]](Right(false)) { asker =>
          val sent =
            calls.map((slot, hosted) => Calling.request(slot.call, hosted, place, permit, asker))
          Calling.dispatch(hosting.requests, hosting.edges, place, sent)
        }
      )
      .left
      .map(e => TurnFailure.Store(e.toString))

  /** The call at `slot`, `hosted`, bound to a hosted tool addressed to `place`
    * ([[TurnOffer.placeOf]]), settled: asked about first when it asks (`ask:n:j`, then the
    * wait), and sent then (`dispatch:n:j`); a free one was sent with its round, and `sent`
    * says whether it went to a serving edge then. Its outcome, the answer, or why there is
    * none, is kept by `tool:n:j` through `settling`; once the edge rang, that step reads the
    * answer from the request ([[Calling.answer]]); a call not sent, it reads whether the turn
    * has an asker, to say why.
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
    val outcome: Either[TurnFailure, Waited] =
      Calling.gate(Gates.Asker(answerWithin), hosted) match {
        case Gated.Free =>
          Right(if (sent) Waited.Phase(awaited(hosting, slot, place)) else Waited.Unsent)
        case Gated.Refused(o) => Right(Waited.Phase(Awaited.Known(o)))
        case Gated.Ask(asked, within) =>
          val entries = settling.entries
          d.transact(slot.askStep, Subject.Turn(slot.turn))(
            TurnTools.ask(entries, slot, call, asked, clock.now())
          ).flatMap { _ =>
            Calling.approval(call, within) match {
              case Approval.Declined(reason) =>
                Right(Waited.Phase(Awaited.Known(Outcome.Declined(reason))))
              case Approval.TimedOut => Right(Waited.Phase(Awaited.Known(Outcome.Unanswered)))
              case Approval.Approved =>
                val turn = slot.turn
                for {
                  to <- place.toRight(TurnFailure.Store(s"$named has no workspace to go to"))
                  went <- d.transact(TurnHostedSteps.dispatchOne(slot), Subject.Turn(turn))(
                    dispatch(hosting, turn, to, Vector((slot, hosted)), Permit.Approved)
                  )
                } yield if (went) Waited.Phase(awaited(hosting, slot, place)) else Waited.Unsent
            }
          }
      }
    val (requests, askers) = (hosting.requests, hosting.askers)
    outcome.flatMap {
      case Waited.Unsent =>
        d.step(slot.step)(() =>
          settling.answerFrom(slot, call, shown, clock.now())(unsent(askers, slot.turn, place))
        )
      case Waited.Phase(Awaited.Known(o)) =>
        d.step(slot.step)(() => settling.answer(slot, call, shown, o, clock.now()))
      case Waited.Phase(Awaited.Unread(failure)) =>
        d.step(slot.step)(() =>
          settling.answer(slot, call, shown, unreadable(failure), clock.now())
        )
      case Waited.Phase(Awaited.Rung(cs)) =>
        d.step(slot.step)(() =>
          settling.answerFrom(slot, call, shown, clock.now())(
            Calling.answer[TurnFailure](requests, cs).fold(unreadable, identity)
          )
        )
    }
  }

  /** What a hosted call came to before its `tool:n:j` step: a phase's, or not sent. */
  private enum Waited {
    case Phase(awaited: Awaited[TurnFailure])
    case Unsent
  }

  /** Why a call of `turn` addressed to `workspace` was not sent: no asker, as `askers` reads it
    * in this transaction, or no edge serving it.
    */
  private def unsent(askers: Askers, turn: TurnRef, workspace: Option[Place])(using
      Tx^
  ): Outcome =
    Calling.asker(turn, askers) match {
      case Right(None) => Outcome.Failed(NoAsker)
      case Right(Some(_)) => unserved(workspace)
      case Left(e) =>
        Outcome.Failed(
          s"This call did not run, and why could not be read: ${TurnFailure.Store(e.toString)}"
        )
    }

  private val NoAsker = "This turn has no asker to make this call for, so it did not run."

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
}

/** The names of the steps a hosted call takes besides the loop's own. */
private object TurnHostedSteps {
  def dispatchOne(slot: TurnTools.Slot): String = Turn.Step.dispatchOne(slot.round, slot.index)
  def expire(slot: TurnTools.Slot): String = Turn.Step.expire(slot.round, slot.index)
  def abandon(slot: TurnTools.Slot): String = Turn.Step.abandon(slot.round, slot.index)
}
