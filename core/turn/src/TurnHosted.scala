package grit.turn

import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.edge.{Permit, RequestState, ToolRequest, ToolRequests}
import grit.core.id.{CallSlot, PrincipalId, ToolCallId}
import grit.core.place.{Directory, Place, Service}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Bound, Outcome, ToolName}
import grit.core.visibility.Subject

/** How a turn settles a hosted call (ADR 0017): as a request addressed to its tool's place in
  * the offer, its workspace or a service it reaches, which an edge serving it claims, runs
  * and answers. The turn waits to be rung as it waits for an approval, then reads the answer
  * from the request and keeps it as the call's result, as it keeps an in-step call's
  * ([[TurnTools.Settling]]).
  */
object TurnHosted {

  /** How long a request waits for an edge to claim it, counted from the commit of the step
    * that sent it (a serving edge is woken by that commit and claims within milliseconds):
    * 10 seconds. Then it is expired, and the call answered that no edge is serving.
    */
  val ServeWithin: FiniteDuration = 10.seconds

  /** How long a claimed request waits for its answer after [[ServeWithin]]: 11 minutes, the
    * longest command `run` allows (10 minutes) and a minute more. Then it is abandoned, and
    * the call answered [[Outcome.Interrupted]]: it may have partly run.
    */
  val RunWithin: FiniteDuration = 11.minutes

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
    * serving it, true; false, and nothing sent, when no live edge serves it or none is
    * addressed there.
    */
  def dispatch(hosting: TurnHosting, place: Place, requests: Vector[ToolRequest])(using
      Tx^
  ): Either[TurnFailure, Boolean] = {
    val there = requests.filter(_.workspace == place)
    if (there.isEmpty) Right(false)
    else
      (for {
        serving <- hosting.edges.serving(place)
        sent <- serving.fold[Either[StoreError, Boolean]](Right(false))(_ =>
          hosting.requests.dispatch(there).map(_ => true)
        )
      } yield sent).left.map(e => TurnFailure.Store(e.toString))
  }

  /** The call at `slot`, `hosted`, bound to a hosted tool addressed to `place`
    * ([[TurnOffer.placeOf]]), settled: asked about first when it asks (`ask:n:j`, then the
    * wait), and sent then (`dispatch:n:j`); a free one was sent with its round, and `sent`
    * says whether an edge was serving its place then. Its outcome, the answer, or
    * why there is none, is kept by `tool:n:j` through `settling`; once the edge rang, that
    * step reads the answer from the request, and a request holding none, or no longer kept,
    * is kept as [[Outcome.Failed]].
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
    val outcome: Either[TurnFailure, Waited] = hosted.ask match {
      case None =>
        Right(
          if (sent) awaited(hosting, slot.call, slot, place) else Waited.Known(unserved(place))
        )
      case Some(shown) =>
        val entries = settling.entries
        d.transact(slot.askStep, Subject.Turn(slot.turn))(
          TurnTools.ask(entries, slot, call, shown, clock.now())
        ).flatMap { _ =>
          TurnTools.approval(d.recv(Approval.topic(call), answerWithin)) match {
            case Approval.Declined(reason) => Right(Waited.Known(Outcome.Declined(reason)))
            case Approval.TimedOut => Right(Waited.Known(Outcome.Unanswered))
            case Approval.Approved =>
              for {
                to <- place.toRight(TurnFailure.Store(s"$named has no workspace to go to"))
                one = request(slot, hosted, to, Permit.Approved)
                went <- d.transact(TurnHostedSteps.dispatchOne(slot), Subject.Turn(slot.turn))(
                  dispatch(hosting, to, Vector(one))
                )
              } yield
                if (went) awaited(hosting, slot.call, slot, place)
                else Waited.Known(unserved(place))
          }
        }
    }
    val requests = hosting.requests
    outcome.flatMap {
      case Waited.Known(o) =>
        d.step(slot.step)(() => settling.answer(slot, call, shown, o, clock.now()))
      case Waited.Rung(cs) =>
        d.step(slot.step)(() =>
          settling.answerFrom(slot, call, shown, clock.now())(rung(requests, cs))
        )
    }
  }

  /** What waiting on a request came to: its outcome, known already, or a ring from its edge,
    * after which the outcome is read from the request at `cs`.
    */
  private enum Waited {
    case Known(outcome: Outcome)
    case Rung(cs: CallSlot)
  }

  /** The request at `cs` waited on: rung within [[ServeWithin]]; else, if an edge claimed it,
    * rung within [[RunWithin]] more; else its answer read as the wait ended, or why there is
    * none.
    */
  private def awaited(
      hosting: TurnHosting,
      cs: CallSlot,
      slot: TurnTools.Slot,
      workspace: Option[Place]
  )(using
      d: Durable^
  ): Waited = {
    import TurnJournal.given
    // A recorded message, a ring or (from an earlier build) a whole outcome, is only a wake-up.
    d.recv(cs.key, ServeWithin) match {
      case Some(_) => Waited.Rung(cs)
      case None =>
        d.transact(TurnHostedSteps.expire(slot), Subject.Turn(slot.turn))(
          standing(hosting.requests.settle(cs))
        ) match {
          case Left(failure) => Waited.Known(unreadable(failure))
          case Right(RequestState.Answered(o)) => Waited.Known(o)
          case Right(RequestState.Expired) =>
            Waited.Known(Outcome.Failed(workspace.flatMap(_.service).fold(NoEdge)(noService)))
          case Right(RequestState.Claimed) =>
            d.recv(cs.key, RunWithin) match {
              case Some(_) => Waited.Rung(cs)
              case None =>
                d.transact(TurnHostedSteps.abandon(slot), Subject.Turn(slot.turn))(
                  standing(hosting.requests.abandon(cs))
                ) match {
                  case Right(RequestState.Answered(o)) => Waited.Known(o)
                  case Right(_) => Waited.Known(Outcome.Interrupted)
                  case Left(failure) => Waited.Known(unreadable(failure))
                }
            }
        }
    }
  }

  /** The answer the request at `cs` holds, its edge having rung: why there is none when it
    * holds none, is no longer kept (a purge mid-turn), or cannot be read.
    */
  private def rung(requests: ToolRequests, cs: CallSlot)(using Tx^): Outcome =
    requests.answered(cs) match {
      case Right(Some(o)) => o
      case Right(None) => Outcome.Failed(RungUnanswered)
      case Left(StoreError.Invalid(_)) => Outcome.Failed(RungGone)
      case Left(e) => unreadable(TurnFailure.Store(e.toString))
    }

  private val RungUnanswered =
    "The edge rang, but its request holds no answer, so this call's result is unknown."

  private val RungGone = "This call's request is no longer kept, so its result is unknown."

  private def unreadable(failure: TurnFailure): Outcome =
    Outcome.Failed(s"The request could not be read: $failure")

  private val NoEdge =
    "No edge is serving this conversation's directory right now, so this call did not run."

  private def noService(service: Service): String =
    s"No edge is serving ${service.name} right now, so this call did not run."

  /** Why a call no edge was serving has no answer. */
  private def unserved(workspace: Option[Place]): Outcome =
    Outcome.Failed(
      (workspace.flatMap(_.directory), workspace.flatMap(_.service)) match {
        case (Some(dir), _) =>
          s"No edge is serving ${Directory.value(dir)} right now, so this call did not run."
        case (None, Some(service)) => noService(service)
        case (None, None) => NoEdge
      }
    )

  private def standing(state: Either[StoreError, RequestState]): Either[TurnFailure, RequestState] =
    state.left.map(e => TurnFailure.Store(e.toString))

  private def request(
      slot: TurnTools.Slot,
      hosted: Bound.Hosted,
      place: Place,
      permit: Permit
  ): ToolRequest =
    ToolRequest(
      slot.call,
      ToolRequest.Protocol,
      slot.turn.conversationId,
      place,
      PrincipalId.Local,
      hosted.tool,
      permit,
      hosted.retry,
      hosted.arguments,
      hosted.repairs,
      hosted.destination
    )
}

/** The names of the steps a hosted call takes besides the loop's own. */
private object TurnHostedSteps {
  def dispatchOne(slot: TurnTools.Slot): String = Turn.Step.dispatchOne(slot.round, slot.index)
  def expire(slot: TurnTools.Slot): String = Turn.Step.expire(slot.round, slot.index)
  def abandon(slot: TurnTools.Slot): String = Turn.Step.abandon(slot.round, slot.index)
}
