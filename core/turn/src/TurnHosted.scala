package grit.turn

import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.edge.{OutcomeJson, Permit, RequestState, ToolRequest}
import grit.core.id.{CallSlot, PrincipalId, ToolCallId, TurnRef}
import grit.core.place.{Directory, Place, Service}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Bound, Outcome, ToolName}

import TurnLoop.Round

/** How a turn settles a hosted call (ADR 0017): as a request addressed to its tool's place in
  * the offer, its workspace or a service it reaches, which an edge serving it claims, runs
  * and answers. The turn waits for the answer as it waits for an approval, then keeps it as
  * the call's result, as it keeps an in-step call's ([[TurnTools.Settling]]).
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

  /** The requests for `calls`, round `round`'s free hosted calls by their index and call id,
    * each addressed to its tool's place in `offer` ([[TurnOffer.placeOf]]), grouped by place
    * in the order each place is first called; a call whose tool has no place is not requested.
    */
  def requests(
      turn: TurnRef,
      round: Round,
      calls: Vector[(Int, ToolCallId, Bound.Hosted)],
      offer: TurnOffer
  ): Either[TurnFailure, Vector[(Place, Vector[ToolRequest])]] =
    calls
      .foldLeft[Either[TurnFailure, Vector[(Place, Vector[ToolRequest])]]](Right(Vector.empty)) {
        case (acc, (index, _, hosted)) =>
          offer.placeOf(hosted.tool).fold(acc) { place =>
            acc.flatMap(done =>
              request(turn, round, index, hosted, place, Permit.Free).map { q =>
                done.span(_._1 != place) match {
                  case (before, (at, sent) +: after) => (before :+ (at -> (sent :+ q))) ++ after
                  case _ => done :+ (place -> Vector(q))
                }
              }
            )
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
    * why there is none, is kept by `tool:n:j` through `settling`.
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
    val outcome: Either[TurnFailure, Outcome] = hosted.ask match {
      case None =>
        callSlot(slot).map(cs => if (sent) awaited(hosting, cs, slot, place) else unserved(place))
      case Some(shown) =>
        val entries = settling.entries
        d.transact(slot.askStep)(TurnTools.ask(entries, slot, call, shown, clock.now())).flatMap {
          _ =>
            TurnTools.approval(d.recv(Approval.topic(call), answerWithin)) match {
              case Approval.Declined(reason) => Right(Outcome.Declined(reason))
              case Approval.TimedOut => Right(Outcome.Unanswered)
              case Approval.Approved =>
                for {
                  cs <- callSlot(slot)
                  to <- place.toRight(TurnFailure.Store(s"$named has no workspace to go to"))
                  one <- request(slot.turn, slot.round, slot.index, hosted, to, Permit.Approved)
                  went <- d.transact(TurnHostedSteps.dispatchOne(slot))(
                    dispatch(hosting, to, Vector(one))
                  )
                } yield if (went) awaited(hosting, cs, slot, place) else unserved(place)
            }
        }
    }
    outcome.flatMap(o =>
      d.step(slot.step)(() => settling.answer(slot, call, shown, o, clock.now()))
    )
  }

  /** The answer to the request at `cs`, waited for: within [[ServeWithin]]; else, if an edge
    * claimed it, within [[RunWithin]] more; else why there is none.
    */
  private def awaited(
      hosting: TurnHosting,
      cs: CallSlot,
      slot: TurnTools.Slot,
      workspace: Option[Place]
  )(using
      d: Durable^
  ): Outcome = {
    import TurnJournal.given
    d.recv(cs.key, ServeWithin) match {
      case Some(message) => read(message)
      case None =>
        d.transact(TurnHostedSteps.expire(slot))(standing(hosting.requests.settle(cs))) match {
          case Left(failure) => Outcome.Failed(s"The request could not be read: $failure")
          case Right(RequestState.Answered(o)) => o
          case Right(RequestState.Expired) =>
            Outcome.Failed(workspace.flatMap(_.service).fold(NoEdge)(noService))
          case Right(RequestState.Claimed) =>
            d.recv(cs.key, RunWithin) match {
              case Some(message) => read(message)
              case None =>
                d.transact(TurnHostedSteps.abandon(slot))(
                  standing(hosting.requests.abandon(cs))
                ) match {
                  case Right(RequestState.Answered(o)) => o
                  case Right(_) => Outcome.Interrupted
                  case Left(failure) => Outcome.Failed(s"The request could not be read: $failure")
                }
            }
        }
    }
  }

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

  private def read(message: String): Outcome =
    scala.util
      .Try(ujson.read(message))
      .toEither
      .left
      .map(_.getMessage)
      .flatMap(OutcomeJson.read)
      .fold(why => Outcome.Failed(s"The edge's answer could not be read: $why"), identity)

  private def standing(state: Either[StoreError, RequestState]): Either[TurnFailure, RequestState] =
    state.left.map(e => TurnFailure.Store(e.toString))

  private def callSlot(slot: TurnTools.Slot): Either[TurnFailure, CallSlot] =
    CallSlot
      .of(slot.turn, slot.round.index, slot.index)
      .toRight(TurnFailure.Store(s"no call at round ${slot.round.index}, index ${slot.index}"))

  private def request(
      turn: TurnRef,
      round: Round,
      index: Int,
      hosted: Bound.Hosted,
      place: Place,
      permit: Permit
  ): Either[TurnFailure, ToolRequest] =
    CallSlot
      .of(turn, round.index, index)
      .toRight(TurnFailure.Store(s"no call at round ${round.index}, index $index"))
      .map(cs =>
        ToolRequest(
          cs,
          ToolRequest.Protocol,
          turn.conversationId,
          place,
          PrincipalId.Local,
          hosted.tool,
          permit,
          hosted.retry,
          hosted.arguments,
          hosted.repairs
        )
      )
}

/** The names of the steps a hosted call takes besides the loop's own. */
private object TurnHostedSteps {
  def dispatchOne(slot: TurnTools.Slot): String = Turn.Step.dispatchOne(slot.round, slot.index)
  def expire(slot: TurnTools.Slot): String = Turn.Step.expire(slot.round, slot.index)
  def abandon(slot: TurnTools.Slot): String = Turn.Step.abandon(slot.round, slot.index)
}
