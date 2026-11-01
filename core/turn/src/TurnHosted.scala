package grit.turn

import grit.act.phase.{Awaited, Calling, WaitSteps, Waiting}
import grit.core.approval.Approval
import grit.core.durable.{Flow, Step}
import grit.core.edge.Permit
import grit.core.id.TurnRef
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

  /** A `call:n:send`, `call:n:send:{place}` or `call:n:j:dispatch` step: `calls`, `turn`'s
    * calls addressed to `place`, let through by `permit`, sent to the edge serving it as
    * requests made for the
    * turn's asker, read in this transaction ([[Calling.asker]]), checked at what `pins` sends
    * at ([[Turn.Picked.at]]): true. False, and nothing sent, when the turn has no asker, or no
    * live edge serves `place` ([[Calling.dispatch]]).
    */
  private[turn] def dispatch(
      hosting: TurnHosting,
      turn: TurnRef,
      place: Place,
      calls: Vector[(TurnTools.Slot, Bound.Hosted)],
      permit: Permit,
      pins: Turn.Bounded
  )(using Tx^): Either[TurnFailure, Boolean] =
    Calling
      .asker(turn, hosting.askers)
      .flatMap(
        _.fold[Either[StoreError, Boolean]](Right(false)) { asker =>
          val sent =
            calls.map((slot, hosted) => Calling.request(slot.call, hosted, place, permit, asker))
          Calling.dispatch(hosting.requests, hosting.edges, place, sent, pins.at)
        }
      )
      .left
      .map(e => TurnFailure.Store(e.toString))

  /** A hosted call settled: asked about first when it asks (`call:n:j:approve`, then the wait
    * of up to its tool's own time, at most its loop's `answerWithin`), and sent then (`call:n:j:dispatch`); a free one was
    * sent with its round, and [[TurnRound.Call.sent]] says whether it went to a serving edge
    * then. Its outcome, the answer, or why there is none, is kept by `call:n:j`; once the edge
    * rang, that step reads the answer from the request ([[Calling.answer]]); a call not sent, it reads
    * whether the turn has an asker, to say why. It is sent as its loop's pins say
    * ([[dispatch]]), and every read of its answer, or of why there is none, its wait's
    * included, runs only while the turn knows no more than it was routed for
    * ([[Turn.Picked.known]]): after a rise the turn fails, saying to ask again, keeps no
    * answer, and calls no model again. A call approved whose tool has no place fails the turn,
    * recording nothing.
    */
  private[turn] def settle
      : Flow[TurnRound.Env, TurnRound.Hosting, Nothing, Either[TurnFailure, TurnTools.Settled]] = {
    import TurnJournal.given
    val waited: Flow[TurnRound.Env, TurnRound.Hosting, Nothing, Waited] =
      Calling
        .await[TurnRound.Hosting, TurnFailure](
          WaitSteps(TurnRound.Names.expire, TurnRound.Names.abandon),
          h =>
            Waiting(h.slot.call, h.place, Subject.Turn(h.slot.turn), h.call.at.looping.routed.bound)
        )
        .over[TurnRound.Env](r => r.hosting.requests)
        .map(Waited.Phase(_))
    // A call sent to no serving edge waits for nothing.
    val awaited: Flow[TurnRound.Env, (TurnRound.Hosting, Boolean), Nothing, Waited] =
      Flow.branch((sending: (TurnRound.Hosting, Boolean)) =>
        if (sending._2) Left(sending._1) else Right(Waited.Unsent)
      )(waited, Flow.pass[Waited])
    val sent = Step.transact[TurnRound.Env, Dispatching, Either[TurnFailure, Boolean]](
      TurnRound.Names.dispatch,
      d => Subject.Turn(d.slot.turn)
    )(r =>
      d =>
        (tx: Tx^) ?=> {
          val (h, turn) = (d.hosting, d.slot.turn)
          val pins = h.call.at.looping.routed
          r.hosted(h.call.at, d.slot, h.call.pending)
            .flatMap(hosted =>
              dispatch(r.hosting, turn, d.to, Vector((d.slot, hosted)), Permit.Approved, pins)
            )
        }
    )
    val dispatched: Flow[TurnRound.Env, Dispatching, Nothing, Either[TurnFailure, Waited]] =
      Flow
        .step(sent)
        .keeping
        .map((d, went) => went.map(d.hosting -> _))
        .andThen(awaited.right[TurnFailure])
    val approved: Flow[TurnRound.Env, TurnRound.Hosting, Nothing, Either[TurnFailure, Waited]] =
      Flow
        .pass[TurnRound.Hosting]
        .map(_.call)
        .andThen(TurnRound.approved)
        .keeping
        .map[Either[Either[TurnFailure, Waited], Dispatching]]((h, answer) =>
          answer.left.map(Left(_)).flatMap {
            case Approval.Declined(reason) =>
              Left(Right(Waited.Phase(Awaited.Known(Outcome.Declined(reason)))))
            case Approval.TimedOut => Left(Right(Waited.Phase(Awaited.Known(Outcome.Unanswered))))
            case Approval.Approved =>
              h.place match {
                case Some(to) => Right(Dispatching(h, to))
                case None =>
                  val named = ToolName.value(h.tool)
                  Left(Left(TurnFailure.Store(s"$named has no workspace to go to")))
              }
          }
        )
        .andThen(Flow.either(Flow.pass[Either[TurnFailure, Waited]], dispatched))
    val came = Step[TurnRound.Env, Came, Either[TurnFailure, TurnTools.Settled]](
      TurnRound.Names.call
    )(r =>
      c => {
        val (h, slot) = (c.hosting, c.slot)
        r.hosted(h.call.at, slot, h.call.pending).flatMap { hosted =>
          val (call, shown, level) = (h.call.id, hosted.shown, h.call.at.level)
          val pins = h.call.at.looping.routed
          val (settling, at) = (r.settling, r.now())
          val (requests, askers) = (r.hosting.requests, r.hosting.askers)
          // An edge's answer is believed at what the service the call went to declares of its
          // items; at no service (a directory, this machine), its tool's ([[Believed.at]]); the
          // edge's integrity never. One no edge gave is grit's own words, believed at the
          // turn's level as a refusal is; but a person's reason for declining is theirs, and
          // believed as the edge's would be.
          val from = h.place.flatMap(_.service).toRight(hosted.output.at(level))
          val own = Left(level)
          c.waited match {
            case Waited.Unsent =>
              settling.answerFrom(slot, call, shown, own, at)(
                pins.known.map(_ => unsent(askers, slot.turn, h.place))
              )
            case Waited.Phase(Awaited.Answered(o)) =>
              settling.answer(slot, call, shown, o, from, at, hosted.logs)
            case Waited.Phase(Awaited.Known(o @ Outcome.Declined(Some(_)))) =>
              settling.answer(slot, call, shown, o, from, at)
            case Waited.Phase(Awaited.Known(o)) => settling.answer(slot, call, shown, o, own, at)
            case Waited.Phase(Awaited.Unread(failure)) =>
              // A wait refused after a rise is unread too: the turn fails here, before its
              // next call.
              settling.answerFrom(slot, call, shown, own, at)(
                pins.known.map(_ => unreadable(failure))
              )
            case Waited.Phase(Awaited.Rung(cs)) =>
              settling.answerFrom(slot, call, shown, from, at, hosted.logs)(
                pins.known
                  .map(_ =>
                    Calling
                      .answer[TurnFailure](requests, cs, pins.bound)
                      .fold(unreadable, identity)
                  )
              )
          }
        }
      }
    )
    Flow
      .branch((h: TurnRound.Hosting) => if (h.asks) Right(h) else Left(h))(
        Flow
          .pass[TurnRound.Hosting]
          .map(h => (h, h.call.sent))
          .andThen(awaited)
          .map[Either[TurnFailure, Waited]](Right(_)),
        approved
      )
      .keeping
      .map((h, w) => w.map(Came(h, _)))
      .andThen(Flow.step(came).right[TurnFailure].map(_.flatten))
  }

  /** What a hosted call came to before its `call:n:j` step: a phase's, or not sent. */
  private enum Waited {
    case Phase(awaited: Awaited[TurnFailure])
    case Unsent
  }

  /** `hosting`, approved, sent to `to`. */
  private final case class Dispatching(hosting: TurnRound.Hosting, to: Place)
      extends TurnRound.OfCall {
    def slot: TurnTools.Slot = hosting.slot
  }

  /** `hosting`, as its wait, or its not being sent, left it. */
  private final case class Came(hosting: TurnRound.Hosting, waited: Waited)
      extends TurnRound.OfCall {
    def slot: TurnTools.Slot = hosting.slot
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
