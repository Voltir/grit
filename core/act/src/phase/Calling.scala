package grit.act.phase

import scala.concurrent.duration.*

import grit.core.act.{ActsFor, Gates}
import grit.core.approval.Approval
import grit.core.durable.{Durable, Journaled}
import grit.core.edge.{EdgeDirectory, Permit, RequestState, ToolRequest, ToolRequests}
import grit.core.id.{CallSlot, PrincipalId, ToolCallId}
import grit.core.identity.Principal
import grit.core.job.ScheduleStore
import grit.core.place.{Place, Service}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, ToolName}
import grit.core.visibility.Subject

/** The steps a hosted call's wait takes when its edge is silent: `expire`, after
  * [[Calling.ServeWithin]], and `abandon`, after [[Calling.RunWithin]] more.
  */
final case class WaitSteps(expire: String, abandon: String)

/** What a call's gate made of it. */
enum Gated {

  /** Sent as it is: its tool asks no one. */
  case Free

  /** Sent once a person shown `shown` approves it, waiting up to `within`. */
  case Ask(shown: String, within: FiniteDuration)

  /** Not sent: answered `outcome`. */
  case Refused(outcome: Outcome)
}

/** What waiting on a request came to. */
enum Awaited[F] {

  /** Its edge rang: the answer is on the request, read by [[Calling.answer]]. */
  case Rung(cs: CallSlot)

  /** Known without reading the request again: no edge claimed it, or it was answered before
    * the wait ended, or abandoned.
    */
  case Known(outcome: Outcome)

  /** The request could not be read: `failure`. */
  case Unread(failure: F)
}

/** A hosted call's phases (ADR 0017): its gate, whom it is made for, its request, sent to the
  * edge serving its place, the wait for that edge, a person's approval, and the answer read.
  */
object Calling {

  /** How long a request waits to be claimed, from the commit of the step that sent it (a
    * serving edge is woken by that commit and claims within milliseconds): 10 s. Then it is
    * expired, and the call answered that no edge is serving.
    */
  val ServeWithin: FiniteDuration = 10.seconds

  /** How long a claimed request waits for its answer after [[ServeWithin]]: 11 minutes, the
    * longest command `run` allows and a minute more. Then it is abandoned, and the call
    * answered [[Outcome.Interrupted]]: it may have partly run.
    */
  val RunWithin: FiniteDuration = 11.minutes

  /** What `gates` makes of a call bound to `hosted`: `Free` when its tool asks no one; else
    * `Ask` under [[Gates.Asker]], and under [[Gates.Closed]] `Refused` with a `Failed` saying
    * its tool asks a person first and nobody waits on this call to approve it.
    */
  def gate(gates: Gates, hosted: Bound.Hosted): Gated =
    (hosted.ask, gates) match {
      case (None, _) => Gated.Free
      case (Some(shown), Gates.Asker(within)) => Gated.Ask(shown, within)
      case (Some(_), Gates.Closed) =>
        Gated.Refused(
          Outcome.Failed(
            s"${ToolName.value(hosted.tool)} asks a person first, and nobody waits on this " +
              "call to approve it, so it did not run."
          )
        )
    }

  /** Whom `actsFor` names for the call at `cs`, read in this transaction: its turn's asker
    * through `askers` (grit as [[PrincipalId.Grit]], a person as their id), or its schedule's
    * principal through `schedules`. `None` when there is no asker or the schedule is gone: the
    * call is not sent.
    */
  def principal(actsFor: ActsFor, cs: CallSlot, askers: Askers, schedules: ScheduleStore)(using
      Tx^
  ): Either[StoreError, Option[PrincipalId]] =
    actsFor match {
      case ActsFor.Asker =>
        askers
          .of(cs.turn)
          .map(_.map {
            case Principal.Grit => PrincipalId.Grit
            case Principal.Person(id, _) => id
          })
      case ActsFor.Scheduled(schedule) => schedules.read(schedule).map(_.map(_.principal))
    }

  /** The request for the call at `cs`, bound to `hosted`, addressed to `place`, let through by
    * `permit`, for `principal`, from `cs`'s conversation.
    */
  def request(
      cs: CallSlot,
      hosted: Bound.Hosted,
      place: Place,
      permit: Permit,
      principal: PrincipalId
  ): ToolRequest =
    ToolRequest(
      cs,
      ToolRequest.Protocol,
      cs.turn.conversationId,
      place,
      principal,
      hosted.tool,
      permit,
      hosted.retry,
      hosted.arguments,
      hosted.repairs,
      hosted.destination
    )

  /** Those of `sent` addressed to `place` written for the live edge serving it, which is woken
    * when this transaction commits: true. False, and nothing written, when no live edge serves
    * `place` or none is addressed there. One this transaction may not send is written answered
    * with its refusal ([[grit.core.edge.ToolRequests.refusal]]), and is never claimed.
    */
  def dispatch(
      requests: ToolRequests,
      edges: EdgeDirectory,
      place: Place,
      sent: Vector[ToolRequest]
  )(using Tx^): Either[StoreError, Boolean] = {
    val there = sent.filter(_.workspace == place)
    if (there.isEmpty) Right(false)
    else
      edges
        .serving(place)
        .flatMap(
          _.fold[Either[StoreError, Boolean]](Right(false))(_ =>
            requests.dispatch(there).map(_ => true)
          )
        )
  }

  /** The wait for the request at `cs`, sent by a step that committed: `Rung` when its edge
    * rings within [[ServeWithin]]; else `steps.expire` settles it (a transact step for
    * `subject`): answered meanwhile, `Known` with the answer; unclaimed, `Known` with a `Failed`
    * naming `place`'s service, or saying no edge serves it; claimed, waited on [[RunWithin]]
    * more, then `Rung`, or `steps.abandon`: answered meanwhile, `Known`, else
    * `Known(Interrupted)`. A step that cannot read the request is `Unread`. Its steps record
    * what they found in `F`'s form.
    */
  def await[F](
      requests: ToolRequests,
      cs: CallSlot,
      place: Option[Place],
      steps: WaitSteps,
      subject: Subject
  )(using
      d: Durable^,
      faults: Faults[F],
      recorded: Journaled[Either[F, RequestState]]
  ): Awaited[F] =
    // A recorded message, a ring or (from an earlier build) a whole outcome, is only a wake-up.
    d.recv(cs.key, ServeWithin) match {
      case Some(_) => Awaited.Rung(cs)
      case None =>
        d.transact(steps.expire, subject)(standing(requests.settle(cs))) match {
          case Left(failure) => Awaited.Unread(failure)
          case Right(RequestState.Answered(o)) => Awaited.Known(o)
          case Right(RequestState.Expired) =>
            Awaited.Known(Outcome.Failed(place.flatMap(_.service).fold(NoEdge)(noService)))
          case Right(RequestState.Claimed) =>
            d.recv(cs.key, RunWithin) match {
              case Some(_) => Awaited.Rung(cs)
              case None =>
                d.transact(steps.abandon, subject)(standing(requests.abandon(cs))) match {
                  case Left(failure) => Awaited.Unread(failure)
                  case Right(RequestState.Answered(o)) => Awaited.Known(o)
                  case Right(_) => Awaited.Known(Outcome.Interrupted)
                }
            }
        }
    }

  /** A person's answer to the gated call `call`, waited for up to `within`
    * ([[grit.core.durable.Durable.recv]] on [[Approval.topic]]): [[Approval.TimedOut]] when
    * none came; [[Approval.Declined]], saying why, when the message does not read, so the call
    * does not run.
    */
  def approval(call: ToolCallId, within: FiniteDuration)(using d: Durable^): Approval =
    d.recv(Approval.topic(call), within) match {
      case None => Approval.TimedOut
      case Some(message) =>
        Approval
          .decode(message)
          .fold(
            why =>
              Approval.Declined(Some(s"The answer could not be read ($why), so it did not run.")),
            identity
          )
    }

  /** The answer on the request at `cs`, read for this transaction, its edge having rung: a
    * `Failed` saying why when it holds none or is no longer kept; `Left`, the store's failure in
    * `F`'s form, when it cannot be read. The one place an answer is read.
    */
  def answer[F](requests: ToolRequests, cs: CallSlot)(using
      Tx^
  )(using faults: Faults[F]): Either[F, Outcome] =
    requests.answered(cs) match {
      case Right(Some(o)) => Right(o)
      case Right(None) => Right(Outcome.Failed(RungUnanswered))
      case Left(StoreError.Invalid(_)) => Right(Outcome.Failed(RungGone))
      case Left(e) => Left(faults.store(e.toString))
    }

  private def standing[F](state: Either[StoreError, RequestState])(using
      faults: Faults[F]
  ): Either[F, RequestState] =
    state.left.map(e => faults.store(e.toString))

  private val RungUnanswered =
    "The edge rang, but its request holds no answer, so this call's result is unknown."

  private val RungGone = "This call's request is no longer kept, so its result is unknown."

  private val NoEdge =
    "No edge is serving this conversation's directory right now, so this call did not run."

  private def noService(service: Service): String =
    s"No edge is serving ${service.name} right now, so this call did not run."
}
