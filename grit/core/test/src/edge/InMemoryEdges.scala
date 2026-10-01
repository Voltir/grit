package grit.core.edge

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{CallSlot, EdgeId, PrincipalId}
import grit.core.place.Place
import grit.core.prompt.Fragment
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolSet}

/** In-memory [[ToolRequests]], [[EdgeDirectory]] and [[Desks]] for tests, keeping
  * [[EdgesContract]]. It ignores the `Tx`. Each answer an edge gives is also kept in
  * [[told]], in order: what the turn waiting on it would receive.
  */
final class InMemoryEdges extends ToolRequests, EdgeDirectory, Desks {

  /** A request and where it stands: `claimedBy` the edge and session that claimed it. */
  final case class Row(
      request: ToolRequest,
      state: RequestState | InMemoryEdges.Open.type,
      claimedBy: Option[(EdgeId, Int)]
  )

  @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[Row]

  /** Each live edge's current session. */
  @caps.unsafe.untrackedCaptures
  var live = Map.empty[EdgeId, Int]

  @caps.unsafe.untrackedCaptures
  var adverts = Map.empty[(EdgeId, Place), Advert]

  @caps.unsafe.untrackedCaptures
  var told = Vector.empty[(CallSlot, Outcome)]

  @caps.unsafe.untrackedCaptures
  private var sessions = 0

  /** Each edge's session as registered, kept when it is killed: a desk acts under it still. */
  @caps.unsafe.untrackedCaptures
  private var registered = Map.empty[EdgeId, Int]

  private def row(slot: CallSlot): Either[StoreError, Row] =
    rows.find(_.request.slot == slot).toRight(StoreError.Invalid(s"no tool request ${slot.key}"))

  private def update(slot: CallSlot)(f: Row => Row): Unit =
    rows = rows.map(r => if (r.request.slot == slot) f(r) else r)

  def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] = {
    requests.foreach { q =>
      if (!rows.exists(_.request.slot == q.slot)) rows = rows :+ Row(q, InMemoryEdges.Open, None)
    }
    Right(())
  }

  def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] =
    row(slot).map { r =>
      (r.state, r.claimedBy) match {
        case (InMemoryEdges.Open, None) =>
          update(slot)(_.copy(state = RequestState.Expired))
          RequestState.Expired
        case (InMemoryEdges.Open, Some(_)) => RequestState.Claimed
        case (s: RequestState, _) => s
      }
    }

  def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] =
    row(slot).map { r =>
      r.state match {
        case a: RequestState.Answered => a
        case _ =>
          update(slot)(_.copy(state = RequestState.Expired))
          RequestState.Expired
      }
    }

  def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]] =
    Right(
      adverts
        .collect { case ((edge, at), advert) if at == place && live.contains(edge) => advert }
        .toVector
        .sortBy(a => EdgeId.value(a.edge))
        .headOption
    )

  /** Deletes the requests of `conversation`'s turns `first` to `last`, as a purge does. */
  def forget(
      conversation: grit.core.id.ConversationId,
      first: grit.core.id.TurnSeq,
      last: grit.core.id.TurnSeq
  ): Unit =
    rows = rows.filterNot { r =>
      val t = r.request.slot.turn
      t.conversationId == conversation &&
      grit.core.id.TurnSeq.value(t.turnSeq) >= grit.core.id.TurnSeq.value(first) &&
      grit.core.id.TurnSeq.value(t.turnSeq) <= grit.core.id.TurnSeq.value(last)
    }

  /** A live edge for `principal` hosting `places`, and its desk. */
  def desk(places: Set[Place], principal: PrincipalId = PrincipalId.Local): Desk^ = {
    sessions += 1
    val edge = EdgeId(f"edge-$sessions%04d")
    live = live.updated(edge, sessions)
    registered = registered.updated(edge, sessions)
    new InMemoryEdges.Fake(this, Registration(edge, principal, places), sessions)
  }

  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^] =
    Right(desk(places, principal))

  /** A live edge for `principal` hosting `places`, without a desk: [[claimAs]], [[answerAs]]
    * and [[advertiseAs]] act as its desk would.
    */
  def register(places: Set[Place], principal: PrincipalId = PrincipalId.Local): Registration = {
    sessions += 1
    val edge = EdgeId(f"edge-$sessions%04d")
    live = live.updated(edge, sessions)
    registered = registered.updated(edge, sessions)
    Registration(edge, principal, places)
  }

  /** `q` claimed for `reg`'s session, as its desk would; whether the claim won. */
  def claimAs(reg: Registration, q: ToolRequest): Boolean =
    registered.get(reg.edge).exists(session => claim(reg, session, q))

  /** The request at `slot` answered by `reg`'s session, as its desk would. */
  def answerAs(reg: Registration, slot: CallSlot, outcome: Outcome): Boolean =
    registered.get(reg.edge).exists(session => answer(reg, session, slot, outcome))

  /** What `reg`'s edge offers in `place`, as its desk would advertise it. */
  def advertiseAs(
      reg: Registration,
      place: Place,
      tools: ToolSet,
      fragments: Vector[Fragment]
  ): Unit =
    advertise(reg, place, tools, fragments)

  /** Ends `edge` as a crash would: it is no longer live. */
  def kill(edge: EdgeId): Unit = live = live - edge

  private[edge] def open(reg: Registration): Vector[ToolRequest] =
    rows.collect { case Row(q, InMemoryEdges.Open, None) if reg.places.contains(q.workspace) => q }

  private[edge] def claim(reg: Registration, session: Int, q: ToolRequest): Boolean =
    rows.find(_.request.slot == q.slot) match {
      case Some(Row(_, InMemoryEdges.Open, None)) =>
        update(q.slot)(_.copy(claimedBy = Some((reg.edge, session))))
        true
      case _ => false
    }

  private[edge] def answer(
      reg: Registration,
      session: Int,
      slot: CallSlot,
      outcome: Outcome
  ): Boolean =
    rows.find(_.request.slot == slot) match {
      case Some(Row(_, InMemoryEdges.Open, Some(by))) if by == (reg.edge, session) =>
        update(slot)(_.copy(state = RequestState.Answered(outcome)))
        told = told :+ (slot, outcome)
        true
      case _ => false
    }

  private[edge] def orphans(reg: Registration, session: Int): Vector[ToolRequest] = {
    val found = rows.collect {
      case Row(q, InMemoryEdges.Open, Some((edge, s)))
          if reg.places.contains(q.workspace) && !live.get(edge).contains(s) =>
        q
    }
    found.flatMap { q =>
      q.retry match {
        case Retry.Interrupt =>
          update(q.slot)(_.copy(state = RequestState.Answered(Outcome.Interrupted)))
          told = told :+ (q.slot, Outcome.Interrupted)
          None
        case Retry.Rerun =>
          update(q.slot)(_.copy(claimedBy = Some((reg.edge, session))))
          Some(q)
      }
    }
  }

  private[edge] def advertise(
      reg: Registration,
      place: Place,
      tools: ToolSet,
      fragments: Vector[Fragment]
  ): Unit =
    adverts = adverts.updated((reg.edge, place), Advert(reg.edge, tools.id, fragments.map(_.id)))
}

object InMemoryEdges {

  /** Neither claimed nor settled; a row with a claim is claimed. */
  case object Open

  /** A desk on `edges` for `registration`'s edge under `session`. */
  final class Fake(edges: InMemoryEdges, val registration: Registration, session: Int)
      extends Desk {
    def await(within: FiniteDuration): Boolean = edges.open(registration).nonEmpty
    def open(): Either[DeskError, Vector[ToolRequest]] = Right(edges.open(registration))
    def claim(request: ToolRequest): Either[DeskError, Boolean] =
      Right(edges.claim(registration, session, request))
    def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean] =
      Right(edges.answer(registration, session, slot, outcome))
    def orphans(): Either[DeskError, Vector[ToolRequest]] = Right(
      edges.orphans(registration, session)
    )
    def advertise(
        place: Place,
        tools: ToolSet,
        instructions: Vector[Fragment]
    ): Either[DeskError, Unit] =
      Right(edges.advertise(registration, place, tools, instructions))
  }
}
