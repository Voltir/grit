package grit.core.edge

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{CallSlot, EdgeId, PrincipalId}
import grit.core.place.Place
import grit.core.prompt.Fragment
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolSet}
import grit.core.visibility.Classification

/** In-memory [[ToolRequests]], [[EdgeDirectory]] and [[Desks]] for tests, keeping
  * [[EdgesContract]]. It reads the `Tx` only for [[ToolRequests.refusal]]. Each answer an edge
  * gives is also kept in
  * [[answers]], in order, for a test to read; the turn is told none of them, only rung with
  * [[Desk.Doorbell]], and reads its answer from the request.
  */
final class InMemoryEdges extends ToolRequests, EdgeDirectory, Desks {

  /** A request and where it stands: `claimedBy` the edge and session that claimed it. */
  final case class Row(
      request: ToolRequest,
      state: RequestState | InMemoryEdges.Open.type,
      claimedBy: Option[(EdgeId, Int)]
  )

  // Every var is replaced whole, by the fake's methods, inside `synchronized`: a served desk's
  // server calls them from its own thread and answers from a virtual thread per request, all
  // at once with the test's. The public ones are volatile, so a test reading one between calls
  // sees the last write.
  @volatile @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[Row]

  /** Each live edge's current session. */
  @volatile @caps.unsafe.untrackedCaptures
  var live = Map.empty[EdgeId, Int]

  @volatile @caps.unsafe.untrackedCaptures
  var adverts = Map.empty[(EdgeId, Place), Advert]

  @volatile @caps.unsafe.untrackedCaptures
  var answers = Vector.empty[(CallSlot, Outcome)]

  @caps.unsafe.untrackedCaptures
  private var sessions = 0

  /** Each edge's session as registered, kept when it is killed: a desk acts under it still. */
  @caps.unsafe.untrackedCaptures
  private var registered = Map.empty[EdgeId, Int]

  /** How many open requests were dispatched to each place: what wakes a desk's await. */
  @caps.unsafe.untrackedCaptures
  private var rung = Map.empty[Place, Long]

  // Each conversation's origin, as `originates` set it.
  @caps.unsafe.untrackedCaptures
  private var origins = Map.empty[grit.core.id.ConversationId, grit.core.store.Origin]

  /** `conversation`'s origin is `origin`, as the conversation store holds one. */
  def originates(conversation: grit.core.id.ConversationId, origin: grit.core.store.Origin): Unit =
    synchronized { origins = origins.updated(conversation, origin) }

  private def row(slot: CallSlot): Either[StoreError, Row] = synchronized {
    rows.find(_.request.slot == slot).toRight(StoreError.Invalid(s"no tool request ${slot.key}"))
  }

  private def update(slot: CallSlot)(f: Row => Row): Unit = synchronized {
    rows = rows.map(r => if (r.request.slot == slot) f(r) else r)
  }

  def dispatch(requests: Vector[ToolRequest], at: Classification)(using
      Tx^
  ): Either[StoreError, Unit] = synchronized {
    requests.foreach { q =>
      if (!rows.exists(_.request.slot == q.slot)) {
        val state = ToolRequests.refusal(q, at).fold(InMemoryEdges.Open)(RequestState.Answered(_))
        rows = rows :+ Row(q, state, None)
        if (state == InMemoryEdges.Open) {
          rung = rung.updated(q.workspace, rung.getOrElse(q.workspace, 0L) + 1)
        }
      }
    }
    notifyAll()
    Right(())
  }

  /** How many open requests were dispatched to `places`, as a desk counts what it heard. */
  private[edge] def rungIn(places: Set[Place]): Long = synchronized {
    places.toVector.map(rung.getOrElse(_, 0L)).sum
  }

  /** Waits up to `within` for [[rungIn]] `places` to pass `heard`, as the Postgres desk waits
    * for a notice; the count it reached, `heard` on timeout.
    */
  private[edge] def ringing(places: Set[Place], heard: Long, within: FiniteDuration): Long =
    synchronized {
      val until = System.nanoTime() + within.toNanos
      var now = rungIn(places)
      while (now == heard && until - System.nanoTime() > 0) {
        java.util.concurrent.TimeUnit.NANOSECONDS.timedWait(this, until - System.nanoTime())
        now = rungIn(places)
      }
      now
    }

  def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = synchronized {
    row(slot).map { r =>
      (r.state, r.claimedBy) match {
        case (InMemoryEdges.Open, None) =>
          update(slot)(_.copy(state = RequestState.Expired))
          RequestState.Expired
        case (InMemoryEdges.Open, Some(_)) => RequestState.Claimed
        case (s: RequestState, _) => s
      }
    }
  }

  def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = synchronized {
    row(slot).map { r =>
      r.state match {
        case a: RequestState.Answered => a
        case _ =>
          update(slot)(_.copy(state = RequestState.Expired))
          RequestState.Expired
      }
    }
  }

  def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
    row(slot).map(_.state match {
      case RequestState.Answered(outcome) => Some(outcome)
      case _ => None
    })

  def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]] = synchronized {
    Right(
      adverts
        .collect { case ((edge, at), advert) if at == place && live.contains(edge) => advert }
        .toVector
        .sortBy(a => EdgeId.value(a.edge))
        .headOption
    )
  }

  /** Deletes the requests of `conversation`'s turns `first` to `last`, as a purge does. */
  def forget(
      conversation: grit.core.id.ConversationId,
      first: grit.core.id.TurnSeq,
      last: grit.core.id.TurnSeq
  ): Unit = synchronized {
    rows = rows.filterNot { r =>
      val t = r.request.slot.turn
      t.conversationId == conversation &&
      grit.core.id.TurnSeq.value(t.turnSeq) >= grit.core.id.TurnSeq.value(first) &&
      grit.core.id.TurnSeq.value(t.turnSeq) <= grit.core.id.TurnSeq.value(last)
    }
  }

  /** A live edge for `principal` hosting `places`, and its desk. */
  def desk(places: Set[Place], principal: PrincipalId = PrincipalId.Local): Desk^ = synchronized {
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
  def register(places: Set[Place], principal: PrincipalId = PrincipalId.Local): Registration =
    synchronized {
      sessions += 1
      val edge = EdgeId(f"edge-$sessions%04d")
      live = live.updated(edge, sessions)
      registered = registered.updated(edge, sessions)
      Registration(edge, principal, places)
    }

  /** `q` claimed for `reg`'s session, as its desk would; whether the claim won. */
  def claimAs(reg: Registration, q: ToolRequest): Boolean = synchronized {
    registered.get(reg.edge).exists(session => claim(reg, session, q))
  }

  /** The request at `slot` answered by `reg`'s session, as its desk would. */
  def answerAs(reg: Registration, slot: CallSlot, outcome: Outcome): Boolean = synchronized {
    registered.get(reg.edge).exists(session => answer(reg, session, slot, outcome))
  }

  /** What `reg`'s edge offers in `place`, as its desk would advertise it. */
  def advertiseAs(
      reg: Registration,
      place: Place,
      tools: ToolSet,
      fragments: Vector[Fragment],
      inWorkTree: Boolean = false
  ): Unit =
    advertise(reg, place, tools, fragments, inWorkTree)

  /** Ends `edge` as a crash would: it is no longer live. */
  def kill(edge: EdgeId): Unit = synchronized { live = live - edge }

  private[edge] def open(reg: Registration): Vector[ToolRequest] = synchronized {
    rows.collect { case Row(q, InMemoryEdges.Open, None) if reg.places.contains(q.workspace) => q }
  }

  private[edge] def claim(reg: Registration, session: Int, q: ToolRequest): Boolean =
    synchronized {
      rows.find(_.request.slot == q.slot) match {
        case Some(Row(_, InMemoryEdges.Open, None)) =>
          update(q.slot)(_.copy(claimedBy = Some((reg.edge, session))))
          true
        case _ => false
      }
    }

  private[edge] def origin(
      reg: Registration,
      session: Int,
      q: ToolRequest
  ): Option[grit.core.store.Origin] = synchronized {
    rows
      .find(r =>
        r.request.slot == q.slot && r.state == InMemoryEdges.Open &&
          r.claimedBy.contains((reg.edge, session))
      )
      .flatMap(r => origins.get(r.request.conversation))
  }

  private[edge] def answer(
      reg: Registration,
      session: Int,
      slot: CallSlot,
      outcome: Outcome
  ): Boolean = synchronized {
    rows.find(_.request.slot == slot) match {
      case Some(Row(_, InMemoryEdges.Open, Some(by))) if by == (reg.edge, session) =>
        update(slot)(_.copy(state = RequestState.Answered(outcome)))
        answers = answers :+ (slot, outcome)
        true
      case _ => false
    }
  }

  private[edge] def orphans(reg: Registration, session: Int): Vector[ToolRequest] = synchronized {
    val found = rows.collect {
      case Row(q, InMemoryEdges.Open, Some((edge, s)))
          if reg.places.contains(q.workspace) && !live.get(edge).contains(s) =>
        q
    }
    found.flatMap { q =>
      q.retry match {
        case Retry.Interrupt =>
          update(q.slot)(_.copy(state = RequestState.Answered(Outcome.Interrupted)))
          answers = answers :+ (q.slot, Outcome.Interrupted)
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
      fragments: Vector[Fragment],
      inWorkTree: Boolean
  ): Unit = synchronized {
    adverts = adverts.updated(
      (reg.edge, place),
      Advert(reg.edge, tools.id, fragments.map(_.id), inWorkTree)
    )
  }
}

object InMemoryEdges {

  /** Neither claimed nor settled; a row with a claim is claimed. */
  case object Open

  /** A desk on `edges` for `registration`'s edge under `session`. */
  final class Fake(edges: InMemoryEdges, val registration: Registration, session: Int)
      extends Desk {
    // Only the desk's own thread awaits: a server's, or the test's.
    @caps.unsafe.untrackedCaptures
    private var heard = edges.rungIn(registration.places)
    def await(within: FiniteDuration): Boolean = {
      val before = heard
      heard =
        try edges.ringing(registration.places, before, within)
        catch {
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            before
        }
      heard != before
    }
    def open(): Either[DeskError, Vector[ToolRequest]] = Right(edges.open(registration))
    def claim(request: ToolRequest): Either[DeskError, Boolean] =
      Right(edges.claim(registration, session, request))
    def origin(request: ToolRequest): Either[DeskError, Option[grit.core.store.Origin]] =
      Right(edges.origin(registration, session, request))
    def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean] =
      Right(edges.answer(registration, session, slot, outcome))
    def orphans(): Either[DeskError, Vector[ToolRequest]] = Right(
      edges.orphans(registration, session)
    )
    def advertise(
        place: Place,
        tools: ToolSet,
        instructions: Vector[Fragment],
        inWorkTree: Boolean
    ): Either[DeskError, Unit] =
      Right(edges.advertise(registration, place, tools, instructions, inWorkTree))
  }
}
