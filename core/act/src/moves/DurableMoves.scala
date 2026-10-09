package grit.act.moves

import java.time.Instant

import grit.act.phase.{Asking, Awaited, Calling, Hearing, WaitSteps}
import grit.core.act.{
  Acting,
  Asked,
  Called,
  Keeping,
  MoveError,
  MoveKind,
  MoveLimits,
  MoveName,
  Moves
}
import grit.core.document.DocumentKeeper
import grit.core.durable.{Durable, Journaled}
import grit.core.edge.{Permit, ToolRequests}
import grit.core.id.{CallSlot, EntryId, ToolCallId, WorkflowId}
import grit.core.message.AssistantBlock
import grit.core.model.NameRepair
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Bound, Hosted, Outcome, Repairs, ToolName, ToolSet, Toolbox}

/** A planner's moves as durable steps ([[MoveSteps]]), over [[Asking]] and [[Calling]]. */
object DurableMoves {

  /** What `body` returns, given the moves of `acting` within `limits`, made as the steps
    * [[MoveSteps]] names, after which none of them may be made. A run's call `i` (from 0) is its
    * turn's call slot `(0, i)`. An ask's cost is recorded under `move:{workflow}:{name}`.
    */
  def plain[A <: caps.Pure](acting: Acting, limits: MoveLimits, env: MovesEnv^)(
      body: Moves^ => A
  )(using d: Durable^): A =
    body(new Run(acting, limits, env))

  /** As [[plain]], its moves also keeping `keeper`'s documents. */
  def keeping[A <: caps.Pure](
      acting: Acting,
      limits: MoveLimits,
      env: MovesEnv^,
      keeper: DocumentKeeper
  )(body: Keeping^ => A)(using d: Durable^): A =
    body(new Kept(acting, limits, env, keeper))

  /** One run's moves, and its keeps over `keeper`. */
  private final class Kept(
      acting: Acting,
      limits: MoveLimits,
      env: MovesEnv^,
      keeper: DocumentKeeper
  )(using
      d: Durable^
  ) extends Run(acting, limits, env),
        Keeping {

    def keep[A <: caps.Pure: Journaled](name: MoveName)(
        body: (DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
    ): Either[MoveError, A] =
      refused(name, MoveKind.Keep).toLeft(()).flatMap { _ =>
        given Journaled[Either[String, A]] = MovesJournal.kept[A]
        val (savepoints, clock, kept) = (env.records.savepoints, env.clock, keeper)
        d.transact(MoveSteps.keep(name), subject) {
          val at = clock.now()
          savepoints.atomic((tx: Tx^) ?=> body(kept, at)(using tx)).left.map(describe)
        }.left
          .map(MoveError.Store(_))
      }
  }

  /** One run's moves. The run state below is read and written only by this object's own
    * methods, between steps.
    */
  private class Run(acting: Acting, limits: MoveLimits, env: MovesEnv^)(using d: Durable^)
      extends Moves {

    // caps.unsafe: each holds an immutable value. The object is made inside `plain` or
    // `keeping`, per run, and never leaves its `body` (whose result is pure, so it cannot carry
    // it out); every update is on the run's one thread, between steps; only this class's own
    // methods read them.
    @caps.unsafe.untrackedCaptures
    private var used: Set[MoveName] = Set.empty
    @caps.unsafe.untrackedCaptures
    private var asks: Int = 0
    @caps.unsafe.untrackedCaptures
    private var calls: Int = 0
    @caps.unsafe.untrackedCaptures
    private var diverged: Option[MoveName] = None

    private val turn = acting.turn
    protected val subject = acting.subject

    /** Why a move `name` of `kind` may not be made now; when it may, its name is spent and its
      * kind counted.
      */
    protected def refused(name: MoveName, kind: MoveKind): Option[MoveError] =
      diverged
        .map(MoveError.Diverged(_))
        .orElse(Option.when(used.contains(name))(MoveError.Repeated(name)))
        .orElse(kind match {
          case MoveKind.Ask if asks >= limits.asks => Some(MoveError.OverLimit(kind, limits.asks))
          case MoveKind.Call if calls >= limits.calls =>
            Some(MoveError.OverLimit(kind, limits.calls))
          case _ => None
        })
        .orElse {
          used = used + name
          kind match {
            case MoveKind.Ask => asks += 1
            case MoveKind.Call => calls += 1
            case MoveKind.Keep => ()
          }
          None
        }

    private def diverge(name: MoveName): MoveError = {
      diverged = diverged.orElse(Some(name))
      MoveError.Diverged(name)
    }

    def ask(name: MoveName, request: ModelRequest): Either[MoveError, Asked] =
      refused(name, MoveKind.Ask).toLeft(()).flatMap { _ =>
        import MovesJournal.given
        val digest = MovesJournal.ask(request)
        val (records, models, db, clock) = (env.records, env.models, env.db, env.clock)
        val (allowance, subject) = (acting.allowance, this.subject)
        val made = d.step(MoveSteps.ask(name)) { () =>
          db.read(subject)((tx: Tx^) ?=>
            Asking.admits(allowance, records.spending, clock.now()).map(_ -> Tx.floor(tx))
          ) match {
            case Left(e) => AskMade.Refused(AskMade.Kind.Store, describe(e), digest)
            case Right((false, _)) => AskMade.Refused(AskMade.Kind.Capped, Capped, digest)
            case Right((true, at)) =>
              models.catalog() match {
                case Left(why) =>
                  AskMade.Refused(AskMade.Kind.Model, s"no model catalog: $why", digest)
                case Right(catalog) =>
                  val provider = models.provider(catalog.pin.summary)
                  Asking.reply(provider, request, Hearing.silent(), clock) match {
                    case Left(why) => AskMade.Refused(AskMade.Kind.Model, why, digest)
                    case Right(message) =>
                      AskMade.Made(digest, message, at, records.estimator.request(request))
                  }
              }
          }
        }
        val same = MovesJournal.sameAsk(made.digest, request)
        made match {
          case AskMade.Refused(_, _, _) if !same => Left(diverge(name))
          case AskMade.Refused(AskMade.Kind.Capped, _, _) => Left(MoveError.Capped)
          case AskMade.Refused(AskMade.Kind.Model, why, _) => Left(MoveError.Model(why))
          case AskMade.Refused(AskMade.Kind.Store, why, _) => Left(MoveError.Store(why))
          case AskMade.Made(_, message, at, estimate) =>
            // Recorded even when the input diverged: the model was called, and its cost spent,
            // estimated from the request it was sent, the recorded one.
            val entry =
              EntryId(s"move:${WorkflowId.value(turn.workflowId)}:${MoveName.value(name)}")
            val ledger = records.ledger
            val turnRef = turn
            val kept = d.transact(MoveSteps.record(name), subject)(
              Asking.spent(ledger, entry, turnRef, message, estimate) match {
                // Its id is this move's alone: a duplicate is its own earlier record.
                case Left(StoreError.DuplicateId(_)) => Right(())
                case other => other.left.map(describe)
              }
            )
            if (!same) Left(diverge(name))
            else kept.left.map(MoveError.Store(_)).map(_ => Asked(message, at))
        }
      }

    def call(
        name: MoveName,
        service: Service,
        tool: ToolName,
        arguments: ujson.Obj
    ): Either[MoveError, Called] =
      refused(name, MoveKind.Call).toLeft(()).flatMap { _ =>
        CallSlot.of(turn, 0, calls - 1) match {
          case None => Left(MoveError.Store(s"no call slot ${calls - 1}"))
          case Some(cs) => sent(name, cs, service, tool, arguments)
        }
      }

    private def sent(
        name: MoveName,
        cs: CallSlot,
        service: Service,
        tool: ToolName,
        arguments: ujson.Obj
    ): Either[MoveError, Called] = {
      import MovesJournal.given
      val digest = MovesJournal.call(service, tool, arguments)
      val records = env.records
      val actsFor = acting.actsFor
      val made = d.transact(MoveSteps.call(name), subject)(
        dispatch(records, actsFor, cs, service, tool, arguments, digest)
      )
      val same = MovesJournal.sameCall(made.digest, service, tool, arguments)
      made match {
        case CallMade.Unsent(_, _, _) if !same => Left(diverge(name))
        case CallMade.Unsent(CallMade.Kind.Store, why, _) => Left(MoveError.Store(why))
        case CallMade.Unsent(_, why, _) => Right(Called.Failed(why))
        case CallMade.Sent(_) =>
          // Waited on even when the input diverged, so the run's later steps keep their places.
          val requests = records.requests
          val waited = Calling.await[String](
            requests,
            cs,
            Some(service.place),
            WaitSteps(MoveSteps.expire(name), MoveSteps.abandon(name)),
            subject
          ) match {
            case Awaited.Rung(_) | Awaited.Known(Outcome.Done(_)) =>
              d.transact(MoveSteps.answer(name), subject)((tx: Tx^) ?=>
                Calling.answer[String](requests, cs).map(called(_, Tx.floor(tx)))
              ).left
                .map(MoveError.Store(_))
            case Awaited.Known(Outcome.Interrupted) => Right(Called.Interrupted)
            case Awaited.Known(o) => Right(Called.Failed(failed(o)))
            case Awaited.Unread(why) => Left(MoveError.Store(why))
          }
          if (!same) Left(diverge(name)) else waited
      }
    }
  }

  /** A `move:{n}` call step's body: whom the call is made for, the advert at `service`'s place,
    * its entry for `tool`, `arguments` bound to it, and its request written for the edge
    * serving there, answered with its refusal when this transaction may not send it.
    */
  private def dispatch(
      records: MoveRecords,
      actsFor: grit.core.act.ActsFor,
      cs: CallSlot,
      service: Service,
      tool: ToolName,
      arguments: ujson.Obj,
      digest: String
  )(using Tx^): CallMade = {
    def unsent(kind: CallMade.Kind, why: String): Either[CallMade, Nothing] =
      Left(CallMade.Unsent(kind, why, digest))
    def stored[A](e: Either[StoreError, A]): Either[CallMade, A] =
      e.left.map(err => CallMade.Unsent(CallMade.Kind.Store, describe(err), digest))
    val place = service.place
    val made = for {
      principal <- stored(Calling.principal(actsFor, cs, records.askers, records.schedules))
        .flatMap(_.fold(unsent(CallMade.Kind.Refused, Nobody))(Right(_)))
      advert <- stored(records.edges.serving(place))
        .flatMap(
          _.fold(unsent(CallMade.Kind.Unserved, failed(Calling.unserved(service))))(Right(_))
        )
      set <- stored(records.toolSets.get(advert.tools))
      entry <- set
        .named(tool)
        .fold(unsent(CallMade.Kind.Unadvertised, unadvertised(service, tool)))(Right(_))
      hosted <- bound(entry, cs, arguments).left.map(why =>
        CallMade.Unsent(CallMade.Kind.Refused, why, digest)
      )
      request = Calling.request(cs, hosted, place, Permit.Free, principal)
      refusal = ToolRequests.refusal(request)
      went <- stored(Calling.dispatch(records.requests, records.edges, place, Vector(request)))
      _ <-
        if (went) Right(()) else unsent(CallMade.Kind.Unserved, failed(Calling.unserved(service)))
      _ <- refusal.fold[Either[CallMade, Unit]](Right(()))(o =>
        unsent(CallMade.Kind.Refused, failed(o))
      )
    } yield CallMade.Sent(digest)
    made.merge
  }

  /** A call of `entry` with `arguments`, bound as an advertised tool is: its destination read
    * from the arguments' `to`; why not, when it asks first or its arguments do not bind.
    */
  private def bound(
      entry: ToolSet.Entry,
      cs: CallSlot,
      arguments: ujson.Obj
  ): Either[String, Bound.Hosted] =
    Hosted.advertised(entry) match {
      case None => Left(failed(Calling.unapproved(entry.name)))
      case Some(offered) =>
        // A planner's arguments are its code's, not a model's: nothing is repaired.
        val asSent = Repairs(NameRepair.AsSent, Set.empty)
        val sent: AssistantBlock.ToolCall =
          AssistantBlock.ToolCall(ToolCallId(cs.key), ToolName.value(entry.name), arguments)
        Toolbox
          .of[{}](offered)
          .left
          .map(_ => "unreachable: one tool")
          .flatMap(_.bind(sent, asSent).left.map(_.message))
          .flatMap {
            case h: Bound.Hosted => Right(h)
            case _ => Left(s"${ToolName.value(entry.name)} is not a hosted tool")
          }
    }

  /** What an outcome is to a planner, its answer holding nothing above `at`. */
  private def called(o: Outcome, at: grit.core.visibility.Label): Called = o match {
    case Outcome.Done(text) => Called.Done(text, at)
    case Outcome.Interrupted => Called.Interrupted
    case other => Called.Failed(failed(other))
  }

  private def failed(o: Outcome): String =
    o.result(ToolCallId("move")).content

  private def unadvertised(service: Service, tool: ToolName): String =
    s"${service.name} does not offer ${ToolName.value(tool)}, so this call did not run."

  private val Nobody = "There is nobody to make this call for, so it did not run."

  private val Capped = "The day's spend reached its cap, so no model was asked."

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
