package grit.act.moves

import java.time.Instant

import grit.act.phase.{
  Asking,
  Awaited,
  Calling,
  Classifying,
  Hearing,
  Shaped,
  Shaping,
  WaitSteps,
  Waiting
}
import grit.core.act.{Acting, Allowance, Called, MoveError, MoveName, Posed}
import grit.core.classify.{AnswersJson, Ask, ClassifierError, Reply, Request}
import grit.core.clock.Clock
import grit.core.document.DocumentKeeper
import grit.core.durable.{Flow, Journaled, Step, StepName}
import grit.core.edge.{Permit, ToolRequests}
import grit.core.flow.{Downgrade, Mint}
import grit.core.id.{CallSlot, EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{NameRepair, Pinned}
import grit.core.place.Service
import grit.core.provider.{ModelRequest, Models}
import grit.core.route.{Routed, Uncleared}
import grit.core.schema.Typed
import grit.core.store.{Db, StoreError, Tx, UsageLedger}
import grit.core.tool.{Bound, Hosted, Outcome, Repairs, ToolName, ToolSet, Toolbox}
import grit.core.visibility.{Integrity, Label, Subject}

/** The names a move's steps are recorded under, from the input each step runs over: the move
  * itself as `move`, named for what it is (`ask`, `call` or `keep`), then its phases, each the
  * move's name and the phase's: an ask's cost recorded (`record`), a call's wait given up on
  * (`expire`, `abandon`) and its answer read (`answer`). A stored form: a run in flight
  * replays by them.
  */
final class ActName[-I] private (
    val move: StepName[I],
    val record: StepName[I],
    val expire: StepName[I],
    val abandon: StepName[I],
    val answer: StepName[I]
)

object ActName {

  /** `family:key` for the move, and `family:key:phase` for its phases, over an `I`:
    * `keyed[I](family)(key)`, the key from the input.
    */
  def keyed[I]: Keying[I] = new Keying[I]

  /** An act's names over an `I`, waiting for their family and key. */
  final class Keying[I] private[ActName] () {

    /** The names of the family `family`, keyed by `key`. */
    inline def apply[N <: String & Singleton](inline family: N)(key: I -> String): ActName[I] =
      of[I](
        StepName.keyed[I](family)(key),
        StepName.keyed[I](family)(i => s"${key(i)}:record"),
        StepName.keyed[I](family)(i => s"${key(i)}:expire"),
        StepName.keyed[I](family)(i => s"${key(i)}:abandon"),
        StepName.keyed[I](family)(i => s"${key(i)}:answer")
      )
  }

  private def of[I](
      move: StepName[I],
      record: StepName[I],
      expire: StepName[I],
      abandon: StepName[I],
      answer: StepName[I]
  ): ActName[I] = new ActName(move, record, expire, abandon, answer)
}

/** A planner's moves' steps, as flows over [[MovesEnv]]: each move made once, recorded under the
  * names [[Act.Names]] gives it, its bodies acting only through the env a run hands them.
  * Whether a move may be made, and what its records come to, are [[DurableMoves]]'.
  */
private[moves] object Act {

  /** The input of a move's steps: the move `name`. */
  trait Move {
    def name: MoveName
  }

  /** A planner's move: one made under `acting`. */
  trait Acted extends Move {
    def acting: Acting
  }

  /** An ask's `ask:{name}`, and `ask:{name}:{phase}` for its phases. */
  val Asks: ActName[Move] = ActName.keyed[Move]("ask")(m => MoveName.value(m.name))

  /** A call's `call:{name}`, and `call:{name}:{phase}` for its phases. */
  val Calls: ActName[Move] = ActName.keyed[Move]("call")(m => MoveName.value(m.name))

  /** A keep's `keep:{name}`. */
  val Keeps: ActName[Move] = ActName.keyed[Move]("keep")(m => MoveName.value(m.name))

  /** The text ask `name` of `request`, under `digest`. */
  final case class Text(name: MoveName, acting: Acting, request: ModelRequest, digest: String)
      extends Acted

  /** The JSON ask `name` of `system` then `messages`, its reply read by `reply`, under `digest`.
    */
  final case class Json[T](
      name: MoveName,
      acting: Acting,
      system: String,
      messages: Vector[Message],
      reply: Typed[T],
      digest: String
  ) extends Acted

  /** The judgment `name` of `request`, its answers read by `questions`, under `digest`. */
  final case class Judgment[T](
      name: MoveName,
      acting: Acting,
      request: Request,
      questions: Ask[?, T],
      digest: String
  ) extends Acted

  /** The call `name` of `tool` at `service` with `arguments`, believed at `believed`, as the
    * call at `cs`, under `digest`.
    */
  final case class Call(
      name: MoveName,
      acting: Acting,
      cs: CallSlot,
      service: Service,
      tool: ToolName,
      arguments: ujson.Obj,
      believed: Integrity,
      digest: String
  ) extends Acted

  /** The keep `name` of `value` into `keeper`'s documents. */
  final case class Keep[I](name: MoveName, acting: Acting, value: I, keeper: DocumentKeeper)
      extends Acted

  /** What an ask's `ask:{n}` recorded, and its `ask:{n}:record`'s result: `Right` when it
    * recorded nothing to keep.
    */
  final case class Settling(made: AskMade, kept: Either[String, Unit])

  /** What a call came to: its `call:{n}` unsent, `why`, under `digest`; or sent, under
    * `digest`, and then what its wait and answer came to.
    */
  enum CallCame {
    case Unsent(kind: CallMade.Kind, why: String, digest: String)
    case Sent(digest: String, waited: Either[MoveError, Called])
  }

  /** The text ask's `ask:{n}`: admitted, then the model's reply. */
  val text: Step[MovesEnv, Text, AskMade] = {
    import MovesJournal.given
    Step[MovesEnv, Text, AskMade](Asks.move)(env =>
      t =>
        admitted(env.records, env.models, env.db, env.clock, t.acting, t.digest) match {
          case Left(refusal) => refusal
          case Right((at, pin)) =>
            Asking.reply(env.models.provider(pin), t.request, Hearing.silent(), env.clock) match {
              case Left(why) => AskMade.Refused(AskMade.Kind.Model, why, t.digest)
              case Right(message) =>
                AskMade.Made(t.digest, message, at, env.records.estimator.request(t.request))
            }
        }
    )
  }

  /** The JSON ask's `ask:{n}`: admitted, then the model's reply, shaped and repaired. */
  def json[T]: Step[MovesEnv, Json[T], AskMade] = {
    import MovesJournal.given
    Step[MovesEnv, Json[T], AskMade](Asks.move)(env =>
      j =>
        admitted(env.records, env.models, env.db, env.clock, j.acting, j.digest) match {
          case Left(refusal) => refusal
          case Right((at, pin)) =>
            val settings = pin.settings
            val request =
              Shaping.request(
                j.system,
                j.messages,
                Posed.ReplyTool,
                j.reply.schema,
                settings.strict
              )
            val estimator = env.records.estimator
            def costs(calls: Vector[Shaped.Call]): Vector[AskMade.Call] =
              calls.map(c =>
                AskMade.Call(
                  c.answer.model,
                  c.answer.usage,
                  estimator.request(c.sent),
                  c.answer.upstream
                )
              )
            Shaping.shaped(
              env.models.provider(pin),
              request,
              Posed.ReplyTool,
              j.reply,
              settings.names,
              settings.repairs,
              Posed.Json.Repairs,
              env.clock
            ) match {
              case Shaped.Read(conforming, _, calls) =>
                AskMade.Shaped(j.digest, conforming.text, at, j.acting.integrity, costs(calls))
              case Shaped.Unread(why, calls) => AskMade.Unshaped(j.digest, why, costs(calls))
              case Shaped.Failed(why, Vector()) =>
                AskMade.Refused(AskMade.Kind.Model, why, j.digest)
              case Shaped.Failed(why, calls) => AskMade.Unshaped(j.digest, why, costs(calls))
            }
        }
    )
  }

  /** The judgment's `ask:{n}`: admitted, then the classifier's answers, asked only when its
    * service is cleared for what the admitting read knew.
    */
  def judgment[T]: Step[MovesEnv, Judgment[T], AskMade] = {
    import MovesJournal.given
    Step[MovesEnv, Judgment[T], AskMade](Asks.move)(env =>
      j =>
        admits(env.records, env.db, env.clock, j.acting, j.digest) match {
          case Left(refusal) => refusal
          case Right((at, routed)) =>
            env.classifier.at(routed) match {
              case Left(u) => uncleared(u, "no classifier", j.digest)
              case Right(asked) =>
                Classifying.replied(asked, j.request, env.clock) match {
                  case Left(error) =>
                    AskMade.Refused(AskMade.Kind.Model, unanswered(error), j.digest)
                  case Right(reply) =>
                    val call = classified(env.records, j.request, reply, env.classifier.upstream)
                    reply.answers.flatMap(a => j.questions.read(a.answers).map(_ => a)) match {
                      case Left(error) => AskMade.Unjudged(j.digest, unanswered(error), call)
                      case Right(answers) =>
                        val written =
                          ujson.write(ujson.Arr.from(answers.answers.map(AnswersJson.write)))
                        AskMade.Judged(j.digest, written, at, call)
                    }
                }
            }
        }
    )
  }

  /** The costs of an ask's model or classifier `calls`, recorded in its `ask:{n}:record` for
    * `turn`, under `workflow`, in a transaction opened for `subject`; and what its `ask:{n}`
    * `made`.
    */
  private[moves] final case class Spent[+A](
      name: MoveName,
      turn: TurnRef,
      workflow: WorkflowId,
      subject: Subject,
      calls: Vector[AskMade.Call],
      made: A
  ) extends Move

  private[moves] val recorded: Step[MovesEnv, Spent[Any], Either[String, Unit]] = {
    import MovesJournal.given
    Step.transact[MovesEnv, Spent[Any], Either[String, Unit]](Asks.record, _.subject)(env =>
      s => {
        val base = s"move:${WorkflowId.value(s.workflow)}:${MoveName.value(s.name)}"
        costed(env.records.ledger, s.turn, s.workflow, base, s.calls)
      }
    )
  }

  /** An ask made by `made`, then, when its record holds model calls, their costs recorded,
    * whatever shape recorded them: a run replays its steps by place, so a record step the first
    * run took is taken again.
    */
  def ask[I <: Acted](made: Step[MovesEnv, I, AskMade]): Flow[MovesEnv, I, Nothing, Settling] =
    settled(Flow.step(made).map(m => (m, ()))).map(_._1)

  /** As [[ask]], with `node` made over what `of` takes from what `made` recorded, beside it. */
  def raised[I <: Acted, A, B](made: Step[MovesEnv, I, AskMade], node: Downgrade[A, B])(
      of: AskMade -> Option[A]
  ): Flow[MovesEnv, I, Nothing, (Settling, Option[B])] =
    settled(node.on(made)(of))

  /** `head`'s ask, then its costs recorded as [[ask]] says, passing `head`'s `X` beside it. */
  private def settled[I <: Acted, X](
      head: Flow[MovesEnv, I, Nothing, (AskMade, X)]
  ): Flow[MovesEnv, I, Nothing, (Settling, X)] =
    head.keeping
      .map((in, made) => {
        val (m, x) = made
        if (m.spent.isEmpty) Right((Settling(m, Right(())), x))
        else {
          val turn = in.acting.turn
          Left(Spent(in.name, turn, turn.workflowId, in.acting.subject, m.spent, made))
        }
      })
      .andThen(
        Flow.either(
          (Flow.step(recorded): Flow[
            MovesEnv,
            Spent[(AskMade, X)],
            Nothing,
            Either[String, Unit]
          ]).keeping
            .map((s, kept) => (Settling(s.made._1, kept), s.made._2)),
          Flow.pass[(Settling, X)]
        )
      )

  /** A sent call, and what its `call:{n}` recorded. */
  private final case class Sending(call: Call, digest: String) extends Acted {
    def name: MoveName = call.name
    def acting: Acting = call.acting
  }

  /** A call: its request written for its edge (`call:{n}`); when sent, waited on (the call's
    * `expire` and `abandon`, as [[Calling.await]] takes them, waited on even
    * when its input diverged, so the run's later steps keep their places), and its answer read
    * once its edge rang or answered done (`answer`), believed at the meet of its service's
    * results and its arguments; one recorded with no integrity is read at `start`, what its
    * run starts believed at.
    */
  def call(start: Integrity): Flow[MovesEnv, Call, Nothing, CallCame] = {
    import MovesJournal.given
    val sent = Step.transact[MovesEnv, Call, CallMade](Calls.move, _.acting.subject)(env =>
      c => dispatch(env.records, c)
    )
    val answered = {
      given Journaled[Either[String, Called]] = MovesJournal.answered(start)
      Step.transact[MovesEnv, Sending, Either[String, Called]](Calls.answer, _.acting.subject)(
        env =>
          s =>
            (tx: Tx^) ?=> {
              val c = s.call
              val believed = Tx.visibility(tx).outputOf(c.service).meet(c.believed)
              Calling
                .answer[String](env.records.requests, c.cs)
                .map(called(_, Tx.floor(tx), believed))
            }
      )
    }
    val awaiting: Flow[MovesEnv, Sending, Nothing, Awaited[String]] =
      Calling
        .await[Sending, String](
          WaitSteps(Calls.expire, Calls.abandon),
          s => Waiting(s.call.cs, Some(s.call.service.place), s.acting.subject, None)
        )
        .over[MovesEnv](env => env.records.requests)
    val waited: Flow[MovesEnv, Sending, Nothing, Either[MoveError, Called]] =
      awaiting.keeping
        .map {
          case (s, Awaited.Rung(_) | Awaited.Answered(Outcome.Done(_, _, _))) => Left(s)
          case (_, Awaited.Answered(Outcome.Interrupted) | Awaited.Known(Outcome.Interrupted)) =>
            Right(Right(Called.Interrupted))
          case (_, Awaited.Answered(o)) => Right(Right(Called.Failed(failed(o))))
          case (_, Awaited.Known(o)) => Right(Right(Called.Failed(failed(o))))
          case (_, Awaited.Unread(why)) => Right(Left(MoveError.Store(why)))
        }
        .andThen(
          Flow.either(
            Flow.step(answered).map(_.left.map(MoveError.Store(_))),
            Flow.pass[Either[MoveError, Called]]
          )
        )
    Flow
      .step(sent)
      .keeping
      .map {
        case (c, CallMade.Sent(digest)) => Left(Sending(c, digest))
        case (_, CallMade.Unsent(kind, why, digest)) => Right(CallCame.Unsent(kind, why, digest))
      }
      .andThen(
        Flow.either(
          waited.keeping.map((s, w) => CallCame.Sent(s.digest, w)),
          Flow.pass[CallCame]
        )
      )
  }

  /** A keep: `body` over its value and documents, `at` its step's clock, in one transaction
    * under a savepoint, so nothing it wrote is kept when it is `Left` or the store fails: its
    * result, or why nothing was kept.
    */
  def keep[I, A](body: (I, DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A])(using
      Journaled[A]
  ): Flow[MovesEnv, Keep[I], Nothing, Either[String, A]] = {
    given Journaled[Either[String, A]] = MovesJournal.kept[A]
    Flow.step(
      Step.transact[MovesEnv, Keep[I], Either[String, A]](Keeps.move, _.acting.subject)(env =>
        k => {
          val at = env.clock.now()
          env.records.savepoints
            .atomic((tx: Tx^) ?=> body(k.value, k.keeper, at)(using tx))
            .left
            .map(describe)
        }
      )
    )
  }

  /** Whether an ask may be made, read in one transaction for `acting`'s subject: the label
    * what it is made from may hold, and the first of the catalog's pins for summaries cleared
    * for what that read knew; or its refusal, under `digest`, when [[admits]] refuses it, no
    * catalog reads, or no pin is cleared.
    */
  private def admitted(
      records: MoveRecords,
      models: Models^,
      db: Db^,
      clock: Clock^,
      acting: Acting,
      digest: String
  ): Either[AskMade, (Label, Pinned)] =
    admits(records, db, clock, acting, digest).flatMap { (at, routed) =>
      models.catalog() match {
        case Left(why) =>
          Left(AskMade.Refused(AskMade.Kind.Model, s"no model catalog: $why", digest))
        case Right(catalog) =>
          routed
            .cleared(catalog.pin.summary)
            .map(pins => (at, pins.first))
            .toRight(uncleared(Uncleared(routed.classification.label), "no model", digest))
      }
    }

  /** An ask refused, under `digest`, because `what` it could go to is cleared for `u`. */
  private def uncleared(u: Uncleared, what: String, digest: String): AskMade =
    AskMade.Refused(
      AskMade.Kind.Uncleared,
      s"$what cleared for ${Label.written(u.classification)}",
      digest,
      Some(u.classification)
    )

  /** Whether `acting`'s allowance admits an ask, read in one transaction for its subject: the
    * label what it is made from may hold, and what that read was routed for; or its refusal,
    * under `digest`, when the store fails or the allowance does not admit it.
    */
  private def admits(
      records: MoveRecords,
      db: Db^,
      clock: Clock^,
      acting: Acting,
      digest: String
  ): Either[AskMade, (Label, Routed)] =
    admission(records, db, clock, acting.allowance, acting.subject) match {
      case Left(e) => Left(AskMade.Refused(AskMade.Kind.Store, describe(e), digest))
      case Right((false, _, _)) => Left(AskMade.Refused(AskMade.Kind.Capped, Capped, digest))
      case Right((true, at, routed)) => Right((at, routed))
    }

  /** Whether `allowance` admits an ask now, read in one transaction for `subject`, with the
    * label what it is made from may hold and what that read was routed for.
    */
  private[moves] def admission(
      records: MoveRecords,
      db: Db^,
      clock: Clock^,
      allowance: Allowance,
      subject: Subject
  ): Either[StoreError, (Boolean, Label, Routed)] =
    db.read(subject)((tx: Tx^) ?=>
      Asking.admits(allowance, records.spending, clock.now()).map((_, Tx.floor(tx), Routed.of))
    )

  /** The cost of a classifier's call, replying `reply` to `request`, served by `upstream`:
    * charged however its answers read.
    */
  private[moves] def classified(
      records: MoveRecords,
      request: Request,
      reply: Reply,
      upstream: Option[String]
  ): AskMade.Call =
    AskMade.Call(
      reply.model,
      reply.usage,
      records.estimator.system(Request.json(request).render()),
      upstream
    )

  /** `calls`' costs recorded for `turn`, kept under `workflow`: the first under `base`, the first repair's under
    * `{base}:repair`, a later one's under `{base}:repair{i}`. Each id is this move's alone, so
    * one recorded already is its own earlier record.
    */
  private def costed(
      ledger: UsageLedger,
      turn: TurnRef,
      workflow: WorkflowId,
      base: String,
      calls: Vector[AskMade.Call]
  )(using Tx^): Either[String, Unit] =
    calls.zipWithIndex
      .foldLeft[Either[StoreError, Unit]](Right(())) { case (done, (c, i)) =>
        done.flatMap { _ =>
          val entry = EntryId(i match {
            case 0 => base
            case 1 => s"$base:repair"
            case _ => s"$base:repair$i"
          })
          Asking.cost(
            ledger,
            entry,
            turn,
            workflow,
            c.model,
            c.upstream,
            c.usage,
            c.estimate
          ) match {
            case Left(StoreError.DuplicateId(_)) => Right(())
            case other => other
          }
        }
      }
      .left
      .map(describe)

  /** Why a classifier gave no answer that reads. */
  def unanswered(error: ClassifierError): String = error match {
    case ClassifierError.Unavailable(cause) => s"the classifier is unavailable: $cause"
    case ClassifierError.Unreadable(why) => s"the classifier's answers do not read: $why"
  }

  /** A call's `call:{n}` body: whom the call is made for, the advert at its service's place,
    * its entry for its tool, its arguments bound to it, and its request written for the edge
    * serving there, answered with its refusal when this transaction may not send it.
    */
  private def dispatch(records: MoveRecords, c: Call)(using Tx^): CallMade = {
    val digest = c.digest
    def unsent(kind: CallMade.Kind, why: String): Either[CallMade, Nothing] =
      Left(CallMade.Unsent(kind, why, digest))
    def stored[A](e: Either[StoreError, A]): Either[CallMade, A] =
      e.left.map(err => CallMade.Unsent(CallMade.Kind.Store, describe(err), digest))
    val place = c.service.place
    val unserved = failed(Calling.unserved(c.service))
    val made = for {
      principal <- stored(
        Calling.principal(c.acting.actsFor, c.cs, records.askers, records.schedules)
      ).flatMap(_.fold(unsent(CallMade.Kind.Refused, Nobody))(Right(_)))
      advert <- stored(records.edges.serving(place))
        .flatMap(_.fold(unsent(CallMade.Kind.Unserved, unserved))(Right(_)))
      set <- stored(records.toolSets.get(advert.tools))
      entry <- set
        .named(c.tool)
        .fold(unsent(CallMade.Kind.Unadvertised, unadvertised(c.service, c.tool)))(Right(_))
      hosted <- bound(entry, c.cs, c.arguments, c.believed).left.map(why =>
        CallMade.Unsent(CallMade.Kind.Refused, why, digest)
      )
      request = Calling.request(c.cs, hosted, place, Permit.Free, principal)
      // A job's run holds no routed classification: it sends at what its transaction knows.
      at = Tx.classification
      refusal = ToolRequests.refusal(request, at)
      went <- stored(
        Calling.dispatch(records.requests, records.edges, place, Vector(request), at)
      )
      _ <- if (went) Right(()) else unsent(CallMade.Kind.Unserved, unserved)
      _ <- refusal.fold[Either[CallMade, Unit]](Right(()))(o =>
        unsent(CallMade.Kind.Refused, failed(o))
      )
    } yield CallMade.Sent(digest)
    made.merge
  }

  /** A call of `entry` with `arguments`, `believed` at that, bound as an advertised tool is: its
    * destination read from the arguments' `to`; why not, when it asks first, its arguments do
    * not bind, or they are believed less than it requires
    * ([[grit.core.tool.CallError.Withheld]]).
    */
  private def bound(
      entry: ToolSet.Entry,
      cs: CallSlot,
      arguments: ujson.Obj,
      believed: Integrity
  ): Either[String, Bound.Hosted] =
    Hosted.advertised(entry) match {
      case None => Left(failed(Calling.unapproved(entry.name)))
      case Some(offered) =>
        // A planner's arguments are its code's, not a model's: nothing is repaired, and they
        // are stamped as believed.
        val asSent = Repairs(NameRepair.AsSent, Set.empty)
        val sent = Mint.tainted[AssistantBlock.ToolCall](
          AssistantBlock.ToolCall(ToolCallId(cs.key), ToolName.value(entry.name), arguments),
          believed
        )
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

  /** What an outcome is to a planner, its answer holding nothing above `at` and believed at
    * `believed`.
    */
  private def called(o: Outcome, at: Label, believed: Integrity): Called = o match {
    case Outcome.Done(text, _, _) => Called.Done(Mint.tainted(text, believed), at)
    case Outcome.Interrupted => Called.Interrupted
    case other => Called.Failed(failed(other))
  }

  private def failed(o: Outcome): String =
    o.result(ToolCallId("move")).content

  private def unadvertised(service: Service, tool: ToolName): String =
    s"${service.name} does not offer ${ToolName.value(tool)}, so this call did not run."

  private val Nobody = "There is nobody to make this call for, so it did not run."

  private[moves] val Capped = "The day's spend reached its cap, so no model was asked."

  private[moves] def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
