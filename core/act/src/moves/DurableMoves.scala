package grit.act.moves

import java.time.Instant

import grit.core.act.{
  Acting,
  Asked,
  Called,
  Extraction,
  Keeping,
  MoveError,
  MoveKind,
  MoveLimits,
  MoveName,
  Moves,
  Posed
}
import grit.core.classify.{Answer, AnswersJson, Ask, Request}
import grit.core.document.DocumentKeeper
import grit.core.durable.{Flow, Journaled, Steps}
import grit.core.flow.{Endorse, Extracted, Mint, Shape, Tainted, Trusted}
import grit.core.id.CallSlot
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.schema.Typed
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** A planner's moves as durable steps ([[Act]]'s), over `grit.act.phase`'s `Asking` and
  * `Calling`.
  */
object DurableMoves {

  /** What `body` returns, given the moves of `acting` within `limits`, made through `steps`,
    * after which none of them may be made: each move recorded as `move:{name}`, and its phases
    * as `move:{name}:record`, `:expire`, `:abandon` and `:answer` ([[ActName]]). A run's call
    * `i` (from 0) is its turn's call slot `(0, i)`. An ask's cost is recorded under
    * `move:{workflow}:{name}`, and a JSON ask's repair's under `move:{workflow}:{name}:repair`.
    */
  def plain[A <: caps.Pure](acting: Acting, limits: MoveLimits)(
      body: Moves^ => A
  )(using steps: Steps[MovesEnv]^): A =
    body(new Run(acting, limits))

  /** `run`, believed at what `acting`'s run starts at ([[Acting.integrity]]). */
  def started[P <: caps.Pure](run: JobRun[P], acting: Acting): Trusted[JobRun[P]] =
    Mint.trusted(run, acting.integrity)

  /** As [[plain]], its moves also keeping `keeper`'s documents. */
  def keeping[A <: caps.Pure](acting: Acting, limits: MoveLimits, keeper: DocumentKeeper)(
      body: Keeping^ => A
  )(using steps: Steps[MovesEnv]^): A =
    body(new Kept(acting, limits, keeper))

  /** One run's moves, and its keeps over `keeper`. */
  private final class Kept(acting: Acting, limits: MoveLimits, keeper: DocumentKeeper)(using
      steps: Steps[MovesEnv]^
  ) extends Run(acting, limits),
        Keeping {

    def keep[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
        body: (I, DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
    ): Either[MoveError, A] =
      refused(name, MoveKind.Keep).toLeft(()).flatMap { _ =>
        if (from.integrity.belowOutside) Left(MoveError.Below(from.integrity))
        else {
          val kept = DocumentKeeper.believedAtMost(keeper, from.integrity)
          val value = Trusted.asIs(from).extract
          made(Act.keep[I, A](body), Act.Keep(name, acting, value, kept)).left
            .map(MoveError.Store(_))
        }
      }
  }

  /** One run's moves. The run state below is read and written only by this object's own
    * methods, between steps.
    */
  private class Run(acting: Acting, limits: MoveLimits)(using steps: Steps[MovesEnv]^)
      extends Moves {

    // caps.unsafe: each holds an immutable value. The object is made inside `plain` or
    // `keeping`, per run, and never leaves its `body` (whose result is pure, so it cannot carry
    // it out); every update is on the run's one thread, between steps; only this class's own
    // methods read them. A read of the run's history runs the same code over the same step
    // results, so it makes the same updates.
    @caps.unsafe.untrackedCaptures
    private var used: Set[MoveName] = Set.empty
    @caps.unsafe.untrackedCaptures
    private var asks: Int = 0
    @caps.unsafe.untrackedCaptures
    private var judgments: Int = 0
    @caps.unsafe.untrackedCaptures
    private var calls: Int = 0
    @caps.unsafe.untrackedCaptures
    private var diverged: Option[MoveName] = None

    private val turn = acting.turn

    protected def sanitiser: Endorse.Sanitiser^ = Mint.sanitiser

    /** What `flow`, a move's steps, passed from `in`, made next through the run's steps. */
    protected def made[I, O](flow: Flow[MovesEnv, I, Nothing, O], in: I): O =
      steps.run(flow, in).merge

    /** Why a move `name` of `kind` may not be made now; when it may, its name is spent and its
      * kind counted.
      */
    protected def refused(name: MoveName, kind: MoveKind): Option[MoveError] =
      diverged
        .map(MoveError.Diverged(_))
        .orElse(Option.when(used.contains(name))(MoveError.Repeated(name)))
        .orElse(kind match {
          case MoveKind.Ask if asks >= limits.asks => Some(MoveError.OverLimit(kind, limits.asks))
          case MoveKind.Judge if judgments >= limits.judgments =>
            Some(MoveError.OverLimit(kind, limits.judgments))
          case MoveKind.Call if calls >= limits.calls =>
            Some(MoveError.OverLimit(kind, limits.calls))
          case _ => None
        })
        .orElse {
          used = used + name
          kind match {
            case MoveKind.Ask => asks += 1
            case MoveKind.Judge => judgments += 1
            case MoveKind.Call => calls += 1
            case MoveKind.Keep => ()
          }
          None
        }

    private def diverge(name: MoveName): MoveError = {
      diverged = diverged.orElse(Some(name))
      MoveError.Diverged(name)
    }

    def ask[R](name: MoveName, posed: Tainted[Posed[R]]): Either[MoveError, Asked[R]] = {
      val believed = posed.integrity
      Trusted.asIs(posed).extract match {
        case Posed.Text(request) => text(name, request, believed)
        case Posed.Json(system, messages, reply) => json(name, system, messages, reply, believed)
        case Posed.Judgment(request, questions) => judgment(name, request, questions, believed)
      }
    }

    /** The text ask `name` of `request`, its reply `believed` at that: its step, recorded form
      * and digest are the ones every job's history holds.
      */
    private def text(
        name: MoveName,
        request: ModelRequest,
        believed: Integrity
    ): Either[MoveError, Asked[Message.Assistant]] =
      refused(name, MoveKind.Ask).toLeft(()).flatMap { _ =>
        val digest = MovesJournal.ask(request)
        val asked = made(Act.ask(Act.text), Act.Text(name, acting, request, digest))
        settled(name, asked, MovesJournal.sameAsk(asked.made.digest, request)) {
          case AskMade.Made(_, message, at, _) =>
            Some(Right(Asked(Mint.tainted(message, believed), at)))
          case _ => None
        }
      }

    /** The JSON ask `name` of `system` then `messages`, its reply read by `reply` and `believed`
      * at that.
      */
    private def json[T](
        name: MoveName,
        system: String,
        messages: Vector[Message],
        reply: Typed[T],
        believed: Integrity
    ): Either[MoveError, Asked[T]] =
      refused(name, MoveKind.Ask).toLeft(()).flatMap { _ =>
        val digest = MovesJournal.json(system, messages, reply.schema)
        val asked =
          made(Act.ask(Act.json[T]), Act.Json(name, acting, system, messages, reply, digest))
        val same = MovesJournal.sameJson(asked.made.digest, system, messages, reply.schema)
        settled(name, asked, same) {
          case AskMade.Shaped(_, written, at, _) =>
            Some(readBack(reply, written).map(r => Asked(Mint.tainted(r, believed), at)))
          case _ => None
        }
      }

    /** The judgment `name` of `request`, its answers read by `questions` and `believed` at that.
      */
    private def judgment[T](
        name: MoveName,
        request: Request,
        questions: Ask[?, T],
        believed: Integrity
    ): Either[MoveError, Asked[T]] =
      refused(name, MoveKind.Judge).toLeft(()).flatMap { _ =>
        val digest = MovesJournal.judgment(request)
        val asked =
          made(Act.ask(Act.judgment[T]), Act.Judgment(name, acting, request, questions, digest))
        settled(name, asked, MovesJournal.sameJudgment(asked.made.digest, request)) {
          case AskMade.Judged(_, written, at, _) =>
            Some(judgedBack(questions, written).map(r => Asked(Mint.tainted(r, believed), at)))
          case _ => None
        }
      }

    /** What an ask comes to once its steps recorded `asked` ([[Act.ask]]): [[MoveError.Diverged]]
      * unless `same`; else the record step's failure (which outranks a refusal or an unread
      * reply, since calls were paid for and their cost is lost), its refusal, an unread reply's
      * [[MoveError.Model]], or `read` of what it made (`None`: a record of another shape, which
      * diverges).
      */
    private def settled[R](name: MoveName, asked: Act.Settling, same: Boolean)(
        read: AskMade -> Option[Either[MoveError, Asked[R]]]
    ): Either[MoveError, Asked[R]] =
      if (!same) Left(diverge(name))
      else
        (asked.made, asked.kept) match {
          case (_, Left(why)) => Left(MoveError.Store(why))
          case (AskMade.Refused(AskMade.Kind.Capped, _, _, _), _) => Left(MoveError.Capped)
          case (AskMade.Refused(AskMade.Kind.Model, why, _, _), _) => Left(MoveError.Model(why))
          case (AskMade.Refused(AskMade.Kind.Store, why, _, _), _) => Left(MoveError.Store(why))
          // The journal reads no uncleared refusal without its label.
          case (AskMade.Refused(AskMade.Kind.Uncleared, why, _, at), _) =>
            Left(at.fold(MoveError.Model(why))(MoveError.Uncleared(_)))
          case (AskMade.Unshaped(_, why, _), _) => Left(MoveError.Model(why))
          case (AskMade.Unjudged(_, why, _), _) => Left(MoveError.Model(why))
          case (other, Right(())) => read(other).getOrElse(Left(diverge(name)))
        }

    def call(
        name: MoveName,
        service: Service,
        tool: ToolName,
        arguments: Tainted[ujson.Obj]
    ): Either[MoveError, Called] =
      refused(name, MoveKind.Call).toLeft(()).flatMap { _ =>
        CallSlot.of(turn, 0, calls - 1) match {
          case None => Left(MoveError.Store(s"no call slot ${calls - 1}"))
          case Some(cs) => sent(name, cs, service, tool, arguments)
        }
      }

    def extract(
        name: MoveName,
        from: Tainted[String],
        question: String,
        shape: Shape
    ): Either[MoveError, Option[Trusted[Extracted]]] =
      Extraction.reply(shape) match {
        case Left(why) => Left(MoveError.Model(why))
        case Right(typed) =>
          given (Endorse.Extraction^) = Mint.extraction(acting.integrity)
          val messages = Extraction.messages(Trusted.asIs(from).extract, question, shape)
          json(name, Extraction.System, messages, typed, from.integrity).map(a =>
            Trusted.asIs(a.reply).extract.map(x => Trusted.endorse(a.reply.map(_ => x)))
          )
      }

    private def sent(
        name: MoveName,
        cs: CallSlot,
        service: Service,
        tool: ToolName,
        tainted: Tainted[ujson.Obj]
    ): Either[MoveError, Called] = {
      val (arguments, believed) = (Trusted.asIs(tainted).extract, tainted.integrity)
      val digest = MovesJournal.call(service, tool, arguments)
      val call = Act.Call(name, acting, cs, service, tool, arguments, believed, digest)
      made(Act.call(acting.integrity), call) match {
        case Act.CallCame.Unsent(_, _, recorded)
            if !MovesJournal.sameCall(recorded, service, tool, arguments) =>
          Left(diverge(name))
        case Act.CallCame.Unsent(CallMade.Kind.Store, why, _) => Left(MoveError.Store(why))
        case Act.CallCame.Unsent(_, why, _) => Right(Called.Failed(why))
        case Act.CallCame.Sent(recorded, waited) =>
          if (!MovesJournal.sameCall(recorded, service, tool, arguments)) Left(diverge(name))
          else waited
      }
    }
  }

  /** `written`, a recorded reply, read by `reply` again: checked by its schema, with no repair
    * (they were made before it was recorded), and read.
    */
  private def readBack[T](reply: Typed[T], written: String): Either[MoveError, T] =
    scala.util
      .Try(ujson.read(written))
      .toEither
      .left
      .map(e => s"its recorded reply is not JSON: ${e.getMessage}")
      .flatMap(v => reply.schema.check(v, Set.empty).left.map(_.message))
      .flatMap(reply.read)
      .left
      .map(MoveError.Model(_))

  /** `written`, a judgment's recorded answers, read by `questions` again. */
  private def judgedBack[T](questions: Ask[?, T], written: String): Either[MoveError, T] =
    scala.util
      .Try(ujson.read(written))
      .toEither
      .left
      .map(e => s"its recorded answers are not JSON: ${e.getMessage}")
      .flatMap(_.arrOpt.toRight("its recorded answers are not an array"))
      .flatMap(_.toVector.foldLeft[Either[String, Vector[Answer]]](Right(Vector())) { (acc, a) =>
        acc.flatMap(done => AnswersJson.read(a).map(done :+ _))
      })
      .flatMap(questions.read(_).left.map(Act.unanswered))
      .left
      .map(MoveError.Model(_))

}
