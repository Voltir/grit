package grit.act.moves

import java.time.Instant

import grit.core.act.{
  Acting,
  Asked,
  Called,
  Dreaming,
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
import grit.core.document.{DocumentKeeper, DocumentSearch}
import grit.core.durable.{Flow, Journaled, Steps}
import grit.core.flow.{Extracted, Mint, Shape, Tainted, Trusted}
import grit.core.id.{CallSlot, DocumentVersion, PluginName}
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.sanitise.IntegrityTree
import grit.core.schema.Typed
import grit.core.stitch.{Strand, StrandExcerpts, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** A planner's moves as durable steps ([[Act]]'s), over `grit.act.phase`'s `Asking` and
  * `Calling`.
  */
object DurableMoves {

  /** The downgrades a run's moves make, for its program to declare ([[Flow.program]]'s
    * `within`): an extraction's ([[Extraction.Node]]) and the integrity tree's
    * ([[IntegrityTree.Node]], without which a vet is [[MoveError.Undeclared]]).
    */
  val Within: Vector[Flow.Node] = Vector(Extraction.Node, IntegrityTree.Node)

  /** What `body` returns, given the moves of `acting` within `limits`, made through `steps`,
    * after which none of them may be made: each recorded as `ask:{name}`, `call:{name}` or
    * `keep:{name}`, and its phases as an ask's `ask:{name}:record` and a call's
    * `call:{name}:expire`, `:abandon` and `:answer` ([[ActName]]). A run's call
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

  /** As [[keeping]], `plugin`'s, its moves also reading `strands` and `needs` ([[Dreaming]]),
    * a summary reading its strands' messages through `excerpts` and its documents through
    * `documents` (`plugin`'s own and its needs' alone) in its own step: recorded as an
    * `ask:{name}` in a JSON ask's form, under its own digest ([[MovesJournal.strands]]).
    */
  def dreaming[A <: caps.Pure](
      acting: Acting,
      limits: MoveLimits,
      plugin: PluginName,
      keeper: DocumentKeeper,
      strands: StrandShelf,
      needs: Needs,
      excerpts: StrandExcerpts,
      documents: DocumentSearch
  )(body: Dreaming^ => A)(using steps: Steps[MovesEnv]^): A =
    body(new Dreamt(acting, limits, plugin, keeper, strands, needs, excerpts, documents))

  /** One run's moves, its keeps over `keeper`, its scans over `strands` and `needs`, and its
    * summaries over `excerpts` and the documents of `plugin` and its needs in `documents`.
    */
  private final class Dreamt(
      acting: Acting,
      limits: MoveLimits,
      plugin: PluginName,
      keeper: DocumentKeeper,
      strands: StrandShelf,
      needs: Needs,
      excerpts: StrandExcerpts,
      documents: DocumentSearch
  )(using steps: Steps[MovesEnv]^)
      extends Kept(acting, limits, keeper),
        Dreaming {

    def scan[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
        body: (I, DocumentKeeper, StrandShelf, Needs, Instant) -> Tx^ ?-> Either[StoreError, A]
    ): Either[MoveError, A] =
      keep(name, from)((i, kept, at) => body(i, kept, strands, needs, at))

    def summarise[T](
        name: MoveName,
        seen: Vector[Strand.Digested],
        shown: Vector[DocumentVersion],
        summary: Tainted[Dreaming.Summary[T]]
    ): Either[MoveError, Option[Dreaming.Summarised[T]]] =
      refused(name, MoveKind.Ask).toLeft(()).flatMap { _ =>
        val Dreaming.Summary(system, chars, documentChars, reply) = Trusted.asIs(summary).extract
        val digest =
          MovesJournal.strands(seen, shown, system, chars, documentChars, reply.schema)
        val readable = plugin +: needs.plugins
        val asked = made(
          Act.ask(Act.summary[T]),
          Act.Summary(
            name,
            acting,
            seen,
            shown,
            system,
            chars,
            documentChars,
            reply,
            excerpts,
            documents,
            readable,
            digest
          )
        )
        val same = MovesJournal.sameStrands(
          asked.made.digest,
          seen,
          shown,
          system,
          chars,
          documentChars,
          reply.schema
        )
        asked.made match {
          case AskMade.Refused(AskMade.Kind.Changed, _, _, _) if same => Right(None)
          case _ =>
            settled(name, asked, same) {
              case AskMade.Shaped(_, written, at, level, _, _) =>
                Some(
                  readBack(reply, written)
                    .map(r => Asked(Mint.tainted(r, summary.integrity.meet(level)), at))
                )
              case _ => None
            }.map(a =>
              Some(
                Dreaming.Summarised(
                  a,
                  asked.made match {
                    case AskMade.Shaped(_, _, _, _, _, versions) => versions
                    case _ => Vector.empty
                  }
                )
              )
            )
        }
      }
  }

  /** One run's moves, and its keeps over `keeper`. */
  private class Kept(acting: Acting, limits: MoveLimits, keeper: DocumentKeeper)(using
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

    // caps.unsafe: each holds an immutable value. The object is made inside `plain`,
    // `keeping` or `dreaming`, per run, and never leaves its `body` (whose result is pure, so it cannot carry
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

    protected def tree: Option[IntegrityTree.Raising^] = IntegrityTree.Node.raise

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
      shaped(name, system, messages, reply, believed)(Act.ask(Act.json[T]).map(s => (s, ())))
        .map(_._1)

    /** As [[json]], its steps `flow`, with what `flow` passed beside its reply. */
    private def shaped[T, X](
        name: MoveName,
        system: String,
        messages: Vector[Message],
        reply: Typed[T],
        believed: Integrity
    )(
        flow: Flow[MovesEnv, Act.Json[T], Nothing, (Act.Settling, X)]
    ): Either[MoveError, (Asked[T], X)] =
      refused(name, MoveKind.Ask).toLeft(()).flatMap { _ =>
        val digest = MovesJournal.json(system, messages, reply.schema)
        val (asked, x) = made(flow, Act.Json(name, acting, system, messages, reply, digest))
        val same = MovesJournal.sameJson(asked.made.digest, system, messages, reply.schema)
        settled(name, asked, same) {
          case AskMade.Shaped(_, written, at, _, _, _) =>
            Some(readBack(reply, written).map(r => Asked(Mint.tainted(r, believed), at)))
          case _ => None
        }.map((_, x))
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
    protected def settled[R](name: MoveName, asked: Act.Settling, same: Boolean)(
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
          val messages = Extraction.messages(Trusted.asIs(from).extract, question, shape)
          type Said = Option[Trusted[Option[Extracted]]]
          val flow: Flow[MovesEnv, Act.Json[Option[Extracted]], Nothing, (Act.Settling, Said)] =
            Act.raised(Act.json[Option[Extracted]], Extraction.Node) {
              case AskMade.Shaped(_, written, _, level, _, _) =>
                readBack(typed, written).toOption.map(a => (answer = a, level = level))
              case _ => None
            }
          shaped(name, Extraction.System, messages, typed, from.integrity)(flow).map((_, said) =>
            said.flatMap(t => t.extract.map(x => t.map(_ => x)))
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
