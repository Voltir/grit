package grit.core.act

import grit.core.classify.{Answer, ClassifierError, Request}
import grit.core.flow.{Endorse, Extracted, Shape, Tainted, TestTainted, Trusted}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** [[Moves]] for a job's tests: each text ask answered by `asks`; each JSON ask by `jsons`
  * (of its system text and messages), the reply then checked by its schema, with no argument
  * repair, and read, and when it does not read, `jsons` asked again up to
  * [[Posed.Json.Repairs]] times, as the durable moves ask a model again (`jsons` is given the
  * same system text and messages each time, not the refusal the model is told); each judgment
  * by `judgments` (of what the classifier would be sent), the answers then read by its
  * questions; each call by `calls`. A reply is believed as what it was posed from is, and a
  * call's answer at the meet of what `calls` gives it and its arguments. A reply or answers
  * that do not read are
  * [[MoveError.Model]] in the durable moves' words. Under the rules every [[Moves]] keeps,
  * which `grit.act.moves.MovesContract` holds it to beside the durable moves: a name made once,
  * by either kind, is [[MoveError.Repeated]] after, whatever the move came to; past `limits`, a
  * move is [[MoveError.OverLimit]]; a refused move is not made. It never diverges: it is one
  * run, never rerun. An extraction is a JSON ask of [[Extraction]], its answer trusted at
  * `integrity`, the run's, met with `Member`. [[made]] is each move made, in order.
  */
class ScriptedMoves(
    limits: MoveLimits,
    asks: ModelRequest -> Either[MoveError, Asked[Message.Assistant]],
    calls: (Service, ToolName, ujson.Obj) -> Either[MoveError, Called],
    jsons: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]] = (_, _) =>
      Left(MoveError.Model("no JSON reply scripted")),
    judgments: Request -> Either[MoveError, Asked[Vector[Answer]]] = _ =>
      Left(MoveError.Model("no judgment scripted")),
    integrity: Integrity = Integrity.Authority
) extends Moves {

  // Holds immutable values; the fake is one test's, read on its one thread.
  @caps.unsafe.untrackedCaptures
  var made: Vector[MoveName] = Vector.empty

  def ask[R](name: MoveName, posed: Tainted[Posed[R]]): Either[MoveError, Asked[R]] =
    unbelieved(name, Trusted.asIs(posed).extract).map(a =>
      Asked(TestTainted.at(Trusted.asIs(a.reply).extract, posed.integrity), a.at)
    )

  /** The ask `name` of `posed`, its reply as the scripts believe it. */
  private def unbelieved[R](name: MoveName, posed: Posed[R]): Either[MoveError, Asked[R]] =
    posed match {
      case Posed.Text(request) => making(name, MoveKind.Ask, limits.asks)(asks(request))
      case Posed.Json(system, messages, reply) =>
        def shaped(left: Int): Either[MoveError, Asked[R]] =
          jsons(system, messages).flatMap { a =>
            reply.schema
              .check(Trusted.asIs(a.reply).extract, Set.empty)
              .left
              .map(m => s"its arguments do not match its schema: ${m.message}")
              .flatMap(reply.read(_).left.map(why => s"its arguments do not read: $why"))
              .map(r => Asked(TestTainted.at(r, a.reply.integrity), a.at)) match {
              case Left(_) if left > 0 => shaped(left - 1)
              case read => read.left.map(MoveError.Model(_))
            }
          }
        making(name, MoveKind.Ask, limits.asks)(shaped(Posed.Json.Repairs))
      case Posed.Judgment(request, questions) =>
        making(name, MoveKind.Judge, limits.judgments)(judgments(request).flatMap { a =>
          questions
            .read(Trusted.asIs(a.reply).extract)
            .left
            .map(e => MoveError.Model(unanswered(e)))
            .map(r => Asked(TestTainted.at(r, a.reply.integrity), a.at))
        })
    }

  def call(
      name: MoveName,
      service: Service,
      tool: ToolName,
      arguments: Tainted[ujson.Obj]
  ): Either[MoveError, Called] =
    making(name, MoveKind.Call, limits.calls)(
      calls(service, tool, Trusted.asIs(arguments).extract).map {
        case Called.Done(text, at) =>
          Called.Done(
            TestTainted.at(Trusted.asIs(text).extract, text.integrity.meet(arguments.integrity)),
            at
          )
        case other => other
      }
    )

  def extract(
      name: MoveName,
      from: Tainted[String],
      question: String,
      shape: Shape
  ): Either[MoveError, Option[Trusted[Extracted]]] =
    Extraction.reply(shape) match {
      case Left(why) => Left(MoveError.Model(why))
      case Right(typed) =>
        given (Endorse^) = TestTainted.extraction(integrity)
        val posed = from.map(t =>
          Posed.Json(Extraction.System, Extraction.messages(t, question, shape), typed)
        )
        ask(name, posed).map(a =>
          Trusted.asIs(a.reply).extract.map(x => Trusted.endorse(a.reply.map(_ => x)))
        )
    }

  protected def sanitiser: Endorse^ = TestTainted.sanitiser

  /** Why a classifier's answers did not read, in the durable moves' words. */
  private def unanswered(error: ClassifierError): String = error match {
    case ClassifierError.Unavailable(cause) => s"the classifier is unavailable: $cause"
    case ClassifierError.Unreadable(why) => s"the classifier's answers do not read: $why"
  }

  protected def making[A](name: MoveName, kind: MoveKind, limit: Int)(
      answer: => Either[MoveError, A]
  ): Either[MoveError, A] = {
    val count = made.count(n => kinds.get(n).contains(kind))
    if (made.contains(name)) Left(MoveError.Repeated(name))
    else if (count >= limit) Left(MoveError.OverLimit(kind, limit))
    else {
      made = made :+ name
      kinds = kinds.updated(name, kind)
      answer
    }
  }

  // As `made`: replaced whole by the test's own calls, on its one thread; nothing else holds it.
  @caps.unsafe.untrackedCaptures
  private var kinds: Map[MoveName, MoveKind] = Map.empty
}
