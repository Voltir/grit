package grit.core.act

import grit.core.classify.{Answer, ClassifierError, Request}
import grit.core.flow.{Extracted, Shape, Tainted, TestTainted, Trusted}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.sanitise.IntegrityTree
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label}

import ScriptedMoves.Replied

class ScriptedMoves(
    limits: MoveLimits,
    asks: ModelRequest -> Either[MoveError, Replied[Message.Assistant]],
    calls: (Service, ToolName, ujson.Obj) -> Either[MoveError, Called],
    jsons: (String, Vector[Message]) -> Either[MoveError, Replied[ujson.Value]] = (_, _) =>
      Left(MoveError.Model("no JSON reply scripted")),
    judgments: Request -> Either[MoveError, Replied[Vector[Answer]]] = _ =>
      Left(MoveError.Model("no judgment scripted")),
    integrity: Integrity = Integrity.Authority
) extends Moves {

  // Holds immutable values; the fake is one test's, read on its one thread.
  @caps.unsafe.untrackedCaptures
  var made: Vector[MoveName] = Vector.empty

  def ask[R](name: MoveName, posed: Tainted[Posed[R]]): Either[MoveError, Asked[R]] =
    unbelieved(name, Trusted.asIs(posed).extract).map(a =>
      Asked(TestTainted.at(a.reply, posed.integrity), a.at)
    )

  /** The ask `name` of `posed`, its reply as the scripts give it. */
  private def unbelieved[R](name: MoveName, posed: Posed[R]): Either[MoveError, Replied[R]] =
    posed match {
      case Posed.Text(request) => making(name, MoveKind.Ask, limits.asks)(asks(request))
      case Posed.Json(system, messages, reply) =>
        def shaped(left: Int): Either[MoveError, Replied[R]] =
          jsons(system, messages).flatMap { a =>
            reply.schema
              .check(a.reply, Set.empty)
              .left
              .map(m => s"its arguments do not match its schema: ${m.message}")
              .flatMap(reply.read(_).left.map(why => s"its arguments do not read: $why"))
              .map(r => Replied(r, a.at)) match {
              case Left(_) if left > 0 => shaped(left - 1)
              case read => read.left.map(MoveError.Model(_))
            }
          }
        making(name, MoveKind.Ask, limits.asks)(shaped(Posed.Json.Repairs))
      case Posed.Judgment(request, questions) =>
        making(name, MoveKind.Judge, limits.judgments)(judgments(request).flatMap { a =>
          questions
            .read(a.reply)
            .left
            .map(e => MoveError.Model(ScriptedMoves.unanswered(e)))
            .map(r => Replied(r, a.at))
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
        case Called.Done(answer, at) =>
          Called.Done(
            TestTainted.at(
              Trusted.asIs(answer).extract,
              answer.integrity.meet(arguments.integrity)
            ),
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
        val posed = from.map(t =>
          Posed.Json(Extraction.System, Extraction.messages(t, question, shape), typed)
        )
        ask(name, posed).map { a =>
          val said = TestTainted.raise(Extraction.Node)(
            (answer = Trusted.asIs(a.reply).extract, level = integrity)
          )
          said.extract.map(x => said.map(_ => x))
        }
    }

  protected def tree: Option[IntegrityTree.Raising^] = Some(TestTainted.raise(IntegrityTree.Node))

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

object ScriptedMoves {

  /** What a script answers an ask with: its reply and the label it is at. The reply is believed
    * as what it was posed from is, as every ask's is ([[Moves.ask]]).
    */
  final case class Replied[R](reply: R, at: Label)

  /** Why a classifier gave no answer that reads, in the durable moves' words: a `judgments`
    * script failing as a classifier would gives `MoveError.Model` of this; `MovesContract`
    * pins both wordings.
    */
  def unanswered(error: ClassifierError): String = error match {
    case ClassifierError.Unavailable(cause) => s"the classifier is unavailable: $cause"
    case ClassifierError.Refused(cause) => s"the classifier refused the request: $cause"
    case ClassifierError.Unreadable(why) => s"the classifier's answers do not read: $why"
  }
}
