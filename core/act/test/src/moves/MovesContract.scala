package grit.act.moves

import grit.core.act.{
  Asked,
  Called,
  MoveError,
  MoveKind,
  MoveLimits,
  MoveName,
  Moves,
  MovesFixtures,
  Posed
}
import grit.core.classify.{Answer as Judged, Ask, StateJson}
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.schema.JsonSchema
import grit.core.tool.ToolName
import grit.core.visibility.{Label, Level}

import utest.*

/** The rules every [[Moves]] keeps, run against [[DurableMoves]] and against the fake jobs'
  * tests use, `grit.core.act.ScriptedMoves`: a move's result as its world gives it; a name made
  * once, by either kind, is `Repeated` after, whatever the move came to; past its kind's limit a
  * move is `OverLimit`; a refused move is not made and counts against no limit; an ask whose
  * store failed was made, and counts; a JSON reply its schema refuses is asked for once more,
  * and a model's failure is `Model` in the same words.
  */
abstract class MovesContract extends TestSuite {
  import MovesContract.*

  /** What `use` returns, given moves within `limits` in a world where a text ask is answered
    * [[Answer]]; a JSON ask's model replies each of `shapes` in turn, across the run, the last
    * again once they run out (`{"answer": Answer}`, [[Shaped]], when there are none); a
    * judgment's classifier answers `judgment`; and a call of [[Tool]] at [[Probe]] is done with
    * [[Read]] at [[Floor]]; when `broken`, every ask's store fails instead.
    */
  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean = false,
      shapes: Vector[ujson.Value] = Vector(),
      judgment: Judged = Judged.YesNo(Yes)
  )(use: Moves^ -> A): A

  val tests = Tests {
    test(
      "an ask is answered as its world answers, and a call done with its edge's answer, each at the world's floor"
    ) {
      val got = within(limits(1, 1)) { m =>
        Seen(
          Vector(
            m.ask(name("a"), Posed.Text(Request)).map(a => s"${text(a)} at ${a.at}"),
            called(m, "c")
          )
        )
      }
      got ==> Seen(Vector(Right(s"$Answer at $Floor"), Right(Called.Done(Read, Floor).toString)))
    }

    test("a name made once, by either kind, is Repeated after, and the move is not made") {
      val got = within(limits(2, 2)) { m =>
        Seen(Vector(asked(m, "a"), called(m, "a"), asked(m, "a"), called(m, "b"), asked(m, "b")))
      }
      got ==> Seen(
        Vector(
          Right(Answer),
          Left(MoveError.Repeated(name("a"))),
          Left(MoveError.Repeated(name("a"))),
          Right(Called.Done(Read, Floor).toString),
          Left(MoveError.Repeated(name("b")))
        )
      )
    }

    test("an ask whose store failed spends its name") {
      val got = within(limits(2, 0), broken = true) { m =>
        Seen(Vector(asked(m, "a"), asked(m, "a")))
      }
      got.moves.map(_.left.map(kind)) ==> Vector(Left("Store"), Left("Repeated(a)"))
    }

    test("past its kind's limit a move is OverLimit; a refused move counts against no limit") {
      val got = within(limits(2, 1)) { m =>
        Seen(
          Vector(
            asked(m, "a"),
            asked(m, "a"),
            asked(m, "b"),
            asked(m, "c"),
            called(m, "d"),
            called(m, "e")
          )
        )
      }
      got ==> Seen(
        Vector(
          Right(Answer),
          Left(MoveError.Repeated(name("a"))),
          Right(Answer),
          Left(MoveError.OverLimit(MoveKind.Ask, 2)),
          Right(Called.Done(Read, Floor).toString),
          Left(MoveError.OverLimit(MoveKind.Call, 1))
        )
      )
    }

    test(
      "a JSON ask is answered as its world answers, counts against asks, and shares its names with text asks"
    ) {
      val got = within(limits(2, 0)) { m =>
        Seen(Vector(shaped(m, "a"), asked(m, "a"), asked(m, "b"), shaped(m, "b"), shaped(m, "c")))
      }
      got ==> Seen(
        Vector(
          Right(s"$Shaped at $Floor"),
          Left(MoveError.Repeated(name("a"))),
          Right(Answer),
          Left(MoveError.Repeated(name("b"))),
          Left(MoveError.OverLimit(MoveKind.Ask, 2))
        )
      )
    }

    test(
      "a judgment is judged as its world judges at the world's floor, counts against judgments while asks remain, and shares its names with asks"
    ) {
      val limited =
        MoveLimits.of(2, 0, 1).fold(e => throw new java.lang.AssertionError(e), identity)
      val got = within(limited) { m =>
        Seen(
          Vector(
            judged(m, "a"),
            asked(m, "a"),
            shaped(m, "b"),
            judged(m, "b"),
            judged(m, "c"),
            asked(m, "c")
          )
        )
      }
      got ==> Seen(
        Vector(
          Right(s"$Yes at $Floor"),
          Left(MoveError.Repeated(name("a"))),
          Right(s"$Shaped at $Floor"),
          Left(MoveError.Repeated(name("b"))),
          Left(MoveError.OverLimit(MoveKind.Judge, 1)),
          Right(Answer)
        )
      )
    }

    test("a judgment whose store failed spends its name and counts against judgments") {
      val limited =
        MoveLimits.of(0, 0, 1).fold(e => throw new java.lang.AssertionError(e), identity)
      val got = within(limited, broken = true) { m =>
        Seen(Vector(judged(m, "a"), judged(m, "a"), judged(m, "b")))
      }
      got.moves.map(_.left.map(kind)) ==>
        Vector(Left("Store"), Left("Repeated(a)"), Left("OverLimit(Judge,1)"))
    }

    test("a JSON reply its schema refuses is asked for once more, and read when that conforms") {
      val got = within(limits(1, 0), shapes = Vector(Unanswered, Answered)) { m =>
        Seen(Vector(shaped(m, "a")))
      }
      got ==> Seen(Vector(Right(s"$Shaped at $Floor")))
    }

    test("a JSON reply refused twice is Model, naming the last refusal, and spends its name") {
      // A third reply would conform: a second repair would read it.
      val got = within(limits(2, 0), shapes = Vector(Unanswered, Unanswered, Answered)) { m =>
        Seen(Vector(shaped(m, "a"), shaped(m, "a")))
      }
      got ==> Seen(
        Vector(
          Left(
            MoveError.Model(
              "its arguments do not match its schema: answer: expected a string, got 1"
            )
          ),
          Left(MoveError.Repeated(name("a")))
        )
      )
    }

    test("a judgment whose answers do not read is Model, in the classifier's words") {
      val choice = Judged.Choice("yes", Vector(Judged.Weight("yes", 1.0)), 1.0)
      val got = within(limits(1, 0), judgment = choice) { m => Seen(Vector(judged(m, "a"))) }
      got ==> Seen(
        Vector(
          Left(
            MoveError.Model(
              "the classifier's answers do not read: \"Is `text` urgent?\": answered a choice to a yes/no"
            )
          )
        )
      )
    }

    test("an ask whose store failed counts against its kind's limit") {
      val got = within(limits(1, 0), broken = true) { m =>
        Seen(Vector(asked(m, "a"), asked(m, "b")))
      }
      got.moves.map(_.left.map(kind)) ==> Vector(Left("Store"), Left("OverLimit(Ask,1)"))
    }

    test("under zero limits no move is made") {
      val got = within(MoveLimits.Zero) { m => Seen(Vector(asked(m, "a"), called(m, "a"))) }
      got ==> Seen(
        Vector(
          Left(MoveError.OverLimit(MoveKind.Ask, 0)),
          Left(MoveError.OverLimit(MoveKind.Call, 0))
        )
      )
    }
  }
}

object MovesContract {

  /** What a run's moves came to, each an ask's text or a call's result. */
  final case class Seen(moves: Vector[Either[MoveError, String]]) extends caps.Pure

  /** The ask `n` of [[Request]] through `m`: its text. */
  def asked(m: Moves^, n: String): Either[MoveError, String] =
    m.ask(name(n), Posed.Text(Request)).map(text)

  /** The JSON ask `n` of [[Request]]'s system text and messages, held to [[AnswerSchema]],
    * through `m`: its conforming JSON's text and the label it was read at.
    */
  def shaped(m: Moves^, n: String): Either[MoveError, String] =
    m.ask(name(n), Posed.json(Request.system, Request.messages, AnswerSchema))
      .map(a => s"${a.reply.text} at ${a.at}")

  /** The judgment `n`, whether [[State]] is urgent, through `m`: its probability of yes and the
    * label it was read at.
    */
  def judged(m: Moves^, n: String): Either[MoveError, String] = {
    given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))
    m.ask(name(n), Posed.judge(State, Ask.yesNo[String]("Is `text` urgent?", None, None)))
      .map(a => s"${a.reply} at ${a.at}")
  }

  /** The state every judgment is about. */
  val State = "the build is red"

  /** What a yes/no judgment is answered: its probability of yes. */
  val Yes: Double = MovesFixtures.Yes

  /** The call `n` of [[Tool]] at [[Probe]] through `m`: its result. */
  def called(m: Moves^, n: String): Either[MoveError, String] =
    m.call(name(n), Probe, Tool, Args).map(_.toString)

  /** What an ask is answered. */
  val Answer = "hello"

  /** What a JSON ask is answered, as its conforming JSON's text. */
  val Shaped = """{"answer":"hello"}"""

  /** [[Shaped]], as a model replies it. */
  val Answered: ujson.Value = ujson.Obj("answer" -> Answer)

  /** A reply [[AnswerSchema]] refuses. */
  val Unanswered: ujson.Value = ujson.Obj("answer" -> 1)

  /** `{"answer": string}`. */
  val AnswerSchema: JsonSchema = JsonSchema
    .read(
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("answer" -> ujson.Obj("type" -> "string")),
        "required" -> ujson.Arr("answer"),
        "additionalProperties" -> false
      )
    )
    .fold(e => throw new java.lang.AssertionError(e.message), identity)

  /** What a call of [[Tool]] at [[Probe]] is answered. */
  val Read = "read"

  /** The least label every transaction of the world is opened at: above public, so a move that
    * reports public whatever its floor is seen.
    */
  val Floor: Label = Label.at(Level.Internal)

  val Probe: Service =
    Service.of("probe").fold(e => throw new java.lang.AssertionError(e), identity)

  val Tool: ToolName = ToolName("probe_read")

  val Args: ujson.Obj = ujson.Obj("path" -> "a")

  val Request: ModelRequest = ModelRequest("Say hello.", Vector(Message.User("hi")))

  def name(n: String): MoveName =
    MoveName.of(n).fold(e => throw new java.lang.AssertionError(e), identity)

  def limits(asks: Int, calls: Int): MoveLimits =
    MoveLimits.of(asks, calls).fold(e => throw new java.lang.AssertionError(e), identity)

  def text(a: Asked[Message.Assistant]): String =
    a.reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  /** A move error's case name, and its name when it is `Repeated`. */
  def kind(e: MoveError): String = e match {
    case MoveError.Repeated(n) => s"Repeated(${MoveName.value(n)})"
    case MoveError.Store(_) => "Store"
    case other => other.toString
  }
}
