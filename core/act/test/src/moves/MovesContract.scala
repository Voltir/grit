package grit.act.moves

import grit.core.act.{Asked, Called, MoveError, MoveKind, MoveLimits, MoveName, Moves}
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.tool.ToolName
import grit.core.visibility.{Label, Level}

import utest.*

/** The rules every [[Moves]] keeps, run against [[DurableMoves]] and against the fake jobs'
  * tests use, `grit.core.act.ScriptedMoves`: a move's result as its world gives it; a name made
  * once, by either kind, is `Repeated` after, whatever the move came to; past its kind's limit a
  * move is `OverLimit`; a refused move is not made and counts against no limit.
  */
abstract class MovesContract extends TestSuite {
  import MovesContract.*

  /** What `use` returns, given moves within `limits` in a world where an ask is answered
    * [[Answer]] and a call of [[Tool]] at [[Probe]] is done with [[Read]] at [[Floor]]; when
    * `broken`, every ask's store fails instead.
    */
  def within[A <: caps.Pure](limits: MoveLimits, broken: Boolean = false)(use: Moves^ -> A): A

  val tests = Tests {
    test(
      "an ask is answered as its world answers, and a call done with its edge's answer, each at the world's floor"
    ) {
      val got = within(limits(1, 1)) { m =>
        Seen(Vector(m.ask(name("a"), Request).map(a => s"${text(a)} at ${a.at}"), called(m, "c")))
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
      val got = within(limits(1, 1)) { m =>
        Seen(Vector(asked(m, "a"), asked(m, "a"), asked(m, "b"), called(m, "c"), called(m, "d")))
      }
      got ==> Seen(
        Vector(
          Right(Answer),
          Left(MoveError.Repeated(name("a"))),
          Left(MoveError.OverLimit(MoveKind.Ask, 1)),
          Right(Called.Done(Read, Floor).toString),
          Left(MoveError.OverLimit(MoveKind.Call, 1))
        )
      )
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
  def asked(m: Moves^, n: String): Either[MoveError, String] = m.ask(name(n), Request).map(text)

  /** The call `n` of [[Tool]] at [[Probe]] through `m`: its result. */
  def called(m: Moves^, n: String): Either[MoveError, String] =
    m.call(name(n), Probe, Tool, Args).map(_.toString)

  /** What an ask is answered. */
  val Answer = "hello"

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

  def text(a: Asked): String =
    a.message.blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  /** A move error's case name, and its name when it is `Repeated`. */
  def kind(e: MoveError): String = e match {
    case MoveError.Repeated(n) => s"Repeated(${MoveName.value(n)})"
    case MoveError.Store(_) => "Store"
    case other => other.toString
  }
}
