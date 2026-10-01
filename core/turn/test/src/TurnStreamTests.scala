package grit.turn

import grit.core.durable.StreamWriter
import grit.core.provider.Delta

import utest.*

/** The reply's stream: its pieces' form, how the writer gathers deltas, and what a reader
  * keeps across attempts.
  */
object TurnStreamTests extends TestSuite {
  import TurnStream.*

  /** A stream that keeps what is written. */
  final class Kept extends StreamWriter {
    @caps.unsafe.untrackedCaptures
    var pieces: Vector[String] = Vector.empty
    def write(piece: String): Unit = pieces = pieces :+ piece
  }

  /** A clock that reads `at`, moved by the test. */
  final class Clock {
    @caps.unsafe.untrackedCaptures
    var at = 0L
  }

  val tests = Tests {
    test("a piece reads back as written; anything else is not a piece") {
      val pieces = Vector(Piece("a", Delta.Text("Fe\"hu")), Piece("b", Delta.Reasoning("…")))
      pieces.map(p => decode(encode(p))) ==> pieces.map(Right(_))
      assert(decode("nope").isLeft, decode("""{"attempt":"a","kind":"x","text":"t"}""").isLeft)
    }

    test("the writer gathers a kind until it changes, grows long, or waits too long") {
      val out = new Kept
      val clock = new Clock
      val w = new Writer(out, "a", () => clock.at)
      w.tell(Delta.Reasoning("th"))
      w.tell(Delta.Reasoning("ink"))
      out.pieces ==> Vector.empty
      w.tell(Delta.Text("Fe"))
      out.pieces.map(decode) ==> Vector(Right(Piece("a", Delta.Reasoning("think"))))
      clock.at = MaxMs
      w.tell(Delta.Text("hu"))
      out.pieces.map(decode).lastOption ==> Some(Right(Piece("a", Delta.Text("Fehu"))))
      w.tell(Delta.Text("x" * MaxChars))
      out.pieces.size ==> 3
      w.tell(Delta.Text("end"))
      w.flush()
      out.pieces.map(decode).lastOption ==> Some(Right(Piece("a", Delta.Text("end"))))
    }

    test("a call is written at once, after what was gathered before it, and read back") {
      val out = new Kept
      val w = new Writer(out, "a", () => 0L)
      w.tell(Delta.Text("Let me look."))
      w.tell(Delta.Calling("read"))
      w.tell(Delta.Calling("list"))
      out.pieces.map(decode) ==> Vector(
        Right(Piece("a", Delta.Text("Let me look."))),
        Right(Piece("a", Delta.Calling("read"))),
        Right(Piece("a", Delta.Calling("list")))
      )
      val heard = Vector(
        Piece("a", Delta.Calling("stale")),
        Piece("b", Delta.Text("Let me look.")),
        Piece("b", Delta.Calling("read"))
      ).foldLeft(Heard.nothing)(_ + _)
      heard ==> Heard(Some("b"), "", "Let me look.", Vector("read"))
    }

    test("a reader keeps the latest attempt: a new one starts over") {
      val heard = Vector(
        Piece("a", Delta.Reasoning("hm")),
        Piece("a", Delta.Text("The El")),
        Piece("b", Delta.Text("The Elder")),
        Piece("b", Delta.Text(" Futhark"))
      ).foldLeft(Heard.nothing)(_ + _)
      heard ==> Heard(Some("b"), "", "The Elder Futhark", Vector())
    }
  }
}
