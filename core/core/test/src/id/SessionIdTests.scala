package grit.core.id

import grit.core.either.Got.got

import utest.*

/** [[SessionId.written]]: the one spelling of a session in text, and [[SessionId.read]], its
  * reading.
  */
object SessionIdTests extends TestSuite {

  val tests: Tests = Tests {
    test("a session is written as its harness, a colon, then its own id as sent") {
      Vector(
        got(Harness.of("made-up").flatMap(SessionId.of(_, "s-1/a-1"))),
        got(SessionId.of(Harness.Grit, "c:2"))
      ).map(_.written) ==> Vector("made-up:s-1/a-1", s"${Harness.value(Harness.Grit)}:c:2")
    }

    test("a session written is read back as it was, its own id holding colons or not") {
      val ids = Vector(
        got(Harness.of("made-up").flatMap(SessionId.of(_, "s-1/a-1"))),
        got(SessionId.of(Harness.Grit, "c:2"))
      )
      ids.map(id => SessionId.read(id.written)) ==> ids.map(Right(_))
    }
  }
}
