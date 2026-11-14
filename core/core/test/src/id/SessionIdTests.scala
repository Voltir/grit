package grit.core.id

import utest.*

/** [[SessionId.written]]: the one spelling of a session in text. */
object SessionIdTests extends TestSuite {
  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  val tests: Tests = Tests {
    test("a session is written as its harness, a colon, then its own id as sent") {
      Vector(
        got(Harness.of("made-up").flatMap(SessionId.of(_, "s-1/a-1"))),
        got(SessionId.of(Harness.Grit, "c:2"))
      ).map(_.written) ==> Vector("made-up:s-1/a-1", s"${Harness.value(Harness.Grit)}:c:2")
    }
  }
}
