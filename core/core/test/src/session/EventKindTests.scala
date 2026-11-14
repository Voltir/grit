package grit.core.session

import utest.*

/** [[Event.Kind.named]]: a kind read back by its stored name. */
object EventKindTests extends TestSuite {
  private val Said: Session.Summary =
    Session.Summary.of("done").fold(why => throw new java.lang.AssertionError(why), s => s)

  private val Every: Vector[Event.Kind] = Vector(
    Event.Kind.Started,
    Event.Kind.Read(3),
    Event.Kind.Edited,
    Event.Kind.Ran,
    Event.Kind.Commit,
    Event.Kind.Closing(Said),
    Event.Kind.Ended
  )

  val tests: Tests = Tests {
    test("every kind is read back by its name, and Names lists each name in order") {
      (
        Every.map(k => Event.Kind.named[String](k.name, Right(3L), Right(Said))),
        Event.Kind.Names
      ) ==> (Every.map(k => Some(Right(k))), Every.map(_.name))
    }

    test("only a read asks for its bytes and only a closing for its summary; no other name reads") {
      Vector("edited", "read", "closing", "Edited").map(
        Event.Kind.named[String](_, Left("no bytes"), Left("no summary"))
      ) ==> Vector(
        Some(Right(Event.Kind.Edited)),
        Some(Left("no bytes")),
        Some(Left("no summary")),
        None
      )
    }
  }
}
