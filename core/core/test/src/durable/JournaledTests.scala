package grit.core.durable

import utest.*

/** The step outcome's journaled form, which recorded histories hold and must read back. */
object JournaledTests extends TestSuite {

  private val flag: Journaled[Either[String, Boolean]] =
    Journaled.outcome(ujson.Bool(_), _.boolOpt.toRight("expected a boolean"))

  val tests = Tests {
    test("an outcome is written as ok with its value, or failed with its reason, and read back") {
      val written = Vector(Right(true), Left("store down")).map(flag.encode)
      (written, written.map(flag.decode)) ==> (
        Vector("""{"ok":true}""", """{"failed":"store down"}"""),
        Vector(Right(Right(true)), Right(Left("store down")))
      )
    }

    test("an outcome holding both, neither, a value its reader refuses, or no object fails") {
      Vector("""{"ok":true,"failed":"x"}""", "{}", """{"ok":3}""", "true").map(flag.decode) ==>
        Vector(
          Left("expected {ok} or {failed}"),
          Left("expected {ok} or {failed}"),
          Left("expected a boolean"),
          Left("expected an object")
        )
    }

    test("a recorded flag is written as ok with its boolean, and a non-boolean refuses to read") {
      // A pin of the recorded form: the lifecycle journals hold `{"ok":true}` in their histories.
      val written = Vector(true, false).map(b => Journaled.flag.encode(Right(b)))
      (written, Journaled.flag.decode("""{"ok":3}""")) ==> (
        Vector("""{"ok":true}""", """{"ok":false}"""),
        Left("recorded: expected a boolean")
      )
    }
  }
}
