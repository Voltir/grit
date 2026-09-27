package grit.core.edge

import grit.core.tool.Outcome

import utest.*

/** [[OutcomeJson]]: an outcome's stored and wire form. */
object OutcomeJsonTests extends TestSuite {

  val tests = Tests {
    // Pins of the stored form: a request's row and a recorded wait for its answer hold it.
    test("each outcome is written in its stored form and read back") {
      val all = Vector(
        Outcome.Done("ok") -> """{"kind":"done","text":"ok"}""",
        Outcome.Failed("no") -> """{"kind":"failed","why":"no"}""",
        Outcome.Declined(Some("not now")) -> """{"kind":"denied","reason":"not now"}""",
        Outcome.Declined(None) -> """{"kind":"denied","reason":null}""",
        Outcome.Unanswered -> """{"kind":"unanswered"}""",
        Outcome.Interrupted -> """{"kind":"interrupted"}"""
      )
      all.map((o, _) => OutcomeJson.write(o).render()) ==> all.map(_._2)
      all.map((_, j) => OutcomeJson.read(ujson.read(j))) ==> all.map((o, _) => Right(o))
    }
  }
}
