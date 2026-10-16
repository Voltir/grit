package grit.core.edge

import grit.core.tool.Outcome
import grit.core.visibility.{Accounted, Integrity, Provenance, TestLabels}

import utest.*

/** [[OutcomeJson]]: an outcome's stored and wire form. */
object OutcomeJsonTests extends TestSuite {

  val tests = Tests {
    // Pins of the stored form: a request's row and a recorded wait for its answer hold it.
    test("each outcome is written in its stored form and read back") {
      val all = Vector(
        Outcome.Done("ok") -> """{"kind":"done","text":"ok"}""",
        Outcome.Done("ok", Some(Integrity.Member)) ->
          """{"kind":"done","text":"ok","integrity":"member"}""",
        Outcome.Failed("no") -> """{"kind":"failed","why":"no"}""",
        Outcome.Declined(Some("not now")) -> """{"kind":"denied","reason":"not now"}""",
        Outcome.Declined(None) -> """{"kind":"denied","reason":null}""",
        Outcome.Unanswered -> """{"kind":"unanswered"}""",
        Outcome.Interrupted -> """{"kind":"interrupted"}"""
      )
      all.map((o, _) => OutcomeJson.write(o).render()) ==> all.map(_._2)
      all.map((_, j) => OutcomeJson.read(ujson.read(j))) ==> all.map((o, _) => Right(o))
    }

    // The stored form of a hosted call's answer: rows and recorded steps hold it.
    test("an edge's items round-trip, and one with none keeps the form it had") {
      val repo = TestLabels.place("service:github/octo/grit")
      val all = Vector(
        Outcome.Done(
          "ok",
          None,
          Accounted.of(
            Vector(Provenance(repo, Integrity.Member), Provenance(repo, Integrity.Untrusted))
          )
        ) ->
          ("""{"kind":"done","text":"ok","items":[""" +
            """{"place":"service:github/octo/grit","author":"member"},""" +
            """{"place":"service:github/octo/grit","author":"untrusted"}]}"""),
        Outcome.Done("ok", None, Accounted.Unaccountable) ->
          """{"kind":"done","text":"ok","items":"unaccountable"}""",
        Outcome.Done("ok", None, Accounted.of(Vector.empty)) ->
          """{"kind":"done","text":"ok","items":[]}""",
        Outcome.Done("ok", None, Accounted.Unread) -> """{"kind":"done","text":"ok"}"""
      )
      all.map((o, _) => OutcomeJson.write(o).render()) ==> all.map(_._2)
      all.map((_, j) => OutcomeJson.read(ujson.read(j))) ==> all.map((o, _) => Right(o))
    }
  }
}
