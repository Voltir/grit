package grit.core.edge

import grit.core.identity.Email
import grit.core.tool.Outcome
import grit.core.visibility.{Accounted, Integrity, Part, Provenance, Readers, TestLabels}

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

    // A recorded step must never hold what only some readers may read: the journals record
    // `write`'s form, and only a request's row holds `answer`'s.
    test("a done with parts is recorded without them; its answer form keeps them, and reads back") {
      val folder = TestLabels.place("service:files/confidential")
      val ana =
        Email.of("ana@example.com").fold(e => throw new java.lang.AssertionError(e), identity)
      val cut = Outcome.Done(
        "2 files",
        None,
        Accounted.of(Vector.empty),
        Some(
          Vector(
            Part(Provenance(folder, Integrity.Member), Readers.AtPlace, "plan.md"),
            Part(Provenance(folder, Integrity.Untrusted), Readers.Originator(ana), "notes.md")
          )
        )
      )
      val answered =
        """{"kind":"done","text":"2 files","items":[],"parts":[""" +
          """{"place":"service:files/confidential","author":"member","readers":"place","text":"plan.md"},""" +
          """{"place":"service:files/confidential","author":"untrusted",""" +
          """"readers":{"originator":"ana@example.com"},"text":"notes.md"}]}"""
      (
        OutcomeJson.write(cut).render(),
        OutcomeJson.answer(cut).render(),
        OutcomeJson.read(ujson.read(answered))
      ) ==> (
        """{"kind":"done","text":"2 files","items":[]}""",
        answered,
        Right(cut)
      )
    }
  }
}
