package grit.core.triage

import grit.core.id.{EntryId, PluginName}
import grit.core.period.{Judgement, Probability}

import utest.*

/** [[CuesJson]]: the form a decision's cues are kept in, read back by every later build. */
object CuesJsonTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)

  private val notes = PluginName.of("notes").getOrElse(sys.error("a name"))

  val tests = Tests {
    test(
      "cues round-trip, an unanswered verdict with its words; a row kept before cues reads as Empty"
    ) {
      val weighed = Cues(
        named = true,
        spoken = Some(EntryId("e-reply")),
        continued = Some(EntryId("e-next")),
        settled = Some(Judgement.Weighed(p(0.125), p(0.75), p(0.125), "jev-1")),
        records = Some(Cues.Match(notes, 2.5, 3))
      )
      val unanswered = Cues.Empty.copy(settled = Some(Judgement.Unanswered("settle model down")))
      (
        CuesJson.write(weighed).render(),
        CuesJson.write(unanswered).render(),
        Vector(weighed, unanswered, Cues.Empty).map(c => CuesJson.read(CuesJson.write(c))),
        CuesJson.read(ujson.Null),
        CuesJson.read(ujson.Str("named"))
      ) ==> (
        """{"named":true,"spoken":"e-reply","continued":"e-next",""" +
          """"settled":{"nobody":0.125,"on_person":0.75,"on_other":0.125,"model":"jev-1"},""" +
          """"records":{"plugin":"notes","score":2.5,"matched":3}}""",
        """{"named":false,"settled":{"unanswered":"settle model down"}}""",
        Vector(Right(weighed), Right(unanswered), Right(Cues.Empty)),
        Right(Cues.Empty),
        Left("cues: expected an object")
      )
    }
  }
}
