package grit.core.triage

import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

/** [[GateJson]]'s fact reading: a recorded form, read back by every later build. */
object GateJsonTests extends TestSuite {

  private val open = Reading.Yes(QuestionName.of("open").getOrElse(sys.error("a name")))

  val tests = Tests {
    test("a fact reading round-trips; an old reading reads as before") {
      val facts = Vector(Fact.Named, Fact.SettledOnPerson, Fact.RecordScore).map(Reading.Fact(_))
      val failed = Gate.Failed(
        Bound.AtLeast(Reading.Fact(Fact.RecordScore), Probability.clamped(0.5)),
        Probability.clamped(0.25)
      )
      (
        facts.map(GateJson.writeReading(_).render()),
        facts.map(r => GateJson.readReading(GateJson.writeReading(r))),
        GateJson.readFailed(GateJson.writeFailed(failed)),
        GateJson.readReading(ujson.read("""{"reads":"yes","name":"open"}""")),
        GateJson.readReading(ujson.read("""{"reads":"fact","fact":"guessed"}"""))
      ) ==> (
        Vector(
          """{"reads":"fact","fact":"named"}""",
          """{"reads":"fact","fact":"settled_on_person"}""",
          """{"reads":"fact","fact":"record_score"}"""
        ),
        facts.map(Right(_)),
        Right(failed),
        Right(open),
        Left("reading: unknown fact guessed")
      )
    }
  }
}
