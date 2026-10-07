package grit.core.triage

import scala.collection.immutable.{ListSet, VectorMap}

import grit.core.classify.Answer
import grit.core.id.{CorpusName, QuestionName}
import grit.core.period.Probability

import utest.*

/** v2's, v3's and v4's named parts, one test each: the one reading it reads, and its edge at `at`. Every
  * part is tested at 0.3, not at one half, so a part that ignores `at` fails. Thresholds are
  * tested here and nowhere else.
  */
object PartsTests extends TestSuite {

  private def p(x: Double): Probability = Probability.clamped(x)

  private val at = p(0.3)
  private val under = 0.29

  private def yes(name: QuestionName, v: Double) = VectorMap[QuestionName, Answer](
    name -> Answer.YesNo(v)
  )

  /** `gate` read at `at` and just under it, as `(reads, at, under)`. */
  private def edge(gate: Gate, answers: Double => VectorMap[QuestionName, Answer]) =
    (gate.reads, gate.check(answers(0.3)), gate.check(answers(under)))

  private def failed(bound: Bound, read: Double) =
    Gate.Checked.Fails(Gate.Failed(bound, p(read)), Vector.empty)

  val tests = Tests {
    test("asks reads gap's weight on asks, passing at at least at") {
      val reading = Reading.Key(Tags.V2.gap, "asks")
      edge(
        Tags.V2.asks(at),
        v =>
          VectorMap(
            Tags.V2.gap -> Answer.Choice(
              "nothing",
              Vector(Answer.Weight("asks", v), Answer.Weight("nothing", 1 - v)),
              0.0
            )
          )
      ) ==> (ListSet(reading), Gate.Checked.Passes, failed(Bound.AtLeast(reading, at), under))
    }

    test("stillOpen reads open's yes, passing at at least at") {
      val reading = Reading.Yes(Tags.V2.open)
      edge(Tags.V2.stillOpen(at), yes(Tags.V2.open, _)) ==>
        (ListSet(reading), Gate.Checked.Passes, failed(Bound.AtLeast(reading, at), under))
    }

    test("notToSomeone reads to's yes, passing only below at") {
      val reading = Reading.Yes(Tags.V2.to)
      edge(Tags.V2.notToSomeone(at), yes(Tags.V2.to, _)) ==>
        (ListSet(reading), failed(Bound.Below(reading, at), 0.3), Gate.Checked.Passes)
    }

    test("notAnchored reads anchor's yes, passing only below at") {
      val reading = Reading.Yes(Tags.V2.anchor)
      edge(Tags.V2.notAnchored(at), yes(Tags.V2.anchor, _)) ==>
        (ListSet(reading), failed(Bound.Below(reading, at), 0.3), Gate.Checked.Passes)
    }

    test("source reads source:<name>'s yes, passing at at least at") {
      val github = CorpusName.of("github").getOrElse(sys.error("a name"))
      val name = QuestionName.read("source:github").getOrElse(sys.error("a name"))
      val reading = Reading.Yes(name)
      edge(Tags.V2.source(github, at), yes(name, _)) ==>
        (ListSet(reading), Gate.Checked.Passes, failed(Bound.AtLeast(reading, at), under))
    }

    test("atGrit reads to-grit's yes, passing at at least at") {
      // A pin of a recorded name: v3's answers are stored under it.
      QuestionName.value(Tags.V3.toGrit) ==> "to-grit"
      val reading = Reading.Yes(Tags.V3.toGrit)
      edge(Tags.V3.atGrit(at), yes(Tags.V3.toGrit, _)) ==>
        (ListSet(reading), Gate.Checked.Passes, failed(Bound.AtLeast(reading, at), under))
    }

    test("notAnchoredRecord reads anchor-record's yes, passing only below at") {
      // A pin of a recorded name: v4's answers are stored under it.
      QuestionName.value(Tags.V4.anchorRecord) ==> "anchor-record"
      val reading = Reading.Yes(Tags.V4.anchorRecord)
      edge(Tags.V4.notAnchoredRecord(at), yes(Tags.V4.anchorRecord, _)) ==>
        (ListSet(reading), failed(Bound.Below(reading, at), 0.3), Gate.Checked.Passes)
    }
  }
}
