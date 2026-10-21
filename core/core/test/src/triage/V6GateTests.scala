package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{PluginName, QuestionName}
import grit.core.period.{Judgement, Probability}

import utest.*

/** v6's draft gate over Jev's answers and the facts code read: each branch passes alone, and a
  * fact is read only within a branch of its `AnyOf`, so it never holds a message.
  */
object V6GateTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private val half = p(0.5)
  private val likely = p(0.7)
  private val notes = PluginName.of("notes").getOrElse(sys.error("a name"))

  private def answers(
      toGrit: Double,
      recordsHold: Double,
      asks: Double = 1.0,
      open: Double = 0.9
  ): VectorMap[QuestionName, Answer] =
    VectorMap(
      Tags.V2.gap -> Answer.Choice(
        "asks",
        Vector(Answer.Weight("asks", asks), Answer.Weight("nothing", 1 - asks)),
        0.0
      ),
      Tags.V2.open -> Answer.YesNo(open),
      Tags.V2.to -> Answer.YesNo(0.5),
      Tags.V3.toGrit -> Answer.YesNo(toGrit),
      Tags.V2.durable -> Answer.YesNo(0.1),
      Tags.V6.recordsHold -> Answer.YesNo(recordsHold)
    )

  /** Cues naming someone or not, a verdict's `settled` on the person, and a record match of
    * `plain` times [[Fact.PlainMatch]].
    */
  private def cues(named: Boolean, settled: Option[Double] = None, plain: Option[Double] = None) =
    Cues(
      named,
      None,
      None,
      settled.map(s => Judgement.Weighed(p(0.1), p(s), p(0.1), "jev")),
      plain.map(x => Cues.Match(notes, x * Fact.PlainMatch, 1))
    )

  private def drafts(a: VectorMap[QuestionName, Answer], c: Cues) = Tags.V6.drafts.drafts(a, c)

  /** `gate` with every branch of an `AnyOf` that reads `fact` dropped. */
  private def without(gate: Gate, fact: Fact): Gate = gate match {
    case Gate.Holds(_) => gate
    case Gate.All(gates) => Gate.All(gates.map(without(_, fact)))
    case Gate.AnyOf(first, rest) =>
      (first +: rest).filterNot(_.reads.contains(Reading.Fact(fact))).map(without(_, fact)) match {
        case Vector(only) => only
        case Vector(f, r*) => Gate.AnyOf(f, r.toVector)
        case _ => Gate.AnyOf(Gate.bounds(Bound.Below(Reading.Fact(fact), p(0.0))), Vector.empty)
      }
  }

  val tests = Tests {
    test("a message put to grit drafts, whoever it names and whatever records hold") {
      drafts(answers(toGrit = 0.9, recordsHold = 0.1), cues(named = true)) ==> Some(true)
    }

    test("one nobody in particular is asked drafts when records could hold an answer") {
      (
        drafts(answers(toGrit = 0.1, recordsHold = 0.6), cues(named = false)),
        drafts(answers(toGrit = 0.1, recordsHold = 0.4), cues(named = false)),
        drafts(answers(toGrit = 0.1, recordsHold = 0.6), cues(named = true))
      ) ==> (Some(true), Some(false), Some(false))
    }

    test(
      "one naming someone drafts when records very likely hold one and a record plainly matches"
    ) {
      (
        drafts(answers(toGrit = 0.1, recordsHold = 0.8), cues(named = true, plain = Some(1.0))),
        drafts(answers(toGrit = 0.1, recordsHold = 0.8), cues(named = true, plain = Some(0.5))),
        drafts(answers(toGrit = 0.1, recordsHold = 0.6), cues(named = true, plain = Some(4.0)))
      ) ==> (Some(true), Some(false), Some(false))
    }

    test("one naming someone drafts when someone was left waiting and records could hold one") {
      (
        drafts(answers(toGrit = 0.1, recordsHold = 0.6), cues(named = true, settled = Some(0.8))),
        drafts(answers(toGrit = 0.1, recordsHold = 0.6), cues(named = true, settled = Some(0.6))),
        drafts(answers(toGrit = 0.1, recordsHold = 0.4), cues(named = true, settled = Some(0.9)))
      ) ==> (Some(true), Some(false), Some(false))
    }

    test("a message that asks nothing, or is answered, is held on every branch") {
      val c = cues(named = false, settled = Some(0.9), plain = Some(4.0))
      (
        drafts(answers(toGrit = 0.9, recordsHold = 0.9, asks = 0.2), c),
        drafts(answers(toGrit = 0.9, recordsHold = 0.9, open = 0.2), c)
      ) ==> (Some(false), Some(false))
    }

    test("a message failing every branch is held on each bound it failed, once, in order") {
      def failed(b: Bound, read: Double) = Gate.Failed(b, p(read))
      val recordsHold = Reading.Yes(Tags.V6.recordsHold)
      Tags.V6.drafts.check(
        answers(toGrit = 0.1, recordsHold = 0.2, asks = 0.0, open = 0.25),
        Cues.Empty
      ) ==> Gate.Checked.Fails(
        failed(Bound.AtLeast(Reading.Key(Tags.V2.gap, "asks"), half), 0.0),
        Vector(
          failed(Bound.AtLeast(Reading.Yes(Tags.V2.open), half), 0.25),
          failed(Bound.AtLeast(Reading.Yes(Tags.V3.toGrit), half), 0.1),
          failed(Bound.AtLeast(recordsHold, half), 0.2),
          failed(Bound.AtLeast(recordsHold, likely), 0.2),
          failed(Bound.AtLeast(Reading.Fact(Fact.RecordScore), half), 0.0),
          failed(Bound.AtLeast(Reading.Fact(Fact.SettledOnPerson), likely), 0.0)
        )
      )
    }

    test(
      "a fact is read only within a branch of the gate's AnyOf, and each fact's branches draft a message the rest would hold"
    ) {
      val cases = for {
        asks <- Vector(0.0, 1.0)
        open <- Vector(0.3, 0.9)
        toGrit <- Vector(0.1, 0.9)
        recordsHold <- Vector(0.2, 0.6, 0.8)
        named <- Vector(false, true)
        settled <- Vector(None, Some(0.5), Some(0.8), Some(0.95))
        plain <- Vector(None, Some(0.5), Some(2.0))
      } yield (answers(toGrit, recordsHold, asks, open), cues(named, settled, plain))
      val outside =
        Fact.values.toVector.filter(f => without(Tags.V6.drafts, f).reads.contains(Reading.Fact(f)))
      val addsNothing = Fact.values.toVector.filterNot(f =>
        cases.exists((a, c) =>
          drafts(a, c).contains(true) && !without(Tags.V6.drafts, f).drafts(a, c).contains(true)
        )
      )
      (outside, addsNothing) ==> (Vector.empty, Vector.empty)
    }
  }
}
