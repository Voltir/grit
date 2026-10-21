package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

object GateTests extends TestSuite {

  private def n(text: String): QuestionName =
    QuestionName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  private def p(x: Double): Probability = Probability.clamped(x)

  private val half = p(0.5)

  /** A choice reporting `reported`, whatever its weights say. */
  private def choice(reported: String, weights: (String, Double)*): Answer =
    Answer.Choice(reported, weights.toVector.map(Answer.Weight(_, _)), 0.0)

  private val kind = n("kind")
  private val helps = n("helps")
  private val to = n("to")

  /** What `fact` reads of `cues`, as a failed `Below(fact, 0)` reports it; `None` when unread. */
  private def read(fact: Fact, cues: Cues): Option[Double] =
    Gate.bounds(Bound.Below(Reading.Fact(fact), p(0.0))).check(VectorMap.empty, cues) match {
      case Gate.Checked.Fails(failed, _) => Some(Probability.value(failed.read))
      case _ => None
    }

  private val notes = grit.core.id.PluginName.of("notes").getOrElse(sys.error("a name"))

  val tests = Tests {
    test(
      "a fact reads its cue: Named 1 or 0, SettledOnPerson the verdict's onPerson, RecordScore s/(1+s); each 0 on Empty"
    ) {
      import grit.core.period.Judgement
      val full = Cues(
        named = true,
        spoken = None,
        continued = None,
        settled = Some(Judgement.Weighed(p(0.125), p(0.75), p(0.125), "jev")),
        records = Some(Cues.Match(notes, 3.0, 2))
      )
      val unanswered = Cues.Empty.copy(settled = Some(Judgement.Unanswered("no model")))
      val facts = Vector(Fact.Named, Fact.SettledOnPerson, Fact.RecordScore)
      (
        facts.map(read(_, full)),
        facts.map(read(_, Cues.Empty)),
        read(Fact.SettledOnPerson, unanswered),
        read(Fact.RecordScore, Cues.Empty.copy(records = Some(Cues.Match(notes, 1.0, 1))))
      ) ==> (
        Vector(Some(1.0), Some(0.75), Some(0.75)),
        Vector(Some(0.0), Some(0.0), Some(0.0)),
        Some(0.0),
        Some(0.5)
      )
    }

    test("a fact is never unread") {
      val gate = Gate.either(
        Gate.bounds(Bound.AtLeast(Reading.Fact(Fact.Named), half)),
        Gate.bounds(Bound.AtLeast(Reading.Fact(Fact.RecordScore), half))
      )
      (
        gate.check(VectorMap.empty, Cues.Empty),
        gate.drafts(VectorMap.empty, Cues.Empty.copy(named = true))
      ) ==> (
        Gate.Checked.Fails(
          Gate.Failed(Bound.AtLeast(Reading.Fact(Fact.Named), half), p(0.0)),
          Vector(Gate.Failed(Bound.AtLeast(Reading.Fact(Fact.RecordScore), half), p(0.0)))
        ),
        Some(true)
      )
    }

    test(
      "Chosen reads 1 for a choice's most weighted key, the first on a tie, whatever choice it reports, and 0 otherwise"
    ) {
      // A gate that fails at Chosen below 0.5 shows what it read there.
      def read(reading: Reading, answer: Answer) =
        Gate
          .bounds(Bound.Below(reading, p(0.0)))
          .check(VectorMap(kind -> answer), Cues.Empty) match {
          case Gate.Checked.Fails(Gate.Failed(_, v), _) => Some(Probability.value(v))
          case _ => None
        }
      val reportsChatter = choice("chatter", "question" -> 0.4, "chatter" -> 0.3, "answer" -> 0.3)
      val tie = choice("b", "a" -> 0.5, "b" -> 0.5)
      val none = choice("a", "a" -> 0.0, "b" -> 0.0)
      Vector(
        read(Reading.Chosen(kind, "chatter"), reportsChatter),
        read(Reading.Chosen(kind, "question"), reportsChatter),
        read(Reading.Chosen(kind, "a"), tie),
        read(Reading.Chosen(kind, "b"), tie),
        read(Reading.Chosen(kind, "a"), none),
        read(Reading.Chosen(kind, "missing"), reportsChatter)
      ) ==> Vector(Some(0.0), Some(1.0), Some(1.0), Some(0.0), Some(0.0), Some(0.0))
    }

    test("check fails with every bound that fails, in order, each with what it read") {
      val gate = Gate.bounds(
        Bound.Below(Reading.Chosen(kind, "chatter"), half),
        Bound.AtLeast(Reading.Yes(helps), half),
        Bound.Below(Reading.Yes(to), half)
      )
      val answers = VectorMap[QuestionName, Answer](
        kind -> choice("chatter", "chatter" -> 0.9, "question" -> 0.1),
        helps -> Answer.YesNo(0.75),
        to -> Answer.YesNo(0.5)
      )
      gate.check(answers, Cues.Empty) ==> Gate.Checked.Fails(
        Gate.Failed(Bound.Below(Reading.Chosen(kind, "chatter"), half), p(1.0)),
        Vector(Gate.Failed(Bound.Below(Reading.Yes(to), half), p(0.5)))
      )
      gate.check(answers.updated(kind, choice("question", "question" -> 1.0)), Cues.Empty) ==>
        Gate.Checked.Fails(Gate.Failed(Bound.Below(Reading.Yes(to), half), p(0.5)), Vector.empty)
      gate.check(
        answers
          .updated(kind, choice("question", "question" -> 1.0))
          .updated(to, Answer.YesNo(0.49)),
        Cues.Empty
      ) ==> Gate.Checked.Passes
    }

    test(
      "check fails on a failing bound whether or not a bound before it is unread, and is Unread at the first unread bound only when none fails; drafts says so"
    ) {
      val gate = Gate.bounds(
        Bound.AtLeast(Reading.Yes(helps), half),
        Bound.AtLeast(Reading.Key(kind, "x"), half)
      )
      // helps is answered as a choice, not a yes/no: unread. kind weighs x at 0: it fails.
      val unreadThenFails = VectorMap[QuestionName, Answer](
        helps -> choice("x", "x" -> 1.0),
        kind -> choice("y", "y" -> 1.0)
      )
      val unreadThenPasses = unreadThenFails.updated(kind, choice("x", "x" -> 1.0))
      (gate.check(unreadThenFails, Cues.Empty), gate.drafts(unreadThenFails, Cues.Empty)) ==> (
        Gate.Checked.Fails(
          Gate.Failed(Bound.AtLeast(Reading.Key(kind, "x"), half), p(0.0)),
          Vector.empty
        ),
        Some(false)
      )
      (gate.check(unreadThenPasses, Cues.Empty), gate.drafts(unreadThenPasses, Cues.Empty)) ==>
        (Gate.Checked.Unread(Reading.Yes(helps)), None)
      val passing = VectorMap[QuestionName, Answer](
        helps -> Answer.YesNo(0.5),
        kind -> choice("x", "x" -> 0.5, "y" -> 0.5)
      )
      (gate.check(passing, Cues.Empty), gate.drafts(passing, Cues.Empty)) ==> (
        Gate.Checked.Passes,
        Some(true)
      )
    }
  }
}
