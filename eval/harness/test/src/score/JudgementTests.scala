package grit.eval.harness.score

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.{PrincipalId, QuestionName, ShadowName}
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.review.{Reason, Verdict}
import grit.core.triage.Kind
import grit.eval.harness.corpus.{CaseId, Digest}
import grit.eval.harness.label.{Rated, Verdicts}
import grit.eval.harness.log.{CacheKey, Outcome, Row, Suite, Weights}
import grit.lifecycle.triage.TriageQuestions

import utest.*
import Fixtures.id

/** Verdicts against live's helps gate and a question set's, over synthetic rows. */
object JudgementTests extends TestSuite {

  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)
  private def shadow(n: String): ShadowName = ShadowName.of(n).fold(sys.error, identity)

  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)
  private val helpsAt = Probability.clamped(0.6)
  private val At = Instant.parse("2026-10-03T09:00:00Z")

  /** Live's kept row of `c`, a question, its helps gate passed when `gates`. */
  private def live(c: CaseId, gates: Boolean): Row[Vector[Weights]] =
    Fixtures.row(
      Suite.Triage,
      c,
      0,
      Outcome.Answered(
        Vector(
          Weights.Choice(Kind.Question.ordinal, Kind.values.toVector.map(_ => 0.2), 0.0),
          Weights.YesNo(0.5),
          Weights.YesNo(0.5),
          Weights.YesNo(if (gates) 0.9 else 0.1)
        )
      )
    )

  /** V2's answers for `c`: its draft passed when `drafts`, `to` as given; no `anchor` when
    * `undecided`.
    */
  private def set(
      c: CaseId,
      drafts: Boolean,
      to: Double,
      undecided: Boolean = false
  ): Row[VectorMap[QuestionName, Answer]] = {
    val asks = if (drafts) 0.8 else 0.1
    val ws = Vector(Answer.Weight("asks", asks), Answer.Weight("nothing", 1 - asks))
    Row(
      Suite.Triage,
      c,
      0,
      Digest.text("r"),
      key,
      "m",
      None,
      Outcome.Answered(
        VectorMap(
          name("gap") -> Answer.Choice("asks", ws, Answer.confidence(ws.map(_.probability))),
          name("open") -> Answer.YesNo(0.9),
          name("to") -> Answer.YesNo(to)
        ) ++ Option.when(!undecided)(name("anchor") -> Answer.YesNo(0.1))
      ),
      Usage.Zero,
      1.milli,
      false,
      None
    )
  }

  private def rated(s: ShadowName, r: Reason, v: Verdict): Rated =
    Rated(s, r, v, PrincipalId("slack:T1/U1"), At)

  val tests = Tests {
    test(
      "each verdict on a case both gates decided counts under its shadow and reason: each gate's decision against speak, the set's to against to-a-person; one either gate cannot decide is undecided"
    ) {
      val (v2, v3) = (shadow("triage-v2"), shadow("triage-v3"))
      val cs = (1 to 8).map(n => id(s"C1/$n")).toVector
      // A `to` at 0.5 or more is to a person, and stops the set's draft.
      val lives = Vector(
        live(cs(0), gates = false),
        live(cs(1), gates = true),
        live(cs(2), gates = false),
        live(cs(3), gates = true),
        live(cs(4), gates = false),
        live(cs(5), gates = true),
        // cs(6): no live row.
        live(cs(7), gates = true)
      )
      val sets = Vector(
        set(cs(0), drafts = true, to = 0.1), // welcome: set right, live wrong, to right
        set(cs(1), drafts = true, to = 0.7), // cut-in: live wrong, set right, to right
        set(cs(2), drafts = false, to = 0.5), // interruption: both right, to wrong
        set(cs(3), drafts = false, to = 0.1), // welcome: live right, set wrong, to right
        set(cs(4), drafts = false, to = 0.2), // interruption, neither: both right, to right
        set(cs(5), drafts = true, to = 0.1), // welcome under another shadow: both right
        set(cs(6), drafts = true, to = 0.1),
        set(cs(7), drafts = true, to = 0.1, undecided = true)
      )
      val verdicts = Verdicts(
        Map(
          cs(0) -> rated(v2, Reason.ShadowOnly, Verdict.Welcome),
          cs(1) -> rated(v2, Reason.Both, Verdict.CutIn),
          cs(2) -> rated(v2, Reason.ShadowOnly, Verdict.Interruption),
          cs(3) -> rated(v2, Reason.LiveOnly, Verdict.Welcome),
          cs(4) -> rated(v2, Reason.Neither, Verdict.Interruption),
          cs(5) -> rated(v3, Reason.Both, Verdict.Welcome),
          cs(6) -> rated(v2, Reason.Neither, Verdict.Welcome),
          cs(7) -> rated(v2, Reason.Neither, Verdict.Welcome)
        )
      )
      Judgement.of(
        lives,
        sets,
        TriageQuestions.V2.speak,
        helpsAt,
        Some(name("to")),
        verdicts
      ) ==> Judgement(
        Vector(
          Judgement.Of(v2, Reason.ShadowOnly, 2, 1, 2, Some(Judgement.Matched(1, 2))),
          Judgement.Of(v2, Reason.LiveOnly, 1, 1, 0, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v2, Reason.Both, 1, 0, 1, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v2, Reason.Neither, 1, 1, 1, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v3, Reason.Both, 1, 1, 1, Some(Judgement.Matched(1, 1)))
        ),
        2
      )
    }

    test("a set naming no to is judged on its gate alone, and no verdict judges nothing") {
      val c = id("C1/1")
      val v2 = shadow("triage-v2")
      val verdicts = Verdicts(Map(c -> rated(v2, Reason.Both, Verdict.Welcome)))
      val judge = Judgement.of(
        Vector(live(c, gates = true)),
        Vector(set(c, drafts = true, to = 0.1)),
        TriageQuestions.V2.speak,
        helpsAt,
        _: Option[QuestionName],
        _: Verdicts
      )
      (judge(None, verdicts), judge(Some(name("to")), Verdicts.Empty)) ==> (
        Judgement(Vector(Judgement.Of(v2, Reason.Both, 1, 1, 1, None)), 0),
        Judgement(Vector.empty, 0)
      )
    }
  }
}
