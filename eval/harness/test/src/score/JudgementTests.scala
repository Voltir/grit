package grit.eval.harness.score

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.identity.TestAccounts
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.review.{Reason, Verdict}
import grit.core.triage.{Kind, Tags}
import grit.eval.harness.capture.{CaseId, Digest}
import grit.eval.harness.jev.Sets
import grit.eval.harness.label.{Rated, Verdicts}
import grit.eval.harness.log.{CacheKey, Outcome, Row, Suite}

import utest.*
import Fixtures.id

/** Verdicts against live's draft and a question set's, each by its own set's gate, over
  * synthetic rows.
  */
object JudgementTests extends TestSuite {

  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)
  private def shadow(n: String): ShadowName = ShadowName.of(n).fold(sys.error, identity)

  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)
  private val At = Instant.parse("2026-10-03T09:00:00Z")

  /** Live's kept row of `c` before v2, a question, its v1 gate passed when `gates`. */
  private def live(c: CaseId, gates: Boolean): Row[VectorMap[QuestionName, Answer]] =
    row(
      c,
      Tags.V1.answers(Kind.Question, p(0.4), p(0.5), p(0.5), p(if (gates) 0.9 else 0.1))
    )

  private def p(d: Double): Probability = Probability.clamped(d)

  private def row(
      c: CaseId,
      answers: VectorMap[QuestionName, Answer]
  ): Row[VectorMap[QuestionName, Answer]] =
    Row(
      Suite.Triage,
      c,
      0,
      Digest.text("r"),
      key,
      "m",
      None,
      Outcome.Answered(answers),
      Usage.Zero,
      1.milli,
      false,
      None
    )

  private def asV1(rows: Vector[Row[VectorMap[QuestionName, Answer]]]): Drafting =
    Drafting(rows, Sets.V1.speak, Sets.V1.durable, Sets.V1.to)

  private def asV2(rows: Vector[Row[VectorMap[QuestionName, Answer]]]): Drafting =
    Drafting(rows, Sets.V2.speak, Sets.V2.durable, Sets.V2.to)

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
    row(
      c,
      VectorMap(
        name("gap") -> Answer.Choice("asks", ws, Answer.confidence(ws.map(_.probability))),
        name("open") -> Answer.YesNo(0.9),
        name("to") -> Answer.YesNo(to)
      ) ++ Option.when(!undecided)(name("anchor") -> Answer.YesNo(0.1))
    )
  }

  private def rated(s: ShadowName, r: Reason, v: Verdict): Rated =
    Rated(s, r, v, TestAccounts.account("slack:T1/U1"), At)

  val tests = Tests {
    test(
      "each verdict on a case both gates decided counts under its shadow and reason: each gate's decision against speak, each side's to against to-a-person; one either gate cannot decide is undecided"
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
      val judged = Judgement(
        Vector(
          Judgement.Of(v2, Reason.ShadowOnly, 2, 1, 2, None, Some(Judgement.Matched(1, 2))),
          Judgement.Of(v2, Reason.LiveOnly, 1, 1, 0, None, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v2, Reason.Both, 1, 0, 1, None, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v2, Reason.Neither, 1, 1, 1, None, Some(Judgement.Matched(1, 1))),
          Judgement.Of(v3, Reason.Both, 1, 1, 1, None, Some(Judgement.Matched(1, 1)))
        ),
        2
      )
      // Both ways round: live asking v2 against a v1 shadow reads live's to, not the set's.
      (
        Judgement.of(asV1(lives), asV2(sets), verdicts),
        Judgement.of(asV2(sets), asV1(lives), verdicts)
      ) ==> (
        judged,
        judged.copy(reasons =
          judged.reasons.map(o =>
            o.copy(live = o.set, set = o.live, liveTo = o.setTo, setTo = o.liveTo)
          )
        )
      )
    }

    test("a side naming no to is judged on its gate alone, and no verdict judges nothing") {
      val c = id("C1/1")
      val v2 = shadow("triage-v2")
      val verdicts = Verdicts(Map(c -> rated(v2, Reason.Both, Verdict.Welcome)))
      val (lives, sets) = (Vector(live(c, gates = true)), Vector(set(c, drafts = true, to = 0.1)))
      (
        Judgement.of(asV1(lives), asV2(sets).copy(to = None), verdicts),
        Judgement.of(asV1(lives), asV2(sets), Verdicts.Empty)
      ) ==> (
        Judgement(Vector(Judgement.Of(v2, Reason.Both, 1, 1, 1, None, None)), 0),
        Judgement(Vector.empty, 0)
      )
    }
  }
}
