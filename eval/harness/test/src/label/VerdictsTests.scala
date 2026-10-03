package grit.eval.harness.label

import java.time.Instant

import grit.core.id.{PrincipalId, ShadowName}
import grit.core.review.{Reason, Verdict}
import grit.eval.harness.corpus.CaseId

import utest.*

/** The verdicts file `pull` writes and `compare --gate` reads. Every id here is synthetic. */
object VerdictsTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  private val v2 = ShadowName.of("triage-v2").fold(sys.error, identity)
  private val rater = PrincipalId("slack:T1/U1")
  private val At = Instant.parse("2026-10-03T09:00:00Z")

  private val verdicts = Verdicts(
    Map(
      id("C1/2.0") -> Rated(v2, Reason.Neither, Verdict.CutIn, rater, At.plusSeconds(1)),
      id("C1/1.0") -> Rated(v2, Reason.ShadowOnly, Verdict.Welcome, rater, At),
      id("C2/1.0") -> Rated(v2, Reason.LiveOnly, Verdict.Interruption, rater, At)
    )
  )

  val tests = Tests {
    test("verdicts are written in case order, each its shadow, reason, verdict, rater and time") {
      Verdicts.written(verdicts) ==>
        """{
          |  "cases": {
          |    "C1/1.0": {
          |      "shadow": "triage-v2",
          |      "reason": "shadow-only",
          |      "verdict": "welcome",
          |      "rater": "slack:T1/U1",
          |      "at": "2026-10-03T09:00:00Z"
          |    },
          |    "C1/2.0": {
          |      "shadow": "triage-v2",
          |      "reason": "neither",
          |      "verdict": "cut-in",
          |      "rater": "slack:T1/U1",
          |      "at": "2026-10-03T09:00:01Z"
          |    },
          |    "C2/1.0": {
          |      "shadow": "triage-v2",
          |      "reason": "live-only",
          |      "verdict": "interruption",
          |      "rater": "slack:T1/U1",
          |      "at": "2026-10-03T09:00:00Z"
          |    }
          |  }
          |}
          |""".stripMargin
    }

    test("verdicts read back as they were written, every reason and verdict") {
      val every = Verdicts(
        Reason.values.toVector
          .flatMap(r => Verdict.values.toVector.map(v => (r, v)))
          .zipWithIndex
          .map { case ((r, v), i) => id(s"C1/$i.0") -> Rated(v2, r, v, rater, At) }
          .toMap
      )
      Verdicts.read(Verdicts.written(every)) ==> Right(every)
    }

    test("a verdict of no form is refused, naming its case and field") {
      val unknown = Verdicts.written(verdicts).replace("\"cut-in\"", "\"meh\"")
      val noTime =
        Verdicts.written(verdicts).replace("\"at\": \"2026-10-03T09:00:01Z\"", "\"at\": 1")
      (Verdicts.read(unknown), Verdicts.read(noTime), Verdicts.read("[]")) ==> (
        Left("verdicts: C1/2.0: verdict meh"),
        Left("verdicts: C1/2.0: at is not a string"),
        Left("verdicts: no cases")
      )
    }
  }
}
