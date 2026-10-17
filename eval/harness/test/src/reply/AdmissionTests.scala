package grit.eval.harness.reply

import grit.core.context.{Results, Retrieved}
import grit.core.id.TurnSeq
import grit.core.period.Probability
import grit.turn.{TurnRecord, TurnRelevance}

import TurnRelevance.Verdict
import utest.*

/** A recorded search's results admitted again under another pass mark or count judged. */
object AdmissionTests extends TestSuite {

  private def own(n: Long): Retrieved = Retrieved.Own(TurnSeq(n))
  private def p(x: Double): Probability = Probability.clamped(x)

  // Four found: one passed at 0.6, one rejected at 0.4, one unjudged, one passed at 0.9.
  private val retrieval = TurnRecord.Retrieval(
    "Falcon budget",
    Results(Vector(own(0) -> 4.0, own(1) -> 3.0, own(2) -> 2.0, own(3) -> 1.0)),
    Vector(
      own(0) -> Verdict.Passed(p(0.6)),
      own(1) -> Verdict.Rejected(p(0.4)),
      own(2) -> Verdict.Unjudged("no classifier"),
      own(3) -> Verdict.Passed(p(0.9))
    )
  )

  private def admitted(judged: Int, pass: Double): Vector[Retrieved] =
    Admission
      .of(judged, p(pass))
      .fold(sys.error(_), identity)
      .admitted(retrieval)
      .ranked
      .map(_._1)

  val tests = Tests {
    test("the shipped admission packs what the turn's own verdicts admitted") {
      Admission.Shipped.admitted(retrieval) ==>
        TurnRelevance.admitted(retrieval.results, retrieval.verdicts)
    }

    test("a higher pass mark rejects one passed under it, and a lower passes one rejected") {
      admitted(30, 0.7) ==> Vector(own(2), own(3))
      admitted(30, 0.3) ==> Vector(own(0), own(1), own(2), own(3))
    }

    test("an unjudged candidate is admitted whatever the pass mark") {
      admitted(30, 1.0) ==> Vector(own(2))
    }

    test("fewer judged packs none past them, judged or not") {
      admitted(2, 0.5) ==> Vector(own(0))
      admitted(0, 0.5) ==> Vector()
    }

    test("more judged than the turn judged is refused, its verdicts not recorded") {
      Admission.of(TurnRelevance.Judged + 1, p(0.5)).isLeft ==> true
      Admission.of(-1, p(0.5)).isLeft ==> true
    }
  }
}
