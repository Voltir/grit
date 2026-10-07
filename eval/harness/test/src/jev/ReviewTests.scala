package grit.eval.harness.jev

import grit.core.triage.Corpora
import grit.eval.harness.capture.CaseId
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}

import utest.*

/** A review file's line: the request as the classifier receives it, its digest, and what the
  * thread left out. Every message here is synthetic.
  */
object ReviewTests extends TestSuite {

  private val id = CaseId.read("C1/1727000000.000100").fold(sys.error, identity)
  private val state = TriageQuestion.State("standup moves to 10:00", "Ana", "Ben: when?")
  private val request = TriageQuestions.V1.request(state, Corpora.Empty)

  val tests = Tests {
    // The review file's keys are what the labelling export reads: pinned.
    test("a line holds the request as sent, its digest, and the cut where it was taken") {
      val line = Review.line(
        Review.Shown(id, Some(Review.Triage(request, Some(Review.Cut("Ben: earlier\n", 0)))), None)
      )
      line.obj.keys.toVector ==> Vector("case", "triage", "stitch")
      line("case").str ==> "C1/1727000000.000100"
      line("triage")("request") ==> grit.core.classify.Request.json(request)
      line("triage")("request")("state")("new_message").str ==> "standup moves to 10:00"
      line("triage")("digest").str ==> request.digest
      line("triage")("cut") ==> ujson.Obj("text" -> "Ben: earlier\n", "at" -> 0)
      line("stitch") ==> ujson.Null
    }

    test("a line without a cut asked for, or a stitch's live state, holds null there") {
      val line = Review.line(
        Review.Shown(id, Some(Review.Triage(request, None)), Some(Review.Stitch(request, None)))
      )
      (line("triage")("cut"), line("stitch")("live"), line("stitch")("digest").str) ==>
        (ujson.Null, ujson.Null, request.digest)
    }
  }
}
