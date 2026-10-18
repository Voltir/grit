package grit.eval.harness.jev

import java.time.Instant

import grit.core.stitch.Stitching
import grit.core.triage.Corpora
import grit.eval.harness.capture.CaseId
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}

import utest.*

/** A review file's line: the request as the classifier receives it, its digest, and what the
  * thread left out. Every message here is synthetic.
  */
object ReviewTests extends TestSuite {

  private val id = CaseId.read("C1/1727000000.000100").fold(sys.error, identity)
  private val thread = Stitching.fit(
    Stitching.Excerpt.empty,
    Vector(Stitching.Line("Ben", "when?", Instant.EPOCH)),
    "\n\n",
    TriageQuestion.ThreadChars
  )
  private val state =
    TriageQuestion.State("standup moves to 10:00", "Ana", Instant.EPOCH, thread)
  private val earlier = ujson.Obj("from" -> "Ben", "text" -> "earlier", "ago" -> "1 minute")
  private val request = TriageQuestions.V1.request(state, Corpora.Empty)

  val tests = Tests {
    // The review file's keys are what the labelling export reads: pinned.
    test("a line holds the request as sent, its digest, and the messages its thread left out") {
      val line = Review.line(
        Review.Shown(id, Some(Review.Triage(request, Some(Review.Cut(Vector(earlier))))), None)
      )
      line.obj.keys.toVector ==> Vector("case", "triage", "stitch")
      line("case").str ==> "C1/1727000000.000100"
      line("triage")("request") ==> grit.core.classify.Request.json(request)
      line("triage")("request")("state")("new_message").str ==> "standup moves to 10:00"
      line("triage")("digest").str ==> request.digest
      line("triage")("cut") ==> ujson.Arr(earlier)
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
