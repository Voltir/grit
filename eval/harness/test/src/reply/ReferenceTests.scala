package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, EntrySeq, WorkflowId}
import grit.core.message.Tokens
import grit.core.place.Service
import grit.eval.harness.corpus.{Part, Parts}
import grit.eval.harness.label.{Found, Locator, Quality, ReplyGuide, ReplyLabel, ReplyLabels}

import utest.*

/** Expectations of a turn's build, each judged passing and failing, and read from labels and
  * from the hand-written file. Every id here is made up.
  */
object ReferenceTests extends TestSuite {

  import Fixtures.right

  private val c = ConversationId("c")
  private val github = right(Service.of("github"))
  private def seqs(xs: Long*) = xs.toVector.map(EntrySeq(_))
  private def part(kind: Part.Kind, xs: Long*) = Part(kind, c, seqs(xs*), Tokens(10), None)
  private def window(ps: Part*) = Some(Parts(ps.toVector, Tokens.Zero, Tokens.Zero))
  private val record = Locator.Record(c, EntrySeq(4))
  private val messages = Locator.Messages(c, ::(EntrySeq(1), List(EntrySeq(2))))

  val tests = Tests {
    test("holds passes when the window shows every entry its locator names, else fails") {
      def judge(w: Option[Parts]) =
        Reference.judge(Vector(Expect.Holds(messages)), w, Some(Set.empty))
      // Seqs 1 and 2 across two parts of any kind; with 2 missing, it fails.
      judge(window(part(Part.Kind.Open, 1), part(Part.Kind.Recent, 2, 3))) ==> Judged.Pass
      judge(window(part(Part.Kind.Open, 1, 3))) ==> Judged.Fail
    }

    test("omits passes when the window shows none of its locator's entries, and fails on one") {
      def judge(w: Option[Parts]) =
        Reference.judge(Vector(Expect.Omits(messages)), w, Some(Set.empty))
      // Seqs 1 and 2 are named; a window showing 1 alone shows one of them.
      judge(window(part(Part.Kind.Open, 3))) ==> Judged.Pass
      judge(window(part(Part.Kind.Recent, 1, 3))) ==> Judged.Fail
      judge(None) ==> Judged.Unjudged
    }

    test("a record is held only by a record or a closed part showing its closing") {
      def judge(w: Option[Parts]) =
        Reference.judge(Vector(Expect.Holds(record)), w, Some(Set.empty))
      judge(window(part(Part.Kind.Closed, 4))) ==> Judged.Pass
      judge(window(part(Part.Kind.Recent, 4))) ==> Judged.Fail
    }

    test("a holds of a window not rebuilt is unjudged, unless another expectation fails") {
      Reference.judge(Vector(Expect.Holds(record)), None, Some(Set.empty)) ==> Judged.Unjudged
      Reference.judge(
        Vector(Expect.Holds(record), Expect.Offers(github)),
        None,
        Some(Set.empty)
      ) ==>
        Judged.Fail
    }

    test("offers passes when a tool of the service is offered, withholds when none is") {
      Reference.judge(Vector(Expect.Offers(github)), None, Some(Set(github))) ==> Judged.Pass
      Reference.judge(Vector(Expect.Offers(github)), None, Some(Set.empty)) ==> Judged.Fail
      Reference.judge(Vector(Expect.Withholds(github)), None, Some(Set.empty)) ==> Judged.Pass
      Reference.judge(Vector(Expect.Withholds(github)), None, Some(Set(github))) ==> Judged.Fail
    }

    test("a service's expectation of a turn whose services were not recorded is unjudged") {
      Vector(Expect.Offers(github), Expect.Withholds(github)).map(e =>
        Reference.judge(Vector(e), None, None)
      ) ==> Vector(Judged.Unjudged, Judged.Unjudged)
    }

    test("a label whose answer was found in records shown expects each held; others nothing") {
      def label(answer: Found) =
        ReplyLabel(ReplyGuide.V1, Some(Quality.Good), Some(answer), Some(true))
      val labels = ReplyLabels(
        Map(
          WorkflowId("w2") -> label(Found.Shown(::(record, List(messages)))),
          WorkflowId("w1") -> label(Found.Thread)
        )
      )
      Reference.labelled(labels) ==>
        Reference(
          VectorMap(WorkflowId("w2") -> Vector(Expect.Holds(record), Expect.Holds(messages)))
        )
    }

    test("the file's expectations are read by turn, in the order written") {
      val text =
        """{"turns": {"w1": [{"offers": "github"},
          |  {"holds": {"record": {"conversation": "c", "closing": 4}}},
          |  {"withholds": "github"},
          |  {"omits": {"messages": {"conversation": "c", "seqs": [1, 2]}}}]}}""".stripMargin
      Reference.read(text) ==> Right(
        Reference(
          VectorMap(
            WorkflowId("w1") ->
              Vector(
                Expect.Offers(github),
                Expect.Holds(record),
                Expect.Withholds(github),
                Expect.Omits(messages)
              )
          )
        )
      )
      Reference.read("""{"turns": {"w1": [{"drafts": true}]}}""") ==>
        Left("reference: w1: not one of holds, omits, offers or withholds")
    }
  }
}
