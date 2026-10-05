package grit.eval.harness.reply

import grit.core.id.{EntryId, EntrySeq}
import grit.eval.harness.corpus.Said
import grit.eval.harness.label.{Locator, ReplyLabels}
import grit.turn.TurnOffer

import utest.*
import Fixtures.*

/** A review of replies: at most five, each its asked message, its reply and a few records,
  * never what the model was shown besides.
  */
object ReplyReviewTests extends TestSuite {

  private def file(t: grit.eval.harness.corpus.TurnCase, nearText: String = near): String = {
    val picked = right(
      ReplyReview.pick(
        Vector(t),
        ReplyLabels.Empty,
        ReplyReview.Ask.Named(Vector(grit.core.id.WorkflowId.value(t.workflow)))
      )
    )
    val review = right(
      ReplyReview.review("20261004", picked, at)(c =>
        ReplyReview.reviewed(c, entries(c.root, nearText), speakers, records = true)
      )
    )
    ReplyReview.render(review)
  }

  val tests = Tests {
    test("asking for more than five turns is refused, not cut to five") {
      val turns = (2L to 7L).toVector.map(s => turn(seq = s))
      val ids = turns.map(t => grit.core.id.WorkflowId.value(t.workflow))
      (
        ReplyReview.pick(turns, ReplyLabels.Empty, ReplyReview.Ask.Named(ids)).map(_.turns.size),
        ReplyReview
          .pick(turns, ReplyLabels.Empty, ReplyReview.Ask.Pick(6, ReplyReview.By.Widest))
          .map(_.turns.size),
        ReplyReview
          .pick(turns, ReplyLabels.Empty, ReplyReview.Ask.Named(ids.take(5)))
          .map(_.turns.size)
      ) ==> (Left("name 1 to 5 turns, not 6"), Left("pick 1 to 5 turns, not 6"), Right(5))
    }

    test("no tool result, reasoning, query or other thread message reaches the file") {
      val written = file(turn())
      (
        Markers.filter(written.contains),
        Vector(asked, replied, near, further).filterNot(written.contains)
      ) ==>
        (Vector.empty, Vector.empty)
    }

    test("records are the window's sections, best supported first, never the thread's own turns") {
      val reviewed = right(
        ReplyReview.reviewed(turn(), entries(TurnOffer.Root.Addressed), speakers, records = true)
      )
      reviewed.records.map(_.at) ==> Vector(
        Locator.Messages(c2, ::(EntrySeq(1), List(EntrySeq(2)))),
        Locator.Messages(c3, ::(EntrySeq(4), Nil))
      )
    }

    test("a record is cut at 800 characters, its start kept") {
      val long = "start " + "x" * 2000
      val reviewed = right(
        ReplyReview.reviewed(
          turn(),
          entries(TurnOffer.Root.Addressed, long),
          speakers,
          records = true
        )
      )
      val first =
        reviewed.records.headOption.map(r => (r.text.length, r.cut, r.text.contains("start x")))
      (first, file(turn(), long).contains("(cut at 800 characters)")) ==> (
        Some((800, true, true)),
        true
      )
    }

    test("a TUI turn is shown by its message's entry id, its words as said") {
      val written = file(turn(said = Said.Tui(EntryId("c1-4"))))
      (
        written.contains("said: tui c1-4 · addressed · replied"),
        written.contains(s"> $asked"),
        written.contains(s"> $replied")
      ) ==>
        (true, true, true)
    }

    test("a heard message answered as said to grit is reviewed by its reply, as an addressed one") {
      val byName = turn(root = TurnOffer.Root.ByName)
      ReplyReview
        .reviewed(byName, entries(TurnOffer.Root.ByName), speakers, records = false)
        .map(_.reply) ==> Right(ReplyReview.Reply.Replied(replied))
    }
  }
}
