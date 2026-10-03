package grit.eval.harness.reply

import grit.core.id.{EntrySeq, WorkflowId}
import grit.eval.harness.label.{Found, Locator, Quality, ReplyGuide, ReplyLabel, ReplyLabels}
import grit.turn.TurnOffer

import utest.*
import Fixtures.*

/** A review file's label stubs, filled by a person, read into `reply-labels.json`. */
object ReplyLabelsTests extends TestSuite {

  /** A review of turns 2 and 3 of `c1`, records shown. */
  private val written: String = {
    val turns = Vector(turn(seq = 2), turn(seq = 3, root = TurnOffer.Root.Heard))
    val picked = right(
      ReplyReview.pick(turns, ReplyLabels.Empty, ReplyReview.Ask.Named(Vector("c1:2", "c1:3")))
    )
    ReplyReview.render(
      right(
        ReplyReview.review("20261004", picked, at)(t =>
          ReplyReview.reviewed(
            t,
            entries(t.root, seq = grit.core.id.TurnSeq.value(t.turn)),
            speakers,
            records = true
          )
        )
      )
    )
  }

  /** `file` with the first blank `key:` line after case `w`'s heading given `value`. */
  private def fill(file: String, w: String, key: String, value: String): String = {
    val from = file.indexOf(s"## $w\n")
    val at = file.indexOf(s"\n$key:\n", from)
    file.substring(0, at) + s"\n$key: $value\n" + file.substring(at + key.length + 3)
  }

  val tests = Tests {
    test("a filled stub round-trips through reply-labels.json, a blank case left out") {
      val filled = fill(
        fill(fill(written, "c1:2", "reply", "bad"), "c1:2", "answer", "nowhere"),
        "c1:2",
        "speak",
        "no"
      )
      val read = right(ReplyReview.labels(filled))
      val expected =
        Vector(
          WorkflowId("c1:2") -> ReplyLabel(
            ReplyGuide.V1,
            Some(Quality.Bad),
            Some(Found.Nowhere),
            Some(false)
          )
        )
      (read, ReplyLabels.read(ReplyLabels.written(ReplyLabels.Empty.merged(read)))) ==>
        (expected, Right(ReplyLabels(expected.toMap)))
    }

    test("shown 2 is the second record's locator") {
      val filled = fill(fill(written, "c1:3", "reply", "good"), "c1:3", "answer", "shown 2")
      ReplyReview.labels(filled).map(_.map(_._2.answer)) ==>
        Right(Vector(Some(Found.Shown(::(Locator.Messages(c3, ::(EntrySeq(4), Nil)), Nil)))))
    }

    test("a file under a guide this build does not know is refused") {
      val filled =
        fill(written.replace("guide: reply-v1", "guide: reply-v9"), "c1:2", "reply", "good")
      ReplyReview.labels(filled) ==> Left("no reply guide reply-v9")
    }
  }
}
