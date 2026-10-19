package grit.slack.edge

import grit.core.review.Verdict
import grit.prose.form.{Block, Doc, Mark, Span, Text}

import utest.*

/** [[ReviewPrompt]]: what a review's prompt shows, and the reactions it is answered with. */
object ReviewPromptTests extends TestSuite {

  private val Link = "https://fake.slack.com/archives/C123ABC456/p1515449522000016"

  /** What every prompt in #standup linked by [[Link]] shows. */
  private val Shown = Doc(
    Vector(
      Block.Paragraph(
        Text(
          Vector(
            Span("Would a reply here have been welcome?", Set(Mark.Strong)),
            Span.plain(" "),
            Span("A message in #standup", Set(Mark.Link(Link)))
          )
        )
      ),
      Block.Paragraph(
        Text.plain(
          "👍 welcome · 👎 an interruption · 👤 meant for someone in particular (even if an answer would have helped)"
        )
      )
    )
  )

  val tests = Tests {
    test(
      "a prompt shows only the bold question, its message in its channel linked, and the legend: no gate, reason or number"
    ) {
      ReviewPrompt.doc("#standup", Link) ==> Shown
    }

    test(
      "the three reactions give their verdicts, a skin tone ignored; any other emoji gives none"
    ) {
      Vector("+1", "-1", "bust_in_silhouette", "+1::skin-tone-4", "eyes", "thumbsup", "+1:", "")
        .map(ReviewPrompt.verdict) ==> Vector(
        Some(Verdict.Welcome),
        Some(Verdict.Interruption),
        Some(Verdict.CutIn),
        Some(Verdict.Welcome),
        None,
        None,
        None,
        None
      )
    }
  }
}
