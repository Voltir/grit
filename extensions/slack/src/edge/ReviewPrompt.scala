package grit.slack.edge

import scala.collection.immutable.VectorMap

import grit.core.review.Verdict
import grit.prose.form.{Block, Doc, Mark, Span, Text}

/** A review's prompt as the Slack edge posts it, and the reactions its rater answers with. */
private[edge] object ReviewPrompt {

  /** The reactions grit adds to every prompt, as Slack names them, each with the verdict it
    * gives: `+1` welcome, `-1` an interruption, `bust_in_silhouette` meant for someone in
    * particular.
    */
  val Reactions: VectorMap[String, Verdict] = VectorMap(
    "+1" -> Verdict.Welcome,
    "-1" -> Verdict.Interruption,
    "bust_in_silhouette" -> Verdict.CutIn
  )

  /** The verdict `emoji` gives, as Slack names it, a skin tone after `::` ignored; `None` for
    * any emoji not in [[Reactions]].
    */
  def verdict(emoji: String): Option[Verdict] =
    Reactions.get(emoji.indexOf(SkinTone) match {
      case -1 => emoji
      case i => emoji.take(i)
    })

  private val SkinTone = "::skin-tone-"

  /** What every prompt shows: the question, its message in `channel` (as a person reads it)
    * linked by `permalink`, and the legend of [[Reactions]].
    */
  def doc(channel: String, permalink: String): Doc =
    Doc(
      Vector(
        Block.Paragraph(
          Text(
            Vector(
              Span("Would a reply here have been welcome?", Set(Mark.Strong)),
              Span.plain(" "),
              Span(s"A message in $channel", Set(Mark.Link(permalink)))
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
}
