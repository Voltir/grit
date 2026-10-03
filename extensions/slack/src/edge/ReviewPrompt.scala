package grit.slack.edge

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.period.Probability
import grit.core.review.{Prompt, Reason, Settled, Verdict}
import grit.core.speech.{Judged, Outcome, Silence}
import grit.prose.form.{Block, Doc, Item, Mark, Span, Text}

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

  /** What `prompt` shows, its message in `channel` (as a person reads it) linked by
    * `permalink`: where it was said, why it was picked, live triage's settled decision, and
    * the shadow's gate and answers, each probability to two places; then the legend of
    * [[Reactions]].
    */
  def doc(prompt: Prompt, channel: String, permalink: String): Doc = {
    val shadow = ShadowName.value(prompt.shadow)
    val (picked, drafts) = prompt.reason match {
      case Reason.ShadowOnly => (s"only $shadow would draft", true)
      case Reason.LiveOnly => ("only live triage would draft", false)
      case Reason.Both => (s"live triage and $shadow would both draft", true)
      case Reason.Neither =>
        (s"a sample of those neither live triage nor $shadow would draft", false)
    }
    val gate = if (drafts) "would draft" else "would stay quiet"
    def line(s: String): Item = Item(Vector(Block.Paragraph(Text.plain(s))))
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
        Block.Paragraph(Text.plain(s"Picked: $picked.")),
        Block.Bullets(
          Vector(
            line(s"live: ${live(prompt.live)}"),
            line((s"$shadow: $gate" +: prompt.answers.toVector.map(answered)).mkString("; "))
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

  /** Live's settled decision, by its kind and numbers alone: no reason a model or a store
    * wrote (a judge's or a turn's failure, why a message was untagged) is shown.
    */
  private def live(settled: Settled): String = settled match {
    case Settled.Drafted(outcome) =>
      val said = outcome match {
        case Outcome.Posted(j) => s"${scored(j)}; posted"
        case Outcome.Below(j, postAt) => s"${scored(j)}; under ${two(postAt)}, not posted"
        case Outcome.Shadowed(j) => s"${scored(j)}; shadowed, not posted"
        case Outcome.Passed => "the model had nothing to add"
        case Outcome.NothingRecalled => "nothing recalled to ground it, not judged"
        case Outcome.Spoken(_) => "grit had replied already, not posted"
        case Outcome.Withdrawn => "speaking stopped, not posted"
        case Outcome.Unjudged(_) => "the judge gave no answer, not posted"
        case Outcome.Failed(_) => "the turn failed before its draft was judged"
      }
      s"drafted; $said"
    case Settled.Held(why) =>
      why match {
        case Silence.Chatter => "held: chatter"
        case Silence.Below(helps, helpsAt) => s"held: helps ${two(helps)} under ${two(helpsAt)}"
        case Silence.AskedOf(_) => "held: names someone else"
        case Silence.Unanswered(_) => "held after the gate: grit's earlier draft in the thread"
        case Silence.Thread(_) => "held after the gate: the thread's rate"
        case Silence.Room(_) => "held after the gate: the room's rate"
        case Silence.Deployment(_) => "held after the gate: the deployment's rate"
        case Silence.OverSpeechCap(_, _) => "held after the gate: the speech cap"
        case Silence.OverBudget => "held after the gate: the budget"
        case Silence.Off => "held before the gate: speaking off"
        case Silence.NoAddress => "held before the gate: no reply address"
        case Silence.Stale(_) => "held before the gate: stale"
        case Silence.Unweighed(_) => "held before the gate: untagged"
      }
  }

  private def scored(j: Judged): String =
    s"grounded ${two(j.grounded)}, worth ${two(j.worth)}"

  /** A question's answer: a yes-no's probability of yes, or a choice's weight on each key. */
  private def answered(qa: (QuestionName, Answer)): String = {
    val (q, a) = qa
    val name = QuestionName.value(q)
    a match {
      case Answer.YesNo(yes) => s"$name ${number(yes)}"
      case Answer.Choice(_, weights, _) =>
        s"$name: ${weights.map(w => s"${w.key} ${number(w.probability)}").mkString(", ")}"
    }
  }

  private def two(p: Probability): String = number(Probability.value(p))

  /** `x` to two places, half up; `?` for a NaN or an infinity. */
  private def number(x: Double): String =
    if (x.isNaN || x.isInfinite) "?"
    else BigDecimal(x).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString
}
