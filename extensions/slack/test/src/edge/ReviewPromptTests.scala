package grit.slack.edge

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, PrincipalId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.review.{Prompt, Reason, Settled, Verdict}
import grit.core.speech.{Judged, Outcome, Silence}
import grit.core.spend.{DailyCap, Spend}
import grit.core.store.Origin
import grit.prose.form.{Block, Doc, Item, Mark, Span, Text}

import utest.*

/** [[ReviewPrompt]]: what a review's prompt shows, and the reactions it is answered with. */
object ReviewPromptTests extends TestSuite {
  import PickedPrompts.*

  private def p(x: Double): Probability = Probability.clamped(x)

  private val Link = "https://fake.slack.com/archives/C123ABC456/p1515449522000016"

  private def prompt(reason: Reason, live: Settled): Prompt =
    Prompt(
      EntryId("in:c1:1515449522.000016"),
      Origin.Slack("T123ABC456", "C123ABC456", "1515449522.000016"),
      Shadow,
      reason,
      live,
      Answers
    )

  /** Each paragraph's and bullet's text, in order. */
  private def lines(doc: Doc): Vector[String] = doc.blocks.flatMap {
    case Block.Paragraph(t) => Vector(t.plain)
    case Block.Bullets(items) =>
      items.flatMap(_.blocks.collect { case Block.Paragraph(t) => t.plain })
    case other => Vector(other.toString)
  }

  private val judged =
    Judged(p(0.72), p(0.55), "judge", Usage(Tokens(1), Tokens(1), Tokens.Zero, None))

  val tests = Tests {
    test(
      "a prompt links its message in its channel, says why it was picked, live's decision, the shadow's gate and answers as numbers, and the legend"
    ) {
      ReviewPrompt.doc(prompt(Reason.ShadowOnly, Below), "#standup", Link) ==> Doc(
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
          Block.Paragraph(Text.plain("Picked: only triage-v2 would draft.")),
          Block.Bullets(
            Vector(
              Item(Vector(Block.Paragraph(Text.plain("live: held: helps 0.42 under 0.50")))),
              Item(
                Vector(
                  Block.Paragraph(
                    Text.plain(
                      "triage-v2: would draft; gap: asks 0.63, maybe 0.25, none 0.13; open 0.81; to: room 0.70, person 0.30; anchor 0.55"
                    )
                  )
                )
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

    test("why a prompt was picked, and the shadow's gate, follow its reason") {
      Reason.values.toVector.map(r =>
        lines(ReviewPrompt.doc(prompt(r, Below), "#standup", Link)).slice(1, 3)
      ) ==>
        Vector(
          Vector(
            "Picked: only triage-v2 would draft.",
            "live: held: helps 0.42 under 0.50"
          ),
          Vector(
            "Picked: only live triage would draft.",
            "live: held: helps 0.42 under 0.50"
          ),
          Vector(
            "Picked: live triage and triage-v2 would both draft.",
            "live: held: helps 0.42 under 0.50"
          ),
          Vector(
            "Picked: a sample of those neither live triage nor triage-v2 would draft.",
            "live: held: helps 0.42 under 0.50"
          )
        )
      Reason.values.toVector.map(r =>
        lines(ReviewPrompt.doc(prompt(r, Below), "#standup", Link))
          .lift(3)
          .map(_.takeWhile(_ != ';'))
      ) ==> Vector(
        Some("triage-v2: would draft"),
        Some("triage-v2: would stay quiet"),
        Some("triage-v2: would draft"),
        Some("triage-v2: would stay quiet")
      )
    }

    test(
      "live's decision is told by its kind and numbers alone: no reason a model or a store wrote reaches the prompt"
    ) {
      val turn = TurnRef(ConversationId("c1"), TurnSeq.First)
      val secret = "the message said: SECRET"
      val settled = Vector(
        Settled.Drafted(Outcome.Posted(judged)),
        Settled.Drafted(Outcome.Below(judged, p(0.6))),
        Settled.Drafted(Outcome.Shadowed(judged)),
        Settled.Drafted(Outcome.Passed),
        Settled.Drafted(Outcome.NothingRecalled),
        Settled.Drafted(Outcome.Spoken(EntryId("reply:w"))),
        Settled.Drafted(Outcome.Withdrawn),
        Settled.Drafted(Outcome.Unjudged(secret)),
        Settled.Drafted(Outcome.Failed(secret)),
        Settled.Held(Silence.Chatter),
        Settled.Held(Silence.Below(p(0.42), p(0.5))),
        Settled.Held(Silence.AskedOf(PrincipalId("slack:T/U2"))),
        Settled.Held(Silence.Unanswered(turn)),
        Settled.Held(Silence.Thread(2)),
        Settled.Held(Silence.Room(3)),
        Settled.Held(Silence.Deployment(9)),
        Settled.Held(
          Silence.OverSpeechCap(
            Spend.Zero,
            DailyCap.of("1").getOrElse(throw new java.lang.AssertionError("cap"))
          )
        ),
        Settled.Held(Silence.OverBudget),
        Settled.Held(Silence.Off),
        Settled.Held(Silence.NoAddress),
        Settled.Held(Silence.Stale(2.hours)),
        Settled.Held(Silence.Unweighed(secret))
      )
      settled.map(s =>
        lines(ReviewPrompt.doc(prompt(Reason.Both, s), "#standup", Link)).lift(2)
      ) ==>
        Vector(
          "drafted; grounded 0.72, worth 0.55; posted",
          "drafted; grounded 0.72, worth 0.55; under 0.60, not posted",
          "drafted; grounded 0.72, worth 0.55; shadowed, not posted",
          "drafted; the model had nothing to add",
          "drafted; nothing recalled to ground it, not judged",
          "drafted; grit had replied already, not posted",
          "drafted; speaking stopped, not posted",
          "drafted; the judge gave no answer, not posted",
          "drafted; the turn failed before its draft was judged",
          "held: chatter",
          "held: helps 0.42 under 0.50",
          "held: names someone else",
          "held after the gate: grit's earlier draft in the thread",
          "held after the gate: the thread's rate",
          "held after the gate: the room's rate",
          "held after the gate: the deployment's rate",
          "held after the gate: the speech cap",
          "held after the gate: the budget",
          "held before the gate: speaking off",
          "held before the gate: no reply address",
          "held before the gate: stale",
          "held before the gate: untagged"
        ).map(l => Some(s"live: $l"))
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
