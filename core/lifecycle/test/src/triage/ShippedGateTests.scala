package grit.lifecycle.triage

import java.time.{Instant, ZoneOffset}

import grit.core.id.{ConversationId, EntrySeq, QuestionName, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.speech.{Decision, Heard, Ledger, Limits, Reach, Silence, Speaking, Speech}
import grit.core.spend.{Budget, DailyCap, Spend}
import grit.core.triage.{Gate, Kind, Reading, Tags}

import utest.*

// Speech's gate over what live triage tags a message with: a deploy changes the gate, so a
// message tagged before it is decided by the gate after.
object ShippedGateTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private val now = Instant.parse("2026-10-03T12:00:00Z")
  private val turn = TurnRef(ConversationId("c"), TurnSeq(5))
  private val cap = DailyCap.of("0.25").getOrElse(sys.error("a cap"))

  private def heard(tags: Tags) = Heard(
    turn,
    EntrySeq(10),
    Place.under(Namespace.Slack, Vector("T", "C")),
    now.minusSeconds(60),
    Reach(Some("C/1"), Set.empty),
    tags
  )

  private def tags(kind: Kind, kindP: Double, helps: Double): Tags =
    Tags.Weighed(
      kind,
      p(kindP),
      p(0.5),
      p(0.5),
      p(helps),
      "jev",
      Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
    )

  private def decide(gate: Gate, t: Tags): Decision =
    Speech.decide(
      Speaking.Within(Limits.suggested(cap, gate)),
      heard(t),
      Ledger(Vector.empty, Spend.Zero, Spend.Zero),
      Budget(ZoneOffset.UTC, None),
      now
    )

  /** Speech's triage check before the gate came from `Limits`, at Bort's `helpsAt` of 0.5:
    * chatter held, then a `helps` under 0.5 held with it; else drafted.
    */
  private def before(kind: Kind, helps: Double): Option[String] =
    if (kind == Kind.Chatter) Some("chatter")
    else Option.when(!(p(helps) >= p(0.5)))(s"below $helps")

  /** A decision in [[before]]'s terms: a hold's first failed bound, by what it reads. */
  private def after(d: Decision): Option[String] = d match {
    case Decision.Drafting(_) => None
    case Decision.Held(Silence.Gated(first, _)) =>
      first.bound match {
        case grit.core.triage.Bound.Below(Reading.Chosen(Tags.V1.kind, "chatter"), _) =>
          Some("chatter")
        case grit.core.triage.Bound.AtLeast(Reading.Yes(Tags.V1.helps), _) =>
          Some(s"below ${Probability.value(first.read)}")
        case other => Some(s"gated on $other")
      }
    case Decision.Held(other) => Some(s"held $other")
  }

  val tests = Tests {
    test("V1's gate decides every v1-tagged message as speech's chatter and helps checks did") {
      val cases = for {
        kind <- Kind.values.toVector
        kindP <- Vector(0.2, 0.5, 1.0)
        helps <- Vector(0.0, 0.25, 0.49, 0.4999, 0.5, 0.5001, 0.6, 0.9, 1.0)
      } yield (kind, kindP, helps)
      cases.map((k, kp, h) =>
        (k, kp, h) -> after(decide(TriageQuestions.V1.speak, tags(k, kp, h)))
      ) ==>
        cases.map((k, kp, h) => (k, kp, h) -> before(k, h))
    }

    test("V2's gate holds a message triaged with v1's tags unasked, on gap's asks") {
      val gap = QuestionName.of("gap").getOrElse(sys.error("a name"))
      decide(TriageQuestions.V2.speak, tags(Kind.Question, 1.0, 0.9)) ==>
        Decision.Held(Silence.Unasked(Reading.Key(gap, "asks")))
    }
  }
}
