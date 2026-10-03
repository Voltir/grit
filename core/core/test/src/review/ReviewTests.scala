package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.{
  ConversationId,
  EntryId,
  PrincipalId,
  QuestionName,
  ShadowName,
  TurnRef,
  TurnSeq
}
import grit.core.message.Cost
import grit.core.period.Probability
import grit.core.speech.{Outcome, Silence}
import grit.core.spend.{DailyCap, Spend}
import grit.core.triage.{Bound, Gate, Reading, Tags}

import utest.*

object ReviewTests extends TestSuite {

  private val At = Instant.parse("2026-10-02T09:00:00Z")
  private val shadow = ShadowName.of("v2").getOrElse(sys.error("a name"))
  private val ask = QuestionName.of("ask").getOrElse(sys.error("a name"))

  /** The shadow's gate in these tests: drafts when `ask` reads yes at least 0.5. */
  private val drafts: VectorMap[QuestionName, Answer] -> Option[Boolean] =
    answers => answers.get(ask).collect { case Answer.YesNo(p) => p >= 0.5 }

  private def reviewing(perDay: Int, sampleOneIn: Int = 1): Reviewing =
    Reviewing.of(shadow, perDay, sampleOneIn, 24.hours).getOrElse(sys.error("a review"))

  private val drafted = Settled.Drafted(Outcome.Passed)
  private val chatter = Settled.Held(
    Silence.Gated(
      Gate.Failed(
        Bound.Below(Reading.Chosen(Tags.V1.kind, "chatter"), Probability.clamped(0.5)),
        Probability.clamped(1)
      ),
      Vector.empty
    )
  )

  /** A candidate `id`, said `second`s after `At`, live's decision `live`, the shadow answering
    * `ask` with `yes` (no answer when `None`).
    */
  private def candidate(id: String, second: Int, live: Settled, yes: Option[Double]): Candidate =
    Candidate(
      EntryId(id),
      ConversationId("c"),
      At.plusSeconds(second.toLong),
      live,
      shadow,
      VectorMap.from(yes.map(p => ask -> Answer.YesNo(p)))
    )

  private def picked(round: Vector[(Candidate, Considered)]): Vector[(String, Considered)] =
    round.map((c, as) => (EntryId.value(c.entry), as))

  val tests = Tests {
    test("a verdict: welcome alone says speak, cut-in alone says the message was to a person") {
      Verdict.values.toVector.map(v => (v, v.speaks, v.toPerson)) ==> Vector(
        (Verdict.Welcome, true, false),
        (Verdict.Interruption, false, false),
        (Verdict.CutIn, false, true)
      )
    }

    test("live's gate: passed by a draft or a hold after it, failed at it, else never reached") {
      val p = Probability.clamped(0.4)
      val cap = DailyCap.of("1").getOrElse(sys.error("a cap"))
      val held = Vector(
        Silence.Off,
        Silence.NoAddress,
        Silence.Stale(2.days),
        Silence.Unweighed("down"),
        Silence.Gated(Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), p), p), Vector.empty),
        Silence.Unasked(Reading.Yes(Tags.V1.helps)),
        Silence.AskedOf(PrincipalId("slack:T/U1")),
        Silence.Unanswered(TurnRef(ConversationId("c"), TurnSeq(1))),
        Silence.Thread(1),
        Silence.Room(1),
        Silence.Deployment(1),
        Silence.OverSpeechCap(Spend(1, Cost.Exact(BigDecimal(1))), cap),
        Silence.OverBudget
      )
      (Review.gated(drafted) +: held.map(s => Review.gated(Settled.Held(s)))) ==> Vector(
        Some(true),
        None,
        None,
        None,
        None,
        Some(false),
        None,
        Some(false),
        Some(true),
        Some(true),
        Some(true),
        Some(true),
        Some(true),
        Some(true)
      )
    }

    test("every ShadowOnly and Both is picked, past perDay and the day's earlier picks") {
      val round = Review.pick(
        Vector(
          candidate("s1", 0, chatter, Some(0.9)),
          candidate("b1", 1, drafted, Some(0.9)),
          candidate("s2", 2, chatter, Some(0.8)),
          candidate("b2", 3, drafted, Some(0.7))
        ),
        drafts,
        Vector(Reason.LiveOnly, Reason.Neither, Reason.ShadowOnly),
        reviewing(perDay = 1)
      )
      picked(round) ==> Vector(
        "s1" -> Considered.Picked(Reason.ShadowOnly),
        "b1" -> Considered.Picked(Reason.Both),
        "s2" -> Considered.Picked(Reason.ShadowOnly),
        "b2" -> Considered.Picked(Reason.Both)
      )
    }

    test(
      "LiveOnly and Neither share perDay, LiveOnly's half rounding up, less the day's earlier " +
        "picks, each its earliest said"
    ) {
      // Out of said order, so the earliest is not the first given.
      val candidates = Vector(
        candidate("l3", 30, drafted, Some(0.1)),
        candidate("n2", 20, chatter, Some(0.1)),
        candidate("l1", 10, drafted, Some(0.1)),
        candidate("n1", 15, chatter, Some(0.1)),
        candidate("l2", 25, drafted, Some(0.1))
      )
      val fresh = Review.pick(candidates, drafts, Vector.empty, reviewing(perDay = 3))
      val later = Review.pick(candidates, drafts, Vector(Reason.LiveOnly), reviewing(perDay = 3))
      (picked(fresh), picked(later)) ==> (
        Vector(
          "l3" -> Considered.Passed(Reason.LiveOnly),
          "n2" -> Considered.Passed(Reason.Neither),
          "l1" -> Considered.Picked(Reason.LiveOnly),
          "n1" -> Considered.Picked(Reason.Neither),
          "l2" -> Considered.Picked(Reason.LiveOnly)
        ),
        Vector(
          "l3" -> Considered.Passed(Reason.LiveOnly),
          "n2" -> Considered.Passed(Reason.Neither),
          "l1" -> Considered.Picked(Reason.LiveOnly),
          "n1" -> Considered.Picked(Reason.Neither),
          "l2" -> Considered.Passed(Reason.LiveOnly)
        )
      )
    }

    test("a Neither is picked only in the sample, by its entry id's hash") {
      // Pinned: which messages a past day sampled is what a weight recovered from the
      // recorded candidates divides by, so the hash must not move. At one in 3, n-2, n-6 and
      // n-10 are in it (the first 16 hex digits of SHA-256, unsigned, mod 3).
      val round = Review.pick(
        (0 to 11).toVector.map(i => candidate(s"n-$i", i, chatter, Some(0.1))),
        drafts,
        Vector.empty,
        reviewing(perDay = 24, sampleOneIn = 3)
      )
      picked(round).collect { case (id, Considered.Picked(_)) => id } ==>
        Vector("n-2", "n-6", "n-10")
    }

    test(
      "a candidate live's gate never reached, or whose answers the gate cannot read, is Unread"
    ) {
      val round = Review.pick(
        Vector(
          candidate("off", 0, Settled.Held(Silence.Off), Some(0.9)),
          candidate("blank", 1, drafted, None)
        ),
        drafts,
        Vector.empty,
        reviewing(perDay = 4)
      )
      picked(round) ==> Vector("off" -> Considered.Unread, "blank" -> Considered.Unread)
    }

    test("a review needs perDay and sampleOneIn at least 1 and a positive window") {
      Vector(
        Reviewing.of(shadow, 0, 1, 1.hour),
        Reviewing.of(shadow, 1, 0, 1.hour),
        Reviewing.of(shadow, 1, 1, Duration.Zero),
        Reviewing.of(shadow, 1, 1, 1.hour)
      ).map(_.map(r => (r.perDay, r.sampleOneIn, r.within))) ==>
        Vector(None, None, None, Some((1, 1, 1.hour)))
    }
  }
}
