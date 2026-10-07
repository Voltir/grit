package grit.kit.run

import java.time.{Instant, ZoneId}

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.DurationInt

import grit.core.classify.Answer
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  PrincipalId,
  QuestionName,
  ShadowName,
  TurnRef
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.review.{Considered, InMemoryReviews, Reason, Reviewing, Settled}
import grit.core.speech.{
  Decision,
  Heard,
  InMemorySpeechStore,
  Limits,
  Outcome,
  Reach,
  Silence,
  Speaking
}
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryUsageLedger,
  Jot,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.triage.{
  Bound,
  Gate,
  InMemoryTriageShadows,
  InMemoryTriageStore,
  Kind,
  Reading,
  ShadowAnswers,
  Shadowed,
  Tags
}
import grit.core.visibility.{Label, Subject}
import grit.dbos.sql.TestTx
import grit.kit.deployment.{Deployments, ShadowReview}
import grit.lifecycle.shadow.ShadowVariant
import grit.lifecycle.triage.TriageQuestions

import utest.*

/** [[Picking.round]] over the in-memory review store, the shadow asking
  * [[TriageQuestions.V2]], at fixed instants.
  */
object PickingTests extends TestSuite {

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Midnight in Los Angeles is 07:00 UTC in October. */
  private val budget = Budget(ZoneId.of("America/Los_Angeles"), None)
  private val shadow = ShadowName.of("v2").getOrElse(sys.error("a name"))
  private val room = Place.under(Namespace.Slack, Vector("T", "C"))
  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, None)
  private def p(x: Double) = Probability.clamped(x)
  private def q(s: String) = QuestionName.of(s).getOrElse(sys.error(s))

  /** `perDay` a day, every message both gates leave quiet in the sample, said within a day. */
  private def review(perDay: Int): ShadowReview = {
    val limits =
      Limits.suggested(
        DailyCap.of("0.25").getOrElse(sys.error("a cap")),
        TriageQuestions.ShippedSpeak
      )
    val variant = ShadowVariant(
      shadow,
      TriageQuestions.V2,
      None,
      DailyCap.of("0.01").getOrElse(sys.error("a cap")),
      Instant.EPOCH
    )
    Deployments
      .of(
        edges = Vector(Deployments.edge("slack", asks = false, reviews = Some(room))),
        speaking = Speaking.Shadow(limits),
        shadows = Vector(variant),
        review = Reviewing.of(shadow, perDay, 1, 24.hours)
      )
      .toOption
      .flatMap(_.review)
      .getOrElse(sys.error("a review"))
  }

  /** V2's answers, drafting when `drafts`. */
  private def answers(drafts: Boolean): VectorMap[QuestionName, Answer] = {
    val asks = if (drafts) 0.9 else 0.1
    VectorMap(
      q("gap") -> Answer.Choice(
        "asks",
        Vector(Answer.Weight("asks", asks), Answer.Weight("nothing", 1 - asks)),
        0.5
      ),
      q("open") -> Answer.YesNo(0.9),
      q("to") -> Answer.YesNo(0.1),
      q("anchor") -> Answer.YesNo(0.1)
    )
  }

  private val gatePasses = Settled.Drafted(Outcome.Passed)
  private val belowHelps = Settled.Held(
    Silence.Gated(
      Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), p(0.5)), p(0.25)),
      Vector.empty
    )
  )
  private val noAddress = Settled.Held(Silence.NoAddress)

  /** The stores a round reads and writes, and a way to hear into them. */
  private final class World {
    private val entries = new InMemoryEntryStore
    private val speech = new InMemorySpeechStore(entries, new InMemoryUsageLedger)
    private val conversations = new InMemoryConversationStore(Some(entries))
    private val periods = new InMemoryPeriodStore(entries)
    private val shadows =
      new InMemoryTriageShadows(entries, new InMemoryTriageStore(entries, periods))
    val reviews = new InMemoryReviews(entries, conversations, speech, shadows)

    private def right[A](r: Either[StoreError, A]): A = r.fold(e => sys.error(s"$e"), identity)

    private val conversation: ConversationId = {
      given Tx = TestTx.fake
      right(
        conversations.findOrCreate(Origin.Task("kit", "picking"), PrincipalId.Local, Label.Public)
      ).id
    }

    /** A heard message `id`, said `at`, live's decision on it settled as `live`, and the
      * shadow's answers drafting when `drafts`.
      */
    def hear(id: String, at: Instant, live: Settled, drafts: Boolean): EntryId = {
      given Tx = TestTx.fake
      val entry = EntryId(id)
      val n = right(entries.lockNext(conversation))
      right(periods.openFor(conversation, n.turnSeq, at))
      right(
        entries.insert(Entry(entry, conversation, n.turnSeq, None, n.seq, Payload.Heard("hm"), at))
      )
      val turn = TurnRef(conversation, n.turnSeq)
      val heard = Heard(
        turn,
        EntrySeq(0),
        room,
        at,
        Reach(Some("C/1"), Set.empty),
        Tags.Weighed(Tags.V1.answers(Kind.Question, p(0.9), p(0.5), p(0.5), p(0.25)), "jev", usage)
      )
      live match {
        case Settled.Held(why) => right(speech.decided(heard, Decision.Held(why), at))
        case Settled.Drafted(outcome) =>
          right(speech.decided(heard, Decision.Drafting(turn), at))
          right(speech.drafted(turn, outcome, None, at))
      }
      right(
        shadows.record(
          entry,
          shadow,
          Shadowed.Answered(
            "d1g35t",
            ShadowAnswers.Named(answers(drafts)),
            usage,
            "jev",
            "jev",
            1.second
          ),
          at
        )
      )
      entry
    }

    def round(review: ShadowReview, now: Instant): Either[StoreError, Int] =
      Picking.round(review, reviews, FakeJot, budget, now)

    /** What every round so far made of each message considered. */
    def considered: Map[EntryId, Considered] = {
      given Tx = TestTx.fake
      right(reviews.reviewed(Instant.EPOCH)).map(r => r.entry -> r.as).toMap
    }
  }

  private val Late = Instant.parse("2026-10-02T06:30:00Z") // 23:30 on the 1st in Los Angeles

  val tests = Tests {
    test(
      "a day's share is less what earlier rounds picked that day, the day beginning at " +
        "midnight in the budget's zone"
    ) {
      val w = new World
      val first = w.hear("first", Late.minusSeconds(600), gatePasses, drafts = false)
      val firstRound = w.round(review(2), Late)
      val second = w.hear("second", Late.plusSeconds(600), gatePasses, drafts = false)
      val secondRound = w.round(review(2), Late.plusSeconds(1200))
      // 07:10 UTC: ten past midnight on the 2nd in Los Angeles, the same UTC day.
      val third = w.hear("third", Late.plusSeconds(1800), gatePasses, drafts = false)
      val thirdRound = w.round(review(2), Late.plusSeconds(2400))
      (Vector(firstRound, secondRound, thirdRound), w.considered) ==> (
        Vector(Right(1), Right(0), Right(1)),
        Map(
          first -> Considered.Picked(Reason.LiveOnly),
          second -> Considered.Passed(Reason.LiveOnly),
          third -> Considered.Picked(Reason.LiveOnly)
        )
      )
    }

    test(
      "every candidate is kept as considered: shadow-only and both picked past the day's " +
        "share, one live's gate never reached as unread"
    ) {
      val w = new World
      val shadowOnly =
        (0 until 3).map(i => w.hear(s"shadow:$i", Late.minusSeconds(60L - i), belowHelps, true))
      val both = w.hear("both", Late.minusSeconds(50), gatePasses, drafts = true)
      val neither = w.hear("neither", Late.minusSeconds(40), belowHelps, drafts = false)
      val unread = w.hear("unread", Late.minusSeconds(30), noAddress, drafts = true)
      (w.round(review(1), Late), w.considered) ==> (
        Right(4),
        shadowOnly.map(_ -> Considered.Picked(Reason.ShadowOnly)).toMap ++ Map(
          both -> Considered.Picked(Reason.Both),
          neither -> Considered.Passed(Reason.Neither),
          unread -> Considered.Unread
        )
      )
    }

    test("a round rerun at the same instant picks nothing and keeps nothing more") {
      val w = new World
      val _ = w.hear("once", Late.minusSeconds(60), belowHelps, drafts = true)
      val first = w.round(review(2), Late)
      val kept = w.considered
      (first, w.round(review(2), Late), w.considered) ==> (Right(1), Right(0), kept)
    }

    test("a message said before the review's within reaches back is not considered") {
      val w = new World
      val old = w.hear("old", Late.minusSeconds(24 * 3600 + 1), belowHelps, drafts = true)
      val edge = w.hear("edge", Late.minusSeconds(24 * 3600), belowHelps, drafts = true)
      (w.round(review(2), Late), w.considered.get(old), w.considered.get(edge)) ==> (
        Right(1),
        None,
        Some(Considered.Picked(Reason.ShadowOnly))
      )
    }
  }
}
