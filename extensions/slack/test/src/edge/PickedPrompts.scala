package grit.slack.edge

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.{EntryId, EntrySeq, PrincipalId, QuestionName, ShadowName, SourceId, TurnRef}
import grit.core.inbox.{InMemoryInbox, InboundId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.review.{Candidate, Considered, InMemoryReviews, Reason, Settled}
import grit.core.speech.{Decision, Heard, Reach, Silence}
import grit.core.store.{Origin, StoreError}
import grit.core.triage.{
  InMemoryTriageShadows,
  InMemoryTriageStore,
  Kind,
  ShadowAnswers,
  Shadowed,
  Tags
}
import grit.dbos.sql.TestTx

/** A review over `inbox`'s stores ([[reviews]]), and heard messages picked for it, as the kit
  * picks them.
  */
final class PickedPrompts(inbox: InMemoryInbox) {
  import PickedPrompts.*

  private val shadows =
    new InMemoryTriageShadows(inbox.entries, new InMemoryTriageStore(inbox.entries, inbox.periods))

  val reviews: InMemoryReviews = InMemoryReviews.over(inbox, shadows)

  private def right[A](r: Either[?, A]): A =
    r.fold(e => throw new java.lang.AssertionError(s"seeding failed: $e"), identity)

  /** Hears message `ts` of `origin`'s thread, `live` settles it, the shadow [[Shadow]] answers
    * it with [[Answers]], and it is picked as `reason`; its entry.
    */
  def pick(
      origin: Origin,
      ts: String,
      reason: Reason = Reason.ShadowOnly,
      live: Settled = Below
  ): EntryId = {
    val at = Instant.parse("2026-10-02T10:00:00Z")
    right(inbox.hear(origin, SourceId(ts), "hm", PrincipalId("slack:T/U1"), at, Reach.Nowhere))
    val conversation = right(
      inbox.conversations.all.find(_.origin == origin).toRight("no conversation")
    ).id
    val entry = InboundId.of(conversation, SourceId(ts))
    val heard = right(right(inbox.entries.get(entry)(using TestTx.fake)).toRight("no entry"))
    val turn = TurnRef(conversation, heard.turnSeq)
    given grit.core.store.Tx = TestTx.fake
    val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, None)
    val decided: Either[StoreError, Boolean] = live match {
      case Settled.Held(why) =>
        inbox.speech.decided(
          Heard(turn, EntrySeq(0), Room, at, Reach.Nowhere, Weighed),
          Decision.Held(why),
          at
        )
      case Settled.Drafted(outcome) =>
        inbox.speech
          .decided(
            Heard(turn, EntrySeq(0), Room, at, Reach.Nowhere, Weighed),
            Decision.Drafting(turn),
            at
          )
          .flatMap(_ => inbox.speech.drafted(turn, outcome, None, at))
    }
    right(decided)
    right(
      shadows.record(
        entry,
        Shadow,
        Shadowed.Answered("d1g35t", ShadowAnswers.Named(Answers), usage, "jev", "jev", 1.second),
        at
      )
    )
    right(
      reviews.considered(
        Candidate(entry, conversation, at, live, Shadow, Answers),
        Considered.Picked(reason),
        at
      )
    )
    entry
  }
}

object PickedPrompts {

  private def p(x: Double): Probability = Probability.clamped(x)

  private def named(s: String): QuestionName =
    QuestionName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  /** The shadow every pick is answered by. */
  val Shadow: ShadowName =
    ShadowName.of("triage-v2").getOrElse(throw new java.lang.AssertionError("v2"))

  /** What the shadow answered of every pick. */
  val Answers: VectorMap[QuestionName, Answer] = VectorMap(
    named("gap") -> Answer.Choice(
      "asks",
      Vector(
        Answer.Weight("asks", 0.625),
        Answer.Weight("maybe", 0.25),
        Answer.Weight("none", 0.125)
      ),
      0.4375
    ),
    named("open") -> Answer.YesNo(0.8125),
    named("to") -> Answer.Choice(
      "room",
      Vector(Answer.Weight("room", 0.7), Answer.Weight("person", 0.3)),
      0.4
    ),
    named("anchor") -> Answer.YesNo(0.55)
  )

  /** Live held it at the gate: `helps` under `helpsAt`. */
  val Below: Settled = Settled.Held(Silence.Below(p(0.42), p(0.5)))

  private val Room = Place.under(Namespace.Slack, Vector("T", "C"))

  private val Weighed =
    Tags.Weighed(
      Kind.Question,
      p(0.9),
      p(0.5),
      p(0.5),
      p(0.25),
      "jev",
      Usage(Tokens(1), Tokens(1), Tokens.Zero, None)
    )
}
