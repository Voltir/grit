package grit.core.speech

import java.time.{Duration as JDuration, Instant}

import scala.concurrent.duration.*

import grit.core.id.{EntryId, EntrySeq, PrincipalId, TurnRef}
import grit.core.message.{Cost, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.spend.{Budget, DailyCap, Spend}
import grit.core.store.Entry
import grit.core.triage.{Gate, Reading, Tags}

/** Where a reply to a heard message could go, in its edge's own address form (`None`: it is
  * never answered, as for a past message), and whom it names besides the assistant.
  */
final case class Reach(replyTo: Option[String], asked: Set[PrincipalId])

object Reach {

  /** No address and nobody named: what a message heard before reach was kept has. */
  val Nowhere: Reach = Reach(None, Set.empty)
}

/** A heard message as [[Speech.decide]] weighs it: its `turn`, its position `seq` in its
  * conversation, its `room`, when it was `said`, its reach, and triage's tags.
  */
final case class Heard(
    turn: TurnRef,
    seq: EntrySeq,
    room: Place,
    said: Instant,
    reach: Reach,
    tags: Tags
)

/** Where an unprompted turn has got to. */
enum Stage {

  /** Decided on; its draft not yet settled. */
  case Drafting

  /** Its reply was posted, at position `seq` of its conversation. */
  case Posted(seq: EntrySeq)

  /** Settled without a post ([[Outcome]]). */
  case Settled
}

/** An unprompted turn decided on: its `turn`, `room`, when it was decided (`at`), and its
  * stage.
  */
final case class Spoken(turn: TurnRef, room: Place, at: Instant, stage: Stage)

/** The speech ledger as a decision needs it: the unprompted turns decided within the longest
  * of the limits' windows, and what was spent today on speech (`speech`) and in all (`all`).
  */
final case class Ledger(turns: Vector[Spoken], speech: Spend, all: Spend)

/** What grit decided about a heard message. */
enum Decision {

  /** Draft: `turn`, the heard message's own, runs. */
  case Drafting(turn: TurnRef)

  /** Stay quiet, for `why`. */
  case Held(why: Silence)
}

/** Why grit did not draft: the first check that failed, in this order. */
enum Silence {

  /** The deployment does not speak. */
  case Off

  /** The edge gave no reply address. */
  case NoAddress

  /** Said `age` before the decision, longer than `fresh`. */
  case Stale(age: FiniteDuration)

  /** Triage gave no tags, for `why`. */
  case Unweighed(why: String)

  /** Triage's answers failed the gate ([[Limits.drafts]]): `first`, the first bound they
    * failed, and `rest`, every later one, in order, each with what it read.
    */
  case Gated(first: Gate.Failed, rest: Vector[Gate.Failed])

  /** Triage's answers hold no answer of `reading`'s kind, which the gate reads before any
    * bound fails, as for a message triaged by an earlier question set.
    */
  case Unasked(reading: Reading)

  /** It names `other`, not the assistant; the first such, by id, when it names several. */
  case AskedOf(other: PrincipalId)

  /** `previous`, an unprompted turn in the same conversation, is drafting, or posted after
    * this message.
    */
  case Unanswered(previous: TurnRef)

  /** `n` posts in the conversation within the thread rate's window: its count. */
  case Thread(n: Int)

  /** `n` posts in the room within the room rate's window: its count. */
  case Room(n: Int)

  /** `n` posts in all within the deployment rate's window: its count. */
  case Deployment(n: Int)

  /** Today's speech spend, `spent`, reached `cap`. */
  case OverSpeechCap(spent: Spend, cap: DailyCap)

  /** The deployment's budget does not admit today's spend. */
  case OverBudget
}

/** The judge's scores for a draft: the probability that it is `grounded` in what the turn
  * recalled, and that it is `worth` the interruption; the `model` that weighed them, and what
  * the call consumed.
  */
final case class Judged(
    grounded: Probability,
    worth: Probability,
    model: String,
    usage: Usage
) {

  /** The weaker of the two: what `postAt` is compared with. */
  def score: Probability =
    if (Probability.value(grounded) <= Probability.value(worth)) grounded else worth
}

/** What became of an unprompted turn's draft. */
enum Outcome {

  /** The model had nothing to add. */
  case Passed

  /** The turn's window showed no record and no other conversation: nothing to ground a draft
    * in, so it was not judged.
    */
  case NothingRecalled

  /** The assistant already replied after its root (`by`, its entry), in the thread or its
    * strand: not posted, so it never posts twice.
    */
  case Spoken(by: EntryId)

  /** The deployment stopped speaking after the draft was decided on: not posted. */
  case Withdrawn

  /** The judge gave no answer, for `why`: not posted. */
  case Unjudged(why: String)

  /** Scored under `postAt`: not posted. */
  case Below(judged: Judged, postAt: Probability)

  /** Scored at or above `postAt` under [[Speaking.Shadow]]: not posted. */
  case Shadowed(judged: Judged)

  /** Scored at or above `postAt`: posted. */
  case Posted(judged: Judged)

  /** The turn failed before its draft was judged, for `why`. */
  case Failed(why: String)
}

object Speech {

  /** Whether grit drafts after `heard` at `now`, under `speaking`, given `ledger` and the
    * deployment's `budget`: the first failing check in [[Silence]]'s order, or `Drafting`.
    * Fails closed: an untagged message is `Unweighed`, and one whose answers the gate cannot
    * read is `Unasked`.
    */
  def decide(
      speaking: Speaking,
      heard: Heard,
      ledger: Ledger,
      budget: Budget,
      now: Instant
  ): Decision = {
    def within(rate: Rate, spoken: Spoken => Boolean): Int =
      ledger.turns.count(t =>
        (t.stage match {
          case Stage.Posted(_) => true
          case Stage.Drafting | Stage.Settled => false
        }) && spoken(t) && t.at.isAfter(now.minus(JDuration.ofNanos(rate.per.toNanos)))
      )
    def here(t: Spoken): Boolean = t.turn.conversationId == heard.turn.conversationId
    val checks: Limits => Vector[() => Option[Silence]] = limits =>
      Vector(
        () => Option.when(heard.reach.replyTo.isEmpty)(Silence.NoAddress),
        () => {
          val age = JDuration.between(heard.said, now).toNanos.nanos
          Option.when(age > limits.fresh)(Silence.Stale(age))
        },
        () =>
          heard.tags match {
            case Tags.Unanswered(why) => Some(Silence.Unweighed(why))
            case weighed: Tags.Weighed =>
              limits.drafts.check(Tags.V1.answers(weighed)) match {
                case Gate.Checked.Passes => None
                case Gate.Checked.Fails(first, rest) => Some(Silence.Gated(first, rest))
                case Gate.Checked.Unread(reading) => Some(Silence.Unasked(reading))
              }
          },
        () =>
          heard.reach.asked.toVector.sortBy(PrincipalId.value).headOption.map(Silence.AskedOf(_)),
        () =>
          ledger.turns
            .find(t =>
              here(t) && (t.stage match {
                case Stage.Drafting => true
                case Stage.Posted(seq) => seq > heard.seq
                case Stage.Settled => false
              })
            )
            .map(t => Silence.Unanswered(t.turn)),
        () => {
          val n = within(limits.thread, here)
          Option.when(n >= limits.thread.count)(Silence.Thread(n))
        },
        () => {
          val n = within(limits.room, _.room == heard.room)
          Option.when(n >= limits.room.count)(Silence.Room(n))
        },
        () => {
          val n = within(limits.deployment, _ => true)
          Option.when(n >= limits.deployment.count)(Silence.Deployment(n))
        },
        () =>
          Option.when(priced(ledger.speech.cost) >= limits.spend.usd)(
            Silence.OverSpeechCap(ledger.speech, limits.spend)
          ),
        () => Option.when(!budget.admits(ledger.all))(Silence.OverBudget)
      )
    val first = speaking match {
      case Speaking.Off => Some(Silence.Off)
      case Speaking.Shadow(limits) =>
        checks(limits).iterator.map(_()).collectFirst { case Some(s) => s }
      case Speaking.Within(limits) =>
        checks(limits).iterator.map(_()).collectFirst { case Some(s) => s }
    }
    first.fold(Decision.Drafting(heard.turn))(Decision.Held(_))
  }

  /** The priced part of `cost`: what a cap is compared with. */
  private def priced(cost: Cost): BigDecimal = cost match {
    case Cost.Exact(usd) => usd
    case Cost.AtLeast(usd) => usd
  }

  /** The assistant's own reply after `root`, the heard message a draft answers, as
    * `Spoken`: the first among `own`, its conversation's entries after it by position, and
    * `strand`, what its strand's other conversations said, after it by time. A person's reply
    * is not one: the judge weighs it. `None` when the assistant has not replied.
    */
  def spoken(root: Entry, own: Vector[Entry], strand: Vector[Entry]): Option[Outcome.Spoken] =
    (own.filter(_.seq > root.seq) ++ strand.filter(_.createdAt.isAfter(root.createdAt)))
      .filter(e =>
        e.payload match {
          case grit.core.store.Payload.Message(_: grit.core.message.Message.Assistant) => true
          case _ => false
        }
      )
      .minByOption(_.createdAt)
      .map(e => Outcome.Spoken(e.id))

  /** What becomes of a draft `judged` under `speaking`: `Posted` (or `Shadowed` under
    * [[Speaking.Shadow]]) when its score is at or above `postAt`, else `Below`; `Unjudged`,
    * with why, when it was not judged; `Withdrawn` under [[Speaking.Off]].
    */
  def post(speaking: Speaking, judged: Either[String, Judged]): Outcome =
    (speaking, judged) match {
      case (Speaking.Off, _) => Outcome.Withdrawn
      case (_, Left(why)) => Outcome.Unjudged(why)
      case (Speaking.Shadow(limits), Right(j)) =>
        if (j.score >= limits.postAt) Outcome.Shadowed(j) else Outcome.Below(j, limits.postAt)
      case (Speaking.Within(limits), Right(j)) =>
        if (j.score >= limits.postAt) Outcome.Posted(j) else Outcome.Below(j, limits.postAt)
    }
}
