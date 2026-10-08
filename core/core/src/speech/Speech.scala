package grit.core.speech

import java.time.{Duration as JDuration, Instant}

import scala.concurrent.duration.*

import grit.core.id.{EntryId, EntrySeq, TurnRef}
import grit.core.identity.Account
import grit.core.message.{Cost, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.spend.{Budget, DailyCap, Spend}
import grit.core.store.{Entry, Tx}
import grit.core.triage.{Gate, Reading, Tags}

/** Where a reply to a heard message could go, in its edge's own address form (`None`: it is
  * never answered, as for a past message), and whom it names besides the assistant.
  */
final case class Reach(replyTo: Option[String], asked: Set[Account])

object Reach {

  /** No address and nobody named: what a message heard before reach was kept has. */
  val Nowhere: Reach = Reach(None, Set.empty)

  /** `reach` as a message heard in `room` keeps it: without its address when `room` is quiet
    * ([[Tx.quiet]]), so nothing is posted there unasked; whom it names, either way.
    */
  def heardIn(room: Place, reach: Reach)(using Tx^): Reach =
    if (Tx.quiet(room)) reach.copy(replyTo = None) else reach
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

  /** Draft: `turn`, the heard message's own, runs unprompted. */
  case Drafting(turn: TurnRef)

  /** Answer as said to grit: triage read the message as put to grit by name
    * ([[Tags.directed]]), so `turn`, its own, runs as an addressed turn does, its reply
    * awaited at `to`, the message's reply address, where the message is marked while the turn
    * runs.
    */
  case Answering(turn: TurnRef, to: String)

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

  /** The gate could not decide without `reading`, which triage's answers do not answer in
    * its kind ([[Gate.check]]), as for a message triaged by an earlier question set.
    */
  case Unasked(reading: Reading)

  /** It names `other`, not the assistant; the first such, by id, when it names several. */
  case AskedOf(other: Account)

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

/** The judge's scores for an unprompted draft: the probability that it is `grounded` in what
  * the turn recalled, and that it is `worth` the interruption; the `model` that weighed them,
  * and what the call consumed.
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

/** Why a draft went out, or under [[Speaking.Shadow]] would have. */
enum Cleared {

  /** An unprompted draft the judge scored at or above `postAt`. */
  case Scored(judged: Judged)

  /** A named turn's draft, which is not judged: drafted before a message put to grit by name
    * was answered as said to it ([[Decision.Answering]]).
    */
  case Named
}

/** What became of a turn triage started: an unprompted or named turn's draft, or a turn
  * answered as said to grit.
  */
enum Outcome {

  /** The model had nothing to add. */
  case Passed

  /** An unprompted turn's window showed no record and no other conversation: nothing to
    * ground its draft in, so it was not judged.
    */
  case NothingRecalled

  /** The assistant already replied after its root (`by`, its entry), in the thread or its
    * strand: not posted, so it never posts twice.
    */
  case Spoken(by: EntryId)

  /** The deployment stopped speaking after the draft was decided on: not posted. */
  case Withdrawn

  /** The judge gave an unprompted draft no answer, for `why`: not posted. */
  case Unjudged(why: String)

  /** An unprompted draft scored under `postAt`: not posted. */
  case Below(judged: Judged, postAt: Probability)

  /** Cleared under [[Speaking.Shadow]]: not posted. */
  case Shadowed(cleared: Cleared)

  /** Posted, as `cleared`. */
  case Posted(cleared: Cleared)

  /** Put to grit by name and answered as a message said to it: its reply is the turn's own,
    * posted by its edge. Not a post: it counts in no rate.
    */
  case Replied

  /** The turn failed before its draft or reply was settled, for `why`. */
  case Failed(why: String)
}

object Speech {

  /** Whether grit drafts after `heard` at `now`, under `speaking`, given `ledger` and the
    * deployment's `budget`: the first failing check in [[Silence]]'s order, or `Drafting`.
    * Fails closed: an untagged message is `Unweighed`, and one whose answers the gate cannot
    * read is `Unasked`. A message triage read as put to grit ([[Tags.directed]]) is
    * `Answering` at its reply address, held only as an addressed one could be: with no
    * address, by the gate (none under [[Speaking.Off]]), or over the budget.
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
        () => gated(limits, heard.tags),
        () => heard.reach.asked.toVector.sortBy(Account.written).headOption.map(Silence.AskedOf(_)),
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
    val limited = speaking match {
      case Speaking.Off => None
      case Speaking.Shadow(limits) => Some(limits)
      case Speaking.Within(limits) => Some(limits)
    }
    (Tags.directed(heard.tags), heard.reach.replyTo) match {
      case (true, None) => Decision.Held(Silence.NoAddress)
      case (true, Some(to)) =>
        limited
          .flatMap(gated(_, heard.tags))
          .orElse(Option.when(!budget.admits(ledger.all))(Silence.OverBudget))
          .fold(Decision.Answering(heard.turn, to))(Decision.Held(_))
      case (false, _) =>
        limited
          .fold(Some(Silence.Off))(checks(_).iterator.map(_()).collectFirst { case Some(s) => s })
          .fold(Decision.Drafting(heard.turn))(Decision.Held(_))
    }
  }

  /** Why `tags` fail `limits`' gate: untagged, failing a bound, or unread; `None` when they
    * pass.
    */
  private def gated(limits: Limits, tags: Tags): Option[Silence] = tags match {
    case Tags.Unanswered(why) => Some(Silence.Unweighed(why))
    case Tags.Weighed(answers, _, _) =>
      limits.drafts.check(answers) match {
        case Gate.Checked.Passes => None
        case Gate.Checked.Fails(first, rest) => Some(Silence.Gated(first, rest))
        case Gate.Checked.Unread(reading) => Some(Silence.Unasked(reading))
      }
  }

  /** The priced part of `cost`: what a cap is compared with. */
  private def priced(cost: Cost): BigDecimal = cost match {
    case Cost.Exact(usd) => usd
    case Cost.AtLeast(usd) => usd
  }

  /** The assistant's own reply after `root`, the heard message a draft answers, as
    * `Spoken`: the first among `own`, its conversation's entries after it by position, and
    * `strand`, what its strand's other conversations said, after it by time. A person's reply
    * is not one: it holds no draft (the judge sees it beside an unprompted one; a named one is
    * posted regardless). `None` when the assistant has not replied.
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

  /** What becomes of an unprompted draft `judged` under `speaking`: `Posted` (or `Shadowed`
    * under [[Speaking.Shadow]]) when its score is at or above `postAt`, else `Below`;
    * `Unjudged`, with why, when it was not judged; `Withdrawn` under [[Speaking.Off]].
    */
  def post(speaking: Speaking, judged: Either[String, Judged]): Outcome =
    (speaking, judged) match {
      case (Speaking.Off, _) => Outcome.Withdrawn
      case (_, Left(why)) => Outcome.Unjudged(why)
      case (Speaking.Shadow(limits), Right(j)) =>
        if (j.score >= limits.postAt) Outcome.Shadowed(Cleared.Scored(j))
        else Outcome.Below(j, limits.postAt)
      case (Speaking.Within(limits), Right(j)) =>
        if (j.score >= limits.postAt) Outcome.Posted(Cleared.Scored(j))
        else Outcome.Below(j, limits.postAt)
    }

  /** What becomes of a named turn's draft, which is not judged, under `speaking`: `Posted`
    * under [[Speaking.Within]], `Shadowed` under [[Speaking.Shadow]], `Withdrawn` under
    * [[Speaking.Off]]. Only for a turn drafted before a message put to grit by name was
    * answered as said to it ([[Decision.Answering]]), resumed.
    */
  def postNamed(speaking: Speaking): Outcome = speaking match {
    case Speaking.Off => Outcome.Withdrawn
    case Speaking.Shadow(_) => Outcome.Shadowed(Cleared.Named)
    case Speaking.Within(_) => Outcome.Posted(Cleared.Named)
  }

}
