package grit.core.speech

import java.time.{Duration as JDuration, Instant}

import scala.concurrent.duration.*

import grit.core.id.{EntryId, EntrySeq, TurnRef}
import grit.core.identity.Account
import grit.core.message.{Cost, Usage}
import grit.core.occasion.Occasion
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.spend.{Budget, DailyCap, Spend, Spending}
import grit.core.store.{Entry, Origin, StoreError, Tx}
import grit.core.triage.{Cues, Gate, Reading, Tags}
import grit.core.visibility.Integrity

/** Where a reply to a heard message could go, in its edge's own address forms (`None`: it is
  * never answered, as for a past message), and whom it names besides the assistant.
  */
final case class Reach(replyTo: Option[ReplyTo], asked: Set[Account])

/** A heard message's reply addresses: in its `thread`, and at its room's top level (`room`),
  * `None` where its edge has no top level to post at; and the thread the message is in
  * (`within`), named alike for every message of that thread, `None` where its edge names none.
  */
final case class ReplyTo(thread: String, room: Option[String] = None, within: Option[String] = None)

/** Where an unprompted reply is posted: in its message's thread, or at its room's top level, at
  * `to`, the message's [[ReplyTo.room]].
  */
enum Venue {
  case Thread
  case Room(to: String)
}

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
  * conversation, its `room`, when it was `said`, its reach, triage's tags, the `integrity` its
  * heard entry is believed at, and the `cues` code read around it.
  */
final case class Heard(
    turn: TurnRef,
    seq: EntrySeq,
    room: Place,
    said: Instant,
    reach: Reach,
    tags: Tags,
    integrity: Integrity,
    cues: Cues
)

object Heard {

  /** `entry`, the heard message that is `turn`'s first entry, said in a conversation of
    * `origin`, with its `reach`, `tags` and `cues`.
    */
  def of(turn: TurnRef, entry: Entry, origin: Origin, reach: Reach, tags: Tags, cues: Cues): Heard =
    Heard(turn, entry.seq, origin.room, entry.createdAt, reach, tags, entry.integrity, cues)
}

/** The occasion that is `turn`'s first entry, as [[Speech.decide]] weighs it: at `to`, its
  * room's top-level reply address, believed at `believed`.
  */
final case class OccasionRoot(turn: TurnRef, occasion: Occasion, to: String, believed: Integrity)
    extends caps.Pure

/** Where an unprompted turn has got to. */
enum Stage {

  /** Decided on; its draft not yet settled. */
  case Drafting

  /** Its reply was posted, at position `seq` of its conversation. */
  case Posted(seq: EntrySeq)

  /** Settled without a post ([[Outcome]]). */
  case Settled
}

/** An unprompted turn decided on: its `turn`, `room`, when it was decided (`at`), its stage,
  * where its reply goes (`venue`), the thread its heard message was in ([[ReplyTo.within]]),
  * and what it began with.
  */
final case class Spoken(
    turn: TurnRef,
    room: Place,
    at: Instant,
    stage: Stage,
    venue: Venue = Venue.Thread,
    thread: Option[String] = None,
    root: Spoken.Root = Spoken.Root.Heard
)

object Spoken {

  /** What an unprompted turn began with: a heard message ([[Heard]]) or an occasion a job named
    * ([[OccasionRoot]]).
    */
  enum Root extends caps.Pure {
    case Heard, Occasion
  }
}

/** The speech ledger as a decision needs it: the unprompted turns decided within the longest
  * of the limits' windows, and what was spent today on speech (`speech`) and in all (`all`).
  */
final case class Ledger(turns: Vector[Spoken], speech: Spend, all: Spend)

object Ledger {

  /** The ledger at `now`, read from `speech` and `spending`: the turns decided since
    * `limits`' longest window began ([[Limits.from]]; none without limits), and what was spent
    * on the day `budget` says `now` falls on.
    */
  def read(
      speech: SpeechStore,
      spending: Spending,
      limits: Option[Limits],
      budget: Budget,
      now: Instant
  )(using Tx^): Either[StoreError, Ledger] = {
    val day = budget.today(now)
    for {
      spoken <- limits.fold[Either[StoreError, Vector[Spoken]]](Right(Vector.empty))(l =>
        speech.spoken(l.from(now))
      )
      spent <- speech.spentOn(day)
      all <- spending.on(day)
    } yield Ledger(spoken, spent, all)
  }
}

/** What grit decided about a heard message. */
enum Decision {

  /** Draft: `turn`, the heard message's own, runs unprompted, its reply to go at `venue`. */
  case Drafting(turn: TurnRef, venue: Venue)

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

  /** The message heard is believed at `at`, below `Outside`: a message from where anyone can
    * write does not start an unprompted reply.
    */
  case Below(at: Integrity)

  /** An occasion decided after `until`, when it lapsed. */
  case Lapsed(until: Instant)

  /** Said `age` before the decision, longer than `fresh`. */
  case Stale(age: FiniteDuration)

  /** Its author said more after it, `by`, before the decision: that later message, whose thread
    * shows this one, is weighed in its place. Never reaches the gate.
    */
  case Continued(by: EntryId)

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

  /** It names `other`, not the assistant; the first such, by id, when it names several. Kept
    * on messages decided before naming was a fact of the gate ([[grit.core.triage.Fact.Named]]).
    */
  case AskedOf(other: Account)

  /** The assistant's own reply to this message, `by`, is already in the conversation or its
    * strand when triage decides ([[grit.core.triage.Cues.spoken]]): the message was answered,
    * so no turn starts. A reply that lands while a turn drafts is its [[Outcome.Spoken]].
    */
  case Spoken(by: EntryId)

  /** `previous`, an unprompted turn in the same conversation, is drafting, or, for a heard
    * message, posted after it.
    */
  case Unanswered(previous: TurnRef)

  /** `n` occasions decided to be drafted in the room within the occasion rate's window
    * ([[Occasions.room]]), shadowed or not: its count.
    */
  case Occasions(n: Int)

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
  * the turn recalled or its thread, and that it is `worth` posting, answering the heard
  * message where the thread has not; the `model` that weighed them, and what the call consumed.
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

  /** An unprompted turn recalled nothing: the model was shown no other conversation's section,
    * no document, no record of this conversation the thread did not show whole, and none of
    * its own tool results but a failed one, so nothing grounded its draft and it was not
    * judged; or the needs gate judged that its message needs nothing recalled, so no draft was
    * made (`grit.turn.TurnNeeds`). What counts as recalled is `grit.turn.TurnJudge.state`'s.
    */
  case NothingRecalled

  /** The assistant already replied after its root (`by`, its entry), in the thread or its
    * strand, by the time the draft was decided on: not posted, so it never posts twice. Unlike
    * [[Silence.Spoken]], read as triage decides, it sees a reply that landed while the turn
    * drafted.
    */
  case Spoken(by: EntryId)

  /** The deployment stopped speaking after the draft was decided on: not posted. */
  case Withdrawn

  /** The judge gave an unprompted draft no answer, for `why`: not posted. */
  case Unjudged(why: String)

  /** An unprompted draft scored under `postAt`: not posted. */
  case Below(judged: Judged, postAt: Probability)

  /** An unprompted draft scored to post, or to be shadowed, whose turn is believed at `at`,
    * below [[Integrity.Outside]] (ADR 0036): what it read came from outside the team, such as
    * text the public typed, so it does not steer what grit says unasked. Not posted.
    */
  case Untrusted(judged: Judged, at: Integrity)

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

  /** Where `turn`'s reply goes when it is posted `at`, read from `spoken`, the turns decided
    * from `rate.per` before `at` on ([[SpeechStore.spoken]]): its venue there, unless that is
    * its room's top level and `rate.count` of that room's other turns decided within `rate.per`
    * before `at` were posted at the top, or `rate` is `None`; then its thread. A turn `spoken`
    * lacks goes in its thread.
    */
  def placing(turn: TurnRef, spoken: Vector[Spoken], rate: Option[Rate], at: Instant): Venue =
    (spoken.find(_.turn == turn).map(t => (t.room, t.venue)), rate) match {
      case (Some((room, top @ Venue.Room(_))), Some(r))
          if posted(spoken, r, at)(t => t.turn != turn && t.room == room && atTop(t)) < r.count =>
        top
      case _ => Venue.Thread
    }

  /** As [[posted]] counts, at any stage. */
  private def within(spoken: Vector[Spoken], rate: Rate, at: Instant)(
      which: Spoken => Boolean
  ): Int =
    spoken.count(t => which(t) && t.at.isAfter(at.minus(JDuration.ofNanos(rate.per.toNanos))))

  /** How many of `spoken`'s posted turns decided within `rate.per` before `at` are `which`. */
  private def posted(spoken: Vector[Spoken], rate: Rate, at: Instant)(
      which: Spoken => Boolean
  ): Int =
    within(spoken, rate, at)(t =>
      (t.stage match {
        case Stage.Posted(_) => true
        case Stage.Drafting | Stage.Settled => false
      }) && which(t)
    )

  private def atTop(t: Spoken): Boolean = t.venue match {
    case Venue.Room(_) => true
    case Venue.Thread => false
  }

  /** Whether grit drafts on `root` at `now` under `speaking`, given `ledger` and the deployment's
    * `budget`: the first failing check in [[Silence]]'s order, or a decision to speak.
    *
    * A heard message is `Drafting`, at its room's top level when [[Limits.toRoom]] places it
    * there ([[ToRoom]]), else in its thread. Fails closed: an untagged message is `Unweighed`,
    * and one whose answers the gate cannot read is `Unasked`. A message triage read as put to
    * grit ([[Tags.directed]]) is `Answering` at its thread's reply address, held only as an
    * addressed one could be: with no address, by the gate (none under [[Speaking.Off]]), or
    * over the budget.
    *
    * An occasion is held as [[unrecorded]] holds it, then by another unprompted turn drafting in
    * its conversation, the occasion rate, and the heard message's checks from the room rate on;
    * else it is `Drafting(turn, Venue.Room(to))`.
    */
  def decide(
      speaking: Speaking,
      root: Heard | OccasionRoot,
      ledger: Ledger,
      budget: Budget,
      now: Instant
  ): Decision = root match {
    case o: OccasionRoot =>
      val room = o.occasion.room
      first(
        Vector(
          () => unrecorded(speaking, o.occasion, Some(o.to), o.believed, now),
          () => unanswered(ledger, o.turn, None)
        ) ++ occasioning(speaking).toVector.flatMap((limits, rate) =>
          (() => {
            val n =
              within(ledger.turns, rate, now)(t => t.root == Spoken.Root.Occasion && t.room == room)
            Option.when(n >= rate.count)(Silence.Occasions(n))
          }) +: spent(limits, room, ledger, budget, now)
        )
      ).fold(Decision.Drafting(o.turn, Venue.Room(o.to)))(Decision.Held(_))
    case heard: Heard =>
      def here(t: Spoken): Boolean = t.turn.conversationId == heard.turn.conversationId
      // The thread rate counts the heard message's thread wherever its posts were recorded: a
      // channel's top level is one conversation of many threads (ADR 0060). Without a thread on
      // either side, the conversation.
      def inThread(t: Spoken): Boolean = (t.thread, heard.reach.replyTo.flatMap(_.within)) match {
        case (Some(theirs), Some(ours)) => theirs == ours
        case _ => here(t)
      }
      // Placement never holds: a gate that fails or is unread, a spent rate or no address is the
      // thread.
      def placed(limits: Limits): Venue =
        (limits.toRoom, heard.reach.replyTo.flatMap(_.room), heard.tags) match {
          case (Some(r), Some(to), Tags.Weighed(answers, _, _))
              if r.gate.check(answers, heard.cues) == Gate.Checked.Passes &&
                posted(ledger.turns, r.rate, now)(t => atTop(t) && t.room == heard.room) <
                r.rate.count =>
            Venue.Room(to)
          case _ => Venue.Thread
        }
      val checks: Limits => Vector[() -> Option[Silence]] = limits =>
        Vector(
          () => Option.when(heard.reach.replyTo.isEmpty)(Silence.NoAddress),
          () =>
            Option.when(heard.integrity.belowOutside)(
              Silence.Below(heard.integrity)
            ),
          () => {
            val age = JDuration.between(heard.said, now).toNanos.nanos
            Option.when(age > limits.fresh)(Silence.Stale(age))
          },
          () => heard.cues.continued.map(Silence.Continued(_)),
          () => gated(limits, heard),
          () => heard.cues.spoken.map(Silence.Spoken(_)),
          () => unanswered(ledger, heard.turn, Some(heard.seq)),
          () => {
            val n = posted(ledger.turns, limits.thread, now)(inThread)
            Option.when(n >= limits.thread.count)(Silence.Thread(n))
          }
        ) ++ spent(limits, heard.room, ledger, budget, now)
      val limited = limitsOf(speaking)
      (Tags.directed(heard.tags), heard.reach.replyTo) match {
        case (true, None) => Decision.Held(Silence.NoAddress)
        case (true, Some(to)) =>
          limited
            .flatMap(gated(_, heard))
            .orElse(Option.when(!budget.admits(ledger.all))(Silence.OverBudget))
            .fold(Decision.Answering(heard.turn, to.thread))(Decision.Held(_))
        case (false, _) =>
          limited match {
            case None => Decision.Held(Silence.Off)
            case Some(limits) =>
              first(checks(limits))
                .fold(Decision.Drafting(heard.turn, placed(limits)))(Decision.Held(_))
          }
      }
  }

  /** The decision made on `root` at `now` ([[decide]]) over the ledger `speech` and `spending`
    * hold, and whether it was kept on its turn ([[SpeechStore.decided]]): `false` when one was
    * kept before, which stands, this one kept nowhere.
    */
  def kept(
      speech: SpeechStore,
      spending: Spending,
      speaking: Speaking,
      budget: Budget,
      root: Heard | OccasionRoot,
      now: Instant
  )(using Tx^): Either[StoreError, (Decision, Boolean)] =
    for {
      ledger <- Ledger.read(speech, spending, limitsOf(speaking), budget, now)
      decision = decide(speaking, root, ledger, budget, now)
      kept <- speech.decided(root, decision, now)
    } yield (decision, kept)

  /** The first unprompted turn in `turn`'s conversation, not only its thread, drafting or posted
    * after `after`: as far as grit can tell, either answers it.
    */
  private def unanswered(ledger: Ledger, turn: TurnRef, after: Option[EntrySeq]): Option[Silence] =
    ledger.turns
      .find(t =>
        t.turn.conversationId == turn.conversationId && (t.stage match {
          case Stage.Drafting => true
          case Stage.Posted(seq) => after.exists(seq > _)
          case Stage.Settled => false
        })
      )
      .map(t => Silence.Unanswered(t.turn))

  /** Why `heard`'s tags and cues fail `limits`' gate: untagged, failing a bound, or unread;
    * `None` when they pass.
    */
  private def gated(limits: Limits, heard: Heard): Option[Silence] = heard.tags match {
    case Tags.Unanswered(why) => Some(Silence.Unweighed(why))
    case Tags.Weighed(answers, _, _) =>
      limits.drafts.check(answers, heard.cues) match {
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

  /** Why grit holds `occasion` at `now` before recording it, in this order: [[Silence.Off]]
    * without [[Limits.occasions]] (or under [[Speaking.Off]]), [[Silence.NoAddress]] when `to`,
    * its room's reply address, is `None` (none, or the room is quiet), [[Silence.Below]] when
    * it is believed below [[Integrity.Outside]], and [[Silence.Lapsed]] after its `until`;
    * `None` when none holds.
    */
  def unrecorded(
      speaking: Speaking,
      occasion: Occasion,
      to: Option[String],
      believed: Integrity,
      now: Instant
  ): Option[Silence] =
    if (occasioning(speaking).isEmpty) Some(Silence.Off)
    else if (to.isEmpty) Some(Silence.NoAddress)
    else if (believed.belowOutside) Some(Silence.Below(believed))
    else Option.when(now.isAfter(occasion.until))(Silence.Lapsed(occasion.until))

  /** The limits `speaking` drafts occasions under, and their occasion rate; `None` without
    * [[Limits.occasions]].
    */
  private def occasioning(speaking: Speaking): Option[(Limits, Rate)] =
    limitsOf(speaking).flatMap(l => l.occasions.map(o => (l, o.room)))

  /** The limits `speaking` drafts under; `None` under [[Speaking.Off]]. */
  private def limitsOf(speaking: Speaking): Option[Limits] = speaking match {
    case Speaking.Off => None
    case Speaking.Shadow(limits) => Some(limits)
    case Speaking.Within(limits) => Some(limits)
  }

  /** The checks every unprompted turn ends on, in this order: posts in `room` within the room
    * rate, posts in all within the deployment rate, today's speech spend against its cap, and
    * the deployment's budget.
    */
  private def spent(
      limits: Limits,
      room: Place,
      ledger: Ledger,
      budget: Budget,
      now: Instant
  ): Vector[() -> Option[Silence]] =
    Vector(
      () => {
        val n = posted(ledger.turns, limits.room, now)(_.room == room)
        Option.when(n >= limits.room.count)(Silence.Room(n))
      },
      () => {
        val n = posted(ledger.turns, limits.deployment, now)(_ => true)
        Option.when(n >= limits.deployment.count)(Silence.Deployment(n))
      },
      () =>
        Option.when(priced(ledger.speech.cost) >= limits.spend.usd)(
          Silence.OverSpeechCap(ledger.speech, limits.spend)
        ),
      () => Option.when(!budget.admits(ledger.all))(Silence.OverBudget)
    )

  /** The first of `checks` that holds, run in order until one does. */
  private def first(checks: Vector[() -> Option[Silence]]): Option[Silence] =
    checks.iterator.map(_()).collectFirst { case Some(s) => s }

  /** What becomes of an unprompted draft `judged`, its turn begun on `root` and believed at
    * `believed`, under `speaking`: `Posted` (or `Shadowed` under [[Speaking.Shadow]]) when its
    * score is at or above `postAt`, unless `believed` is below [[Integrity.Outside]], when it is
    * `Untrusted`; `Below` when its score is under `postAt`; `Unjudged`, with why, when it was not
    * judged; `Withdrawn` under [[Speaking.Off]], and an occasion's without [[Limits.occasions]].
    */
  def post(
      speaking: Speaking,
      root: Spoken.Root,
      judged: Either[String, Judged],
      believed: Integrity
  ): Outcome = {
    def cleared(limits: Limits, j: Judged, as: Cleared => Outcome): Outcome =
      if (!(j.score >= limits.postAt)) Outcome.Below(j, limits.postAt)
      else if (believed.belowOutside) Outcome.Untrusted(j, believed)
      else as(Cleared.Scored(j))
    val under =
      if (root == Spoken.Root.Occasion && occasioning(speaking).isEmpty) Speaking.Off else speaking
    (under, judged) match {
      case (Speaking.Off, _) => Outcome.Withdrawn
      case (_, Left(why)) => Outcome.Unjudged(why)
      case (Speaking.Shadow(limits), Right(j)) => cleared(limits, j, Outcome.Shadowed(_))
      case (Speaking.Within(limits), Right(j)) => cleared(limits, j, Outcome.Posted(_))
    }
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
