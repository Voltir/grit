package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  EntrySeq,
  PrincipalId,
  QuestionName,
  ShadowName,
  TurnRef
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.speech.{Decision, Heard, Outcome, Reach, Silence, SpeechStore}
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  Origin,
  Payload,
  PeriodStore,
  StoreError,
  Tx
}
import grit.core.triage.{Bound, Gate, Kind, Reading, ShadowAnswers, Shadowed, Tags, TriageShadows}
import grit.core.visibility.{Compartment, Compartments, Labelled, Labeller, TestLabels, Visibility}

import utest.*

/** The contract every [[ReviewStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own
  * conversation, shadow and addresses, and reads of every review are filtered to its own.
  */
abstract class ReviewContract extends TestSuite {

  protected def entries: EntryStore

  /** The periods of those entries, whose purge deletes them. */
  protected def periods: PeriodStore

  /** Where the conversations are, whose removal takes their reviews. */
  protected def conversations: ConversationStore

  /** Where live triage's decisions are kept. */
  protected def speech: SpeechStore

  /** Where the shadows' answers are kept. */
  protected def shadows: TriageShadows

  /** The store under test, over the stores above. */
  protected def reviews: ReviewStore

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** As [[transaction]], labelling places as `visibility` does. */
  protected def transactionUnder[A](visibility: Visibility)(body: (Tx^) ?=> A): A

  /** The conversation of `origin`, created at `label` if new. */
  protected def conversation(origin: Origin, label: grit.core.visibility.Label): ConversationId

  private def conversation(origin: Origin): ConversationId =
    conversation(origin, grit.core.visibility.Label.Public)

  private val At = Instant.parse("2026-10-02T10:00:00Z")
  private val room = Place.under(Namespace.Slack, Vector("T", "C"))
  private val rater = PrincipalId("slack:T/U1")
  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, None)

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def name(s: String): ShadowName =
    ShadowName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def question(s: String): QuestionName =
    QuestionName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def p(x: Double) = Probability.clamped(x)

  /** A question set's answers, in an order that is not their names' sorted order. */
  private val answers: VectorMap[QuestionName, Answer] = VectorMap(
    question("to") -> Answer.YesNo(0.125),
    question("gap") ->
      // Its confidence as stored forms recompute it (Answer.confidence).
      Answer.Choice("asks", Vector(Answer.Weight("asks", 0.75), Answer.Weight("owes", 0.25)), 0.5)
  )

  private def answered(as: ShadowAnswers): Shadowed =
    Shadowed.Answered("d1g35t", as, usage, "jev", "jev", 1.second)

  private val below = Settled.Held(
    Silence.Gated(
      Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), p(0.5)), p(0.25)),
      Vector.empty
    )
  )
  private val passed = Settled.Drafted(Outcome.Passed)

  /** Live's decision as the turn leaves it: decided, and settled or not. */
  private enum Live {
    case Undecided
    case Unsettled
    case As(settled: Settled)
  }

  /** A heard message `id` as `c`'s next turn, said `at`, in a period opened for it as the
    * inbox does; live's decision on it as `live`; and `shadowed` kept as each shadow's row.
    */
  private def hear(
      c: ConversationId,
      id: String,
      at: Instant,
      live: Live,
      shadowed: Vector[(ShadowName, Shadowed)]
  ): EntryId = {
    val entry = EntryId(id)
    val turn = transaction {
      val n = right(entries.lockNext(c))
      right(periods.openFor(c, n.turnSeq, at))
      right(entries.insert(Entry(entry, c, n.turnSeq, None, n.seq, Payload.Heard("hm"), at)))
      TurnRef(c, n.turnSeq)
    }
    val heard = Heard(
      turn,
      EntrySeq(0),
      room,
      at,
      Reach(Some("C/1"), Set.empty),
      Tags.Weighed(Tags.V1.answers(Kind.Question, p(0.9), p(0.5), p(0.5), p(0.25)), "jev", usage)
    )
    transaction {
      live match {
        case Live.Undecided => ()
        case Live.Unsettled =>
          right(speech.decided(heard, Decision.Drafting(turn), at))
          ()
        case Live.As(Settled.Held(why)) =>
          right(speech.decided(heard, Decision.Held(why), at))
          ()
        case Live.As(Settled.Drafted(outcome)) =>
          right(speech.decided(heard, Decision.Drafting(turn), at))
          right(speech.drafted(turn, outcome, None, at))
          ()
      }
      shadowed.foreach((n, row) => right(shadows.record(entry, n, row, at)))
    }
    entry
  }

  /** `c`'s messages, `n` of them, each said a second after the last from `At`, held below
    * the gate's `helps` and answered by `shadow`: candidates.
    */
  private def candidates(c: ConversationId, prefix: String, shadow: ShadowName, n: Int) =
    (0 until n).toVector.map { i =>
      val id = hear(
        c,
        s"$prefix:$i",
        At.plusSeconds(i.toLong),
        Live.As(below),
        Vector(shadow -> answered(ShadowAnswers.Named(answers)))
      )
      Candidate(id, c, At.plusSeconds(i.toLong), below, shadow, answers)
    }

  private def mine(c: ConversationId): Vector[Reviewed] =
    transaction(right(reviews.reviewed(Instant.EPOCH))).filter(_.conversation == c)

  private def unposted(c: ConversationId): Vector[EntryId] = {
    val ids = mine(c).map(_.entry).toSet
    transaction(right(reviews.unposted(room))).map(_.entry).filter(ids.contains)
  }

  /** Where [[posting]] maps the trial's review place, and the public one. */
  private val trialPlace = Place.under(Namespace.Slack, Vector("T", "CTRIAL"))
  private val publicPlace = Place.under(Namespace.Slack, Vector("T", "CPUBLIC"))

  /** [[TestLabels.trial]] declared; [[trialPlace]] mapped at [[TestLabels.Trial]],
    * [[publicPlace]] at public, every other place unmapped.
    */
  private val posting: Visibility = {
    val rooms: Labeller[Place] = new Labeller[Place] {
      def label(item: Place): Labelled =
        if (item == trialPlace) Labelled.Mapped(TestLabels.Trial)
        else if (item == publicPlace) Labelled.Mapped(grit.core.visibility.Label.Public)
        else Labelled.Unmapped(grit.core.visibility.Label.Public)
      def requires: Vector[Compartment] = Vector(TestLabels.trial)
    }
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
  }

  val tests = Tests {
    test(
      "a message is offered, as a candidate and as a prompt, only to a place written at a " +
        "label dominating its conversation's; to an unmapped place, none"
    ) {
      val open = conversation(Origin.Task("contract", "review-to-public"))
      val trial = conversation(Origin.Task("contract", "review-to-trial"), TestLabels.Trial)
      val shadow = name("rv-to")
      val Vector(a) = candidates(open, "rv-to-public", shadow, 1): @unchecked
      val Vector(b) = candidates(trial, "rv-to-trial", shadow, 1): @unchecked
      def offered(to: Place) =
        transactionUnder(posting)(right(reviews.candidates(shadow, At, 10, to))).map(_.entry)
      val asCandidates = Vector(publicPlace, trialPlace, room).map(offered)
      transaction {
        right(reviews.considered(a, Considered.Picked(Reason.Both), At))
        right(reviews.considered(b, Considered.Picked(Reason.Both), At))
      }
      val ids = Set(a.entry, b.entry)
      def prompted(to: Place) =
        transactionUnder(posting)(right(reviews.unposted(to))).map(_.entry).filter(ids.contains)
      (asCandidates, Vector(publicPlace, trialPlace, room).map(prompted)) ==> (
        Vector(Vector(a.entry), Vector(a.entry, b.entry), Vector.empty),
        Vector(Vector(a.entry), Vector(a.entry, b.entry), Vector.empty)
      )
    }

    test(
      "a candidate is heard since, answered by the shadow as a set, its live decision " +
        "settled; the earliest said first, at most limit"
    ) {
      val c = conversation(Origin.Task("contract", "review-candidates"))
      val shadow = name("rv-candidates")
      val set = Vector(shadow -> answered(ShadowAnswers.Named(answers)))
      val late = hear(c, "rv-cand:late", At.plusSeconds(20), Live.As(below), set)
      val early = hear(c, "rv-cand:early", At.plusSeconds(10), Live.As(passed), set)
      val _ = Vector(
        hear(c, "rv-cand:unsettled", At.plusSeconds(30), Live.Unsettled, set),
        hear(c, "rv-cand:undecided", At.plusSeconds(31), Live.Undecided, set),
        hear(
          c,
          "rv-cand:other",
          At.plusSeconds(40),
          Live.As(below),
          Vector(name("rv-candidates-other") -> answered(ShadowAnswers.Named(answers)))
        ),
        hear(
          c,
          "rv-cand:failed",
          At.plusSeconds(50),
          Live.As(below),
          Vector(
            shadow -> Shadowed.Failed("f41l3d", ClassifierError.Kind.Unreadable, 1.second)
          )
        ),
        hear(
          c,
          "rv-cand:worded",
          At.plusSeconds(60),
          Live.As(below),
          Vector(shadow -> answered(ShadowAnswers.Worded(Vector(Answer.YesNo(0.5)))))
        ),
        hear(c, "rv-cand:before", At.minusSeconds(10), Live.As(below), set)
      )
      val all = Vector(
        Candidate(early, c, At.plusSeconds(10), passed, shadow, answers),
        Candidate(late, c, At.plusSeconds(20), below, shadow, answers)
      )
      transaction(
        (reviews.candidates(shadow, At, 10, room), reviews.candidates(shadow, At, 1, room))
      ) ==>
        (Right(all), Right(all.take(1)))
    }

    test(
      "a considered message is no candidate, picked, passed or unread, is considered once, " +
        "and is reviewed as it was considered"
    ) {
      val c = conversation(Origin.Task("contract", "review-considered"))
      val shadow = name("rv-considered")
      val Vector(a, b, d) = candidates(c, "rv-considered", shadow, 3): @unchecked
      val as = Vector(
        a -> Considered.Picked(Reason.LiveOnly),
        b -> Considered.Passed(Reason.Neither),
        d -> Considered.Unread
      )
      as.zipWithIndex.map { case ((cand, considered), i) =>
        transaction(reviews.considered(cand, considered, At.plusSeconds(100L - i)))
      } ==> Vector(Right(true), Right(true), Right(true))
      transaction(reviews.considered(a, Considered.Unread, At)) ==> Right(false)
      transaction(reviews.candidates(shadow, At, 10, room)) ==> Right(Vector.empty)
      // Considered latest first, so the earliest considered is the last given.
      mine(c) ==> as.zipWithIndex.reverse.map { case ((cand, considered), i) =>
        Reviewed(cand.entry, c, shadow, At.plusSeconds(100L - i), considered, None)
      }
      transaction(reviews.reviewed(At.plusSeconds(99))).map(_.filter(_.conversation == c)) ==>
        Right(
          Vector(
            Reviewed(
              b.entry,
              c,
              shadow,
              At.plusSeconds(99),
              Considered.Passed(Reason.Neither),
              None
            ),
            Reviewed(
              a.entry,
              c,
              shadow,
              At.plusSeconds(100),
              Considered.Picked(Reason.LiveOnly),
              None
            )
          )
        )
    }

    test(
      "a picked prompt is offered until posted, the earliest picked first; posted once, only " +
        "if picked, at an address no other prompt holds"
    ) {
      val origin = Origin.Task("contract", "review-posted")
      val c = conversation(origin)
      val shadow = name("rv-posted")
      val Vector(a, b, d) = candidates(c, "rv-posted", shadow, 3): @unchecked
      transaction {
        right(reviews.considered(a, Considered.Picked(Reason.LiveOnly), At.plusSeconds(2)))
        right(reviews.considered(b, Considered.Picked(Reason.ShadowOnly), At.plusSeconds(1)))
        right(reviews.considered(d, Considered.Passed(Reason.Neither), At))
      }
      val offered = transaction(right(reviews.unposted(room))).filter(_.origin == origin)
      offered ==> Vector(
        Prompt(b.entry, origin, shadow, Reason.ShadowOnly, below, answers),
        Prompt(a.entry, origin, shadow, Reason.LiveOnly, below, answers)
      )
      Vector(
        transaction(reviews.posted(d.entry, "rv-posted/d", At)),
        transaction(reviews.posted(b.entry, "rv-posted/1", At)),
        transaction(reviews.posted(b.entry, "rv-posted/2", At)),
        transaction(reviews.posted(a.entry, "rv-posted/1", At))
      ) ==> Vector(Right(false), Right(true), Right(false), Right(false))
      val before = unposted(c)
      transaction(reviews.posted(a.entry, "rv-posted/2", At)) ==> Right(true)
      (before, unposted(c)) ==> (Vector(a.entry), Vector.empty)
    }

    test(
      "a reaction keeps its verdict on the posted prompt, a later one replacing it; none at an " +
        "address no prompt holds"
    ) {
      val c = conversation(Origin.Task("contract", "review-reacted"))
      val shadow = name("rv-reacted")
      val Vector(a) = candidates(c, "rv-reacted", shadow, 1): @unchecked
      transaction {
        right(reviews.considered(a, Considered.Picked(Reason.Both), At))
        right(reviews.posted(a.entry, "rv-reacted/1", At))
      }
      Vector(
        transaction(reviews.reacted("rv-reacted/none", rater, Verdict.Welcome, At)),
        transaction(reviews.reacted("rv-reacted/1", rater, Verdict.Welcome, At.plusSeconds(1))),
        transaction(reviews.reacted("rv-reacted/1", rater, Verdict.CutIn, At.plusSeconds(2)))
      ) ==> Vector(Right(false), Right(true), Right(true))
      mine(c).map(_.label) ==> Vector(Some(Label(Verdict.CutIn, rater, At.plusSeconds(2))))
    }

    test("a withdrawal removes the label standing only when it is that verdict by that rater") {
      val c = conversation(Origin.Task("contract", "review-unreacted"))
      val shadow = name("rv-unreacted")
      val Vector(a) = candidates(c, "rv-unreacted", shadow, 1): @unchecked
      val address = "rv-unreacted/1"
      transaction {
        right(reviews.considered(a, Considered.Picked(Reason.Both), At))
        right(reviews.posted(a.entry, address, At))
        right(reviews.reacted(address, rater, Verdict.Welcome, At))
      }
      val refused = Vector(
        transaction(reviews.unreacted(address, rater, Verdict.CutIn)),
        transaction(reviews.unreacted(address, PrincipalId("slack:T/U2"), Verdict.Welcome))
      )
      val kept = mine(c).map(_.label)
      val withdrawn = transaction(reviews.unreacted(address, rater, Verdict.Welcome))
      (refused, kept, withdrawn, mine(c).map(_.label)) ==> (
        Vector(Right(false), Right(false)),
        Vector(Some(Label(Verdict.Welcome, rater, At))),
        Right(true),
        Vector(None)
      )
    }

    test(
      "a review outlives its entry's purge, its prompt no longer offered, and goes with its " +
        "conversation"
    ) {
      val c = conversation(Origin.Task("contract", "review-retention"))
      val shadow = name("rv-retention")
      val Vector(a, b) = candidates(c, "rv-retention", shadow, 2): @unchecked
      transaction {
        right(reviews.considered(a, Considered.Picked(Reason.Both), At))
        right(reviews.considered(b, Considered.Picked(Reason.ShadowOnly), At))
        right(reviews.posted(b.entry, "rv-retention/1", At))
        right(reviews.reacted("rv-retention/1", rater, Verdict.Interruption, At))
      }
      val offered = unposted(c)
      val period = right(transaction(periods.all(c))) match {
        case Vector(only) => only.ref
        case other => throw new java.lang.AssertionError(s"not one period: $other")
      }
      transaction {
        for {
          last <- entries
            .get(b.entry)
            .flatMap(
              _.toRight(StoreError.Invalid("no entry")).map(_.turnSeq)
            )
          _ <- periods.seal(
            CloseRef(period, last, At),
            CloseReason.Lapsed,
            TestClosings.prose("heard", None),
            At
          )
          _ <- periods.purge(period, At)
        } yield ()
      } ==> Right(())
      val purged = (transaction(right(entries.get(a.entry))), unposted(c), mine(c))
      transaction(conversations.remove(c)) ==> Right(())
      (offered, purged, mine(c)) ==> (
        Vector(a.entry),
        (
          None,
          Vector.empty,
          Vector(
            Reviewed(a.entry, c, shadow, At, Considered.Picked(Reason.Both), None),
            Reviewed(
              b.entry,
              c,
              shadow,
              At,
              Considered.Picked(Reason.ShadowOnly),
              Some(Label(Verdict.Interruption, rater, At))
            )
          )
        ),
        Vector.empty
      )
    }
  }
}
