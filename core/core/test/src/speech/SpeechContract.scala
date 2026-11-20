package grit.core.speech

import java.time.Instant

import grit.core.id.{
  CloseRef,
  ConversationId,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PluginName,
  TurnRef,
  TurnSeq
}
import grit.core.identity.TestAccounts
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.occasion.{Occasion, OccasionKey, TestOccasions}
import grit.core.period.{CloseReason, Judgement, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.spend.{Day, Spend}
import grit.core.store.{
  Entry,
  EntryStore,
  Origin,
  Payload,
  PeriodStore,
  StoreError,
  Tx,
  UsageLedger
}
import grit.core.triage.{Cues, Kind, Tags}
import grit.core.visibility.Integrity

import utest.*

/** The contract every [[SpeechStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own
  * conversation, and reads of the whole ledger are filtered to their own turns.
  */
abstract class SpeechContract extends TestSuite {

  /** The entry store the heard messages and replies are in. */
  protected def entries: EntryStore

  /** The periods of those entries, whose purge deletes them. */
  protected def periods: PeriodStore

  /** The usage ledger the unprompted turns' calls are recorded in. */
  protected def ledger: UsageLedger

  /** The speech store under test, over [[entries]] and [[ledger]]. */
  protected def speech: SpeechStore

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** The day a row [[ledger]] records now falls on. */
  /** The conversation of `origin`, created when new. */
  protected def conversationAt(origin: Origin): ConversationId

  protected def today: Day

  /** The cues [[speech]] kept with its decision on `turn`, read back as its readers read
    * them; `None` when no decision on `turn` is kept.
    */
  protected def cuesKept(turn: TurnRef): Option[Cues]

  private val At = Instant.parse("2026-09-30T10:00:00Z")
  private val room = Place.under(Namespace.Slack, Vector("T", "C"))

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def p(x: Double) = Probability.clamped(x)

  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001")))

  private val reply: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("it moved")), StopReason.EndTurn, usage, "m")

  /** An entry of `c`'s next turn holding `payload`: that turn. */
  private def next(c: ConversationId, payload: Payload, id: String): TurnRef = transaction {
    val n = right(entries.lockNext(c))
    right(
      entries.insert(Entry(EntryId(id), c, n.turnSeq, None, n.seq, payload, At, Integrity.Member))
    )
    TurnRef(c, n.turnSeq)
  }

  private def heardAs(turn: TurnRef): Heard =
    Heard(
      turn,
      EntrySeq(0),
      room,
      At,
      Reach(Some(ReplyTo("C/1")), Set.empty),
      Tags.Weighed(Tags.V1.answers(Kind.Question, p(0.9), p(0.5), p(0.5), p(0.8)), "jev", usage),
      Integrity.Member,
      Cues.Empty
    )

  /** A heard message, `id`, said at `at` as `origin`'s conversation's next turn, its reach
    * `to`: that turn.
    */
  private def heardAt(origin: Origin, at: Instant, to: ReplyTo, id: String): TurnRef = {
    val c = conversationAt(origin)
    val turn = transaction {
      val n = right(entries.lockNext(c))
      right(periods.openFor(c, n.turnSeq, at))
      right(
        entries.insert(
          Entry(EntryId(id), c, n.turnSeq, None, n.seq, Payload.Heard("hi"), at, Integrity.Member)
        )
      )
      TurnRef(c, n.turnSeq)
    }
    transaction(speech.heard(turn, Reach(Some(to), Set.empty))) ==> Right(())
    turn
  }

  /** A heard message as `c`'s next turn, decided on at `at` as `decision` does to it. */
  private def decide(
      c: ConversationId,
      at: Instant,
      drafting: Boolean,
      venue: Venue = Venue.Thread
  ): TurnRef = {
    val turn = next(c, Payload.Heard("is it Thursday?"), s"${ConversationId.value(c)}:h:$at")
    val decision =
      if (drafting) Decision.Drafting(turn, venue) else Decision.Held(Silence.Unweighed("down"))
    transaction(speech.decided(heardAs(turn), decision, at)) ==> Right(true)
    turn
  }

  /** A heard message as `c`'s next turn, decided at `at` to be answered as said to grit. */
  private def answer(c: ConversationId, at: Instant): TurnRef = {
    val turn = next(c, Payload.Heard("Pip, is it Thursday?"), s"${ConversationId.value(c)}:a:$at")
    transaction(speech.decided(heardAs(turn), Decision.Answering(turn, "C/1"), at)) ==> Right(true)
    turn
  }

  private def mine(c: ConversationId, since: Instant): Vector[Spoken] =
    transaction(right(speech.spoken(since))).filter(_.turn.conversationId == c)

  private val judged = Judged(p(0.6), p(0.8), "jev", usage)

  /** An occasion in [[room]], and the note its turn begins with. */
  private val occasion = Occasion(
    room,
    OccasionKey.of("ahead:review").getOrElse(throw new java.lang.AssertionError("key")),
    TestOccasions.Brief,
    DocumentVersionId.of(5).getOrElse(throw new java.lang.AssertionError("5")),
    At.plusSeconds(3600)
  )
  private val occasionNote = Payload.Occasion(occasion.brief, occasion.about, occasion.key)

  /** `turn`, begun on [[occasion]], as a decision weighs it. */
  private def occasionOf(turn: TurnRef): OccasionRoot =
    OccasionRoot(turn, occasion, "C//", grit.core.visibility.Integrity.Member)

  val tests = Tests {
    test("a heard message's reach is kept, the first one standing; none for another turn") {
      val c = conversation("speech-reach")
      val turn = next(c, Payload.Heard("hi"), "speech-reach:h")
      val first = Reach(Some(ReplyTo("C/1")), Set(TestAccounts.account("slack:T/U1")))
      transaction(speech.heard(turn, first)) ==> Right(())
      transaction(speech.heard(turn, Reach(None, Set.empty))) ==> Right(())
      transaction(speech.reach(turn)) ==> Right(Some(first))
      val said = next(c, Payload.Message(Message.User("hi")), "speech-reach:u")
      transaction(speech.reach(said)) ==> Right(None)
      transaction(speech.heard(said, first)) match {
        case Left(StoreError.Invalid(_)) => ()
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
    }

    test("a heard message's reach goes with its entry: none once its period is purged") {
      val c = conversation("speech-reach-purged")
      val turn = transaction {
        val n = right(entries.lockNext(c))
        right(periods.openFor(c, n.turnSeq, At))
        right(
          entries.insert(
            Entry(
              EntryId("speech-reach-purged:h"),
              c,
              n.turnSeq,
              None,
              n.seq,
              Payload.Heard("hi"),
              At,
              Integrity.Member
            )
          )
        )
        TurnRef(c, n.turnSeq)
      }
      val reach = Reach(Some(ReplyTo("C/1")), Set.empty)
      transaction(speech.heard(turn, reach)) ==> Right(())
      transaction(speech.reach(turn)) ==> Right(Some(reach))
      val period = right(transaction(periods.of(turn)))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      transaction {
        for {
          _ <- periods.seal(
            CloseRef(period, turn.turnSeq, At),
            CloseReason.Lapsed,
            TestClosings.prose("heard", None),
            At
          )
          _ <- periods.purge(period, At)
        } yield ()
      } ==> Right(())
      transaction(speech.reach(turn)) ==> Right(None)
    }

    test("a decision is kept once; only drafting ones are spoken, from a time on, oldest first") {
      val c = conversation("speech-decided")
      // Decided out of time order, so oldest first is not the order they were kept in.
      val late = decide(c, At.plusSeconds(120), drafting = true)
      val _ = decide(c, At.plusSeconds(60), drafting = false)
      val early = decide(c, At, drafting = true)
      transaction(speech.decided(heardAs(early), Decision.Held(Silence.Off), At)) ==> Right(false)
      mine(c, At) ==> Vector(
        Spoken(early, room, At, Stage.Drafting),
        Spoken(late, room, At.plusSeconds(120), Stage.Drafting)
      )
      mine(c, At.plusSeconds(61)) ==> Vector(
        Spoken(late, room, At.plusSeconds(120), Stage.Drafting)
      )
    }

    test("a decision keeps its heard message's cues, read back as written") {
      val c = conversation("speech-cues")
      val turn = next(c, Payload.Heard("is the build green?"), "speech-cues:h")
      val plain = next(c, Payload.Heard("and the docs?"), "speech-cues:p")
      val cues = Cues(
        named = true,
        spoken = Some(EntryId("speech-cues:r")),
        continued = Some(EntryId("speech-cues:n")),
        settled = Some(Judgement.Unanswered("no settle model")),
        records = Some(
          Cues.Match(PluginName.of("notes").getOrElse(sys.error("a name")), 1.5, 2)
        )
      )
      transaction(
        speech.decided(heardAs(turn).copy(cues = cues), Decision.Held(Silence.Off), At)
      ) ==> Right(true)
      transaction(
        speech.decided(heardAs(plain), Decision.Drafting(plain, Venue.Thread), At)
      ) ==> Right(true)
      (cuesKept(turn), cuesKept(plain), cuesKept(TurnRef(c, TurnSeq(99)))) ==>
        (Some(cues), Some(Cues.Empty), None)
    }

    test("a posted outcome keeps its reply's position; any other settles; each kept once") {
      val c = conversation("speech-drafted")
      val posted = decide(c, At, drafting = true)
      val below = decide(c, At.plusSeconds(1), drafting = true)
      val seq = transaction {
        val n = right(entries.lockNext(c))
        right(
          entries.insert(
            Entry(
              posted.replyId,
              c,
              posted.turnSeq,
              None,
              n.seq,
              Payload.Message(reply),
              At,
              Integrity.Member
            )
          )
        )
        n.seq
      }
      transaction(
        speech.drafted(posted, Outcome.Posted(Cleared.Scored(judged)), Some("it moved"), At)
      ) ==> Right(
        true
      )
      transaction(speech.drafted(below, Outcome.Below(judged, p(0.9)), Some("x"), At)) ==> Right(
        true
      )
      transaction(speech.drafted(below, Outcome.Passed, None, At)) ==> Right(false)
      mine(c, At).map(_.stage) ==> Vector(Stage.Posted(seq), Stage.Settled)
    }

    test("an outcome for a turn never decided on, or posted with no reply, is refused") {
      val c = conversation("speech-refused")
      val never = next(c, Payload.Heard("hi"), "speech-refused:h")
      val drafting = decide(c, At, drafting = true)
      Vector(
        transaction(speech.drafted(never, Outcome.Passed, None, At)),
        transaction(speech.drafted(drafting, Outcome.Posted(Cleared.Scored(judged)), None, At))
      ).map {
        case Left(StoreError.Invalid(_)) => "invalid"
        case other => other.toString
      } ==> Vector("invalid", "invalid")
      mine(c, At).map(_.stage) ==> Vector(Stage.Drafting)
    }

    test(
      "forget deletes a conversation's decisions on turns from..to, both included, and no other"
    ) {
      val c = conversation("speech-forget")
      val other = conversation("speech-forget-other")
      val turns = (0 to 2).toVector.map(i => decide(c, At.plusSeconds(i.toLong), drafting = true))
      val kept = decide(other, At, drafting = true)
      transaction(speech.forget(c, turns(0).turnSeq, turns(1).turnSeq)) ==> Right(())
      (mine(c, At).map(_.turn), mine(other, At).map(_.turn)) ==> (Vector(turns(2)), Vector(kept))
    }

    test(
      "a day's speech spend with a call recorded unpriced is at least what the priced ones cost"
    ) {
      val c = conversation("speech-unpriced")
      val drafting = decide(c, At, drafting = true)
      val before = transaction(right(speech.spentOn(today)))
      transaction {
        right(
          ledger.record(
            drafting.draftId,
            drafting,
            drafting.workflowId,
            "m",
            None,
            None,
            usage,
            Tokens(10)
          )
        )
        right(
          ledger.record(
            EntryId(s"judge:${drafting.workflowId}"),
            drafting,
            drafting.workflowId,
            "jev",
            None,
            None,
            usage.copy(costUsd = None),
            Tokens(10)
          )
        )
      }
      transaction(speech.spentOn(today)) ==>
        Right(before + Spend(2, Cost.AtLeast(BigDecimal("0.001"))))
    }

    test("the day's speech spend: the drafting turns' recorded calls, and no other turn's") {
      val c = conversation("speech-spent")
      val drafting = decide(c, At, drafting = true)
      val held = decide(c, At.plusSeconds(1), drafting = false)
      val before = transaction(right(speech.spentOn(today)))
      transaction {
        right(
          ledger.record(
            drafting.draftId,
            drafting,
            drafting.workflowId,
            "m",
            None,
            None,
            usage,
            Tokens(10)
          )
        )
        right(
          ledger.record(
            EntryId(s"judge:${drafting.workflowId}"),
            drafting,
            drafting.workflowId,
            "jev",
            None,
            None,
            usage,
            Tokens(10)
          )
        )
        right(
          ledger.record(held.replyId, held, held.workflowId, "m", None, None, usage, Tokens(10))
        )
      }
      transaction(speech.spentOn(today)) ==>
        Right(before + Spend(2, Cost.Exact(BigDecimal("0.002"))))
    }
    test(
      "a turn answered as said to grit is not spoken, spends no speech, and settles once"
    ) {
      val c = conversation("speech-answering")
      val answering = answer(c, At)
      val drafting = decide(c, At.plusSeconds(1), drafting = true)
      val before = transaction(right(speech.spentOn(today)))
      transaction {
        right(
          ledger.record(
            answering.replyId,
            answering,
            answering.workflowId,
            "m",
            None,
            None,
            usage,
            Tokens(10)
          )
        )
      }
      (
        transaction(speech.spentOn(today)),
        mine(c, At).map(_.turn),
        transaction(speech.answering(answering)),
        transaction(speech.answering(drafting))
      ) ==> (Right(before), Vector(drafting), Right(true), Right(false))
      transaction {
        val n = right(entries.lockNext(c))
        right(
          entries.insert(
            Entry(
              answering.replyId,
              c,
              answering.turnSeq,
              None,
              n.seq,
              Payload.Message(reply),
              At,
              Integrity.Member
            )
          )
        )
      }
      (
        transaction(speech.drafted(answering, Outcome.Replied, Some("it moved"), At)),
        transaction(speech.drafted(answering, Outcome.Failed("late"), None, At)),
        mine(c, At).map(_.turn)
      ) ==> (Right(true), Right(false), Vector(drafting))
    }

    test(
      "a draft placed at its room's top level keeps its venue until moved to its thread, kept so; any other turn's venue is the thread, and moving it changes nothing"
    ) {
      val c = conversation("speech-venue")
      val top = decide(c, At, drafting = true, Venue.Room("C//1"))
      val thread = decide(c, At.plusSeconds(1), drafting = true)
      val held = decide(c, At.plusSeconds(2), drafting = false)
      val kept = decide(c, At.plusSeconds(3), drafting = true, Venue.Room("C//3"))
      def venues = transaction(Vector(top, thread, held, kept).map(t => right(speech.venue(t))))
      val before = (venues, mine(c, At).map(_.venue))
      transaction {
        Vector(top, thread, held).foreach(t => right(speech.toThread(t)))
        right(speech.lockRoom(kept))
      }
      (before, venues, mine(c, At).map(_.venue)) ==> (
        (
          Vector(Venue.Room("C//1"), Venue.Thread, Venue.Thread, Venue.Room("C//3")),
          Vector(Venue.Room("C//1"), Venue.Thread, Venue.Room("C//3"))
        ),
        Vector(Venue.Thread, Venue.Thread, Venue.Thread, Venue.Room("C//3")),
        Vector(Venue.Thread, Venue.Thread, Venue.Room("C//3"))
      )
    }

    test(
      "addressIn is the newest reach with a room address in the room since `since`, and its top-level origin"
    ) {
      val top = Origin.Room("T", "address-newest")
      val thread = Origin.Thread("T", "address-newest", "1.0")
      val elsewhere = Origin.Room("T", "address-elsewhere")
      heardAt(thread, At, ReplyTo("C/1.0", Some("C//thread")), "address-newest:1")
      heardAt(top, At.plusSeconds(60), ReplyTo("C/2.0", Some("C//top")), "address-newest:2")
      heardAt(top, At.plusSeconds(120), ReplyTo("C/3.0", None), "address-newest:3")
      heardAt(elsewhere, At.plusSeconds(180), ReplyTo("D/1", Some("D//")), "address-elsewhere:1")
      heardAt(thread, At.plusSeconds(30), ReplyTo("C/1.0", Some("C//older")), "address-newest:4")
      transaction(speech.addressIn(top.room, At)) ==> Right(Some((top, "C//top")))
    }

    test("addressIn finds none older than since, and the top level of a thread heard since") {
      val top = Origin.Room("T", "address-since")
      val thread = Origin.Thread("T", "address-since", "1.0")
      heardAt(top, At, ReplyTo("C/1.0", Some("C//old")), "address-since:1")
      transaction(speech.addressIn(top.room, At.plusSeconds(1))) ==> Right(None)
      heardAt(thread, At.plusSeconds(5), ReplyTo("C/1.0", Some("C//new")), "address-since:2")
      transaction(speech.addressIn(top.room, At.plusSeconds(1))) ==> Right(Some((top, "C//new")))
    }

    test(
      "an occasion's decision is kept once, counts as an occasion's in the room's ledger at its venue, and is forgotten with its turns"
    ) {
      val c = conversationAt(Origin.Room("T", "occasioned"))
      val turn = next(c, occasionNote, "occasioned:o")
      val drafting = Decision.Drafting(turn, Venue.Room("C//"))
      (
        transaction(speech.decided(occasionOf(turn), drafting, At)),
        transaction(speech.decided(occasionOf(turn), Decision.Held(Silence.OverBudget), At)),
        mine(c, At),
        transaction(speech.venue(turn))
      ) ==> (
        Right(true),
        Right(false),
        Vector(
          Spoken(turn, room, At, Stage.Drafting, Venue.Room("C//"), None, Spoken.Root.Occasion)
        ),
        Right(Venue.Room("C//"))
      )
      transaction(speech.forget(c, turn.turnSeq, turn.turnSeq)) ==> Right(())
      (mine(c, At), transaction(speech.venue(turn))) ==> (Vector.empty, Right(Venue.Thread))
    }

    test("a shadowed occasion is Settled and counts in no post rate; a held one is not spoken") {
      val c = conversationAt(Origin.Room("T", "occasion-shadowed"))
      val shadowed = next(c, occasionNote, "occasion-shadowed:o1")
      val heldOne = next(c, occasionNote, "occasion-shadowed:o2")
      transaction(
        speech.decided(occasionOf(shadowed), Decision.Drafting(shadowed, Venue.Room("C//")), At)
      ) ==>
        Right(true)
      transaction(speech.decided(occasionOf(heldOne), Decision.Held(Silence.Occasions(1)), At)) ==>
        Right(true)
      transaction(
        speech.drafted(shadowed, Outcome.Shadowed(Cleared.Scored(judged)), Some("hi"), At)
      ) ==>
        Right(true)
      mine(c, At).map(s => (s.turn, s.stage, s.root)) ==> Vector(
        (shadowed, Stage.Settled, Spoken.Root.Occasion)
      )
    }

    test("a heard message's reach keeps its room address beside its thread's") {
      val c = conversation("speech-room-reach")
      val turn = next(c, Payload.Heard("heads up"), "speech-room-reach:h")
      val reach = Reach(Some(ReplyTo("C/1/1", Some("C//1"))), Set.empty)
      transaction(right(speech.heard(turn, reach)))
      transaction(speech.reach(turn)) ==> Right(Some(reach))
    }

    test(
      "a decided turn is read with the thread its heard message's reach was kept in; none when its reach names none, or none was kept"
    ) {
      val c = conversation("speech-within")
      val within = decide(c, At, drafting = true)
      val nameless = decide(c, At.plusSeconds(1), drafting = true)
      val unkept = decide(c, At.plusSeconds(2), drafting = true)
      transaction {
        right(speech.heard(within, Reach(Some(ReplyTo("C/1/2", None, Some("C/1"))), Set.empty)))
        right(speech.heard(nameless, Reach(Some(ReplyTo("C/1/3")), Set.empty)))
      }
      (
        transaction(speech.reach(within)).map(_.flatMap(_.replyTo).flatMap(_.within)),
        mine(c, At).map(t => (t.turn, t.thread))
      ) ==> (
        Right(Some("C/1")),
        Vector((within, Some("C/1")), (nameless, None), (unkept, None))
      )
    }
  }
}
