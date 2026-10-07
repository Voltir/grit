package grit.core.inbox

import java.time.{Instant, ZoneOffset}

import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  ScheduleId,
  ScheduleKey,
  SourceId,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Account, TestAccounts}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.hour
import grit.core.job.{Declared, Ending, Schedule, Slot, SlotRule}
import grit.core.message.{Cost, Message}
import grit.core.period.{Period, PeriodState}
import grit.core.speech.Reach
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Origin, Payload}
import grit.core.visibility.{
  Compartments,
  Label,
  Labelled,
  Level,
  RoomLabels,
  TestLabels,
  Visibility
}

import utest.*

/** What every [[Inbox]] keeps of recording messages and reading a turn's progress, run
  * against the in-memory fake in core and SqlInbox in grit.dbos.
  */
abstract class InboxContract extends TestSuite {

  /** Runs `body` over a store holding nothing from the origins these tests use, with its
    * inbox, taking new messages as `budget` allows and labelling rooms as `visibility` does,
    * and the [[InboxContract.Store]] under it.
    */
  protected def withInbox[A](budget: Budget, visibility: Visibility)(
      body: (Inbox, InboxContract.Store^) => A
  ): A

  /** [[withInbox]] under the shipped visibility: every room public. */
  private def withInbox[A](budget: Budget)(body: (Inbox, InboxContract.Store^) => A): A =
    withInbox(budget, Visibility.Shipped)(body)

  private val Uncapped = Budget(ZoneOffset.UTC, None)

  private def said(text: String): Message.User = Message.User(text)

  /** When the heard messages below were said, days before any test runs. */
  private val Said = Instant.parse("2026-09-26T10:00:00Z")

  /** When the posts below were made, before anything was said under them. */
  private val PostedAt = Instant.parse("2026-09-25T09:00:00Z")

  /** The call the posts below were made by: one of another conversation's turns. */
  private val Asking: CallSlot =
    CallSlot
      .of(TurnRef(ConversationId("0190a000-0000-7000-8000-000000000001"), TurnSeq.First), 0, 1)
      .getOrElse(throw new java.lang.AssertionError("a slot at 0, 1 reads"))

  private val remind = new Counting("remind")

  /** A declared schedule of `remind`, keyed `key`, its slots `rule`. */
  private def declared(key: String, rule: SlotRule): (Declarer, Declared[Count]) =
    (
      Declarer.Deployment,
      Declared(ScheduleKey.of(key).fold(sys.error, identity), remind, rule, Count(1))
    )

  private def scheduled(key: String): ScheduleId =
    ScheduleId.declared(Declarer.Deployment, ScheduleKey.of(key).fold(sys.error, identity))

  /** When the slots below are due. */
  private val Due = Instant.parse("2026-10-07T09:00:00Z")

  /** A turn `started` names, and the slot. */
  private def begun(started: Either[InboxError, Slotted]): (TurnRef, Slot) =
    started match {
      case Right(Slotted.Started(turn, slot)) => (turn, slot)
      case other => throw new java.lang.AssertionError(s"not started: $other")
    }

  val tests = Tests {
    test(
      "a due slot starts as its job's run: the first turn of its slot's conversation, opened with its opening, at that version's source; one not due, or never declared, is idle"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val now = Due.plusSeconds(60)
        store.declare(
          Vector(
            declared("start-due", SlotRule.Once(Due, hour)),
            declared("start-later", SlotRule.Once(Due.plusSeconds(3600), hour))
          ),
          Due
        )
        val slot = Slot(scheduled("start-due"), Due)
        val origin = slot.origin(remind.name)
        Vector("start-later", "never").map(k => inbox.startSlot(scheduled(k), Some(1), now)) ==>
          Vector(Right(Slotted.Idle), Right(Slotted.Idle))
        val (turn, started) = begun(inbox.startSlot(slot.schedule, Some(1), now))
        (started, turn.turnSeq) ==> (slot, TurnSeq.First)
        inbox.ingested(origin, SourceId("v1")) ==> Right(Some(turn))
        store.written(origin) ==> Vector(
          (Payload.Message(Message.User("Scheduled run of remind, due 2026-10-07 09:00 UTC")), None)
        )
        store.schedule(slot.schedule).map(_.ended) ==> Some(None)
      }
    }

    test(
      "a once slot more than its grace past is missed, with or without its job, and its schedule ends missed, never run"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        store.declare(
          Vector(
            declared("missed-job", SlotRule.Once(Due, hour)),
            declared("missed-jobless", SlotRule.Once(Due, hour))
          ),
          Due
        )
        val late = Due.plusSeconds(3601)
        val keys = Vector("missed-job", "missed-jobless")
        keys.zip(Vector(Some(1), None)).map((k, v) => inbox.startSlot(scheduled(k), v, late)) ==>
          keys.map(k => Right(Slotted.Missed(Slot(scheduled(k), Due))))
        keys.map(k => store.schedule(scheduled(k)).flatMap(_.ended)) ==>
          Vector(Some(Ending.Missed), Some(Ending.Missed))
        keys.map(k => store.exists(Slot(scheduled(k), Due).origin(remind.name))) ==>
          Vector(false, false)
      }
    }

    test(
      "a run that ended without a reply fails: a once schedule ends failed, a recurrence goes on to its next slot"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val daily = SlotRule.Daily(java.time.LocalTime.of(9, 0), ZoneOffset.UTC)
        store.declare(
          Vector(declared("fail-once", SlotRule.Once(Due, hour)), declared("fail-daily", daily)),
          Due.minusSeconds(60)
        )
        val keys = Vector("fail-once", "fail-daily")
        val turns = keys.map(k => begun(inbox.startSlot(scheduled(k), Some(1), Due))._1)
        turns.foreach(store.end)
        val after = Due.plusSeconds(60)
        keys.map(k => inbox.startSlot(scheduled(k), Some(1), after)) ==>
          keys.map(k => Right(Slotted.Failed(Slot(scheduled(k), Due))))
        keys.map(k => store.schedule(scheduled(k)).flatMap(_.ended)) ==>
          Vector(Some(Ending.Failed), None)
        val mine = keys.map(scheduled).toSet
        (
          store.waiting(after).filter(mine),
          store.waiting(Due.plusSeconds(86400)).filter(mine)
        ) ==> (Vector(), Vector(scheduled("fail-daily")))
      }
    }

    test(
      "a once schedule declared again after its run replied while undeclared ends ran, its slot never run again"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val slot = Slot(scheduled("revived"), Due)
        store.declare(Vector(declared("revived", SlotRule.Once(Due, hour))), Due)
        val _ = begun(inbox.startSlot(slot.schedule, Some(1), Due))
        store.declare(Vector(), Due.plusSeconds(1))
        store.replied(slot, 1, Due.plusSeconds(1))
        store.declare(Vector(declared("revived", SlotRule.Once(Due, hour))), Due.plusSeconds(2))
        inbox.startSlot(slot.schedule, Some(1), Due.plusSeconds(3)) ==> Right(Slotted.Ran(slot))
        store.schedule(slot.schedule).flatMap(_.ended) ==> Some(Ending.Ran)
        inbox.startSlot(slot.schedule, Some(1), Due.plusSeconds(4)) ==> Right(Slotted.Idle)
        store.written(slot.origin(remind.name)).size ==> 1
      }
    }

    test(
      "a slot's run is never started as a turn: startTurn refuses it, naming its slot, while a one-shot run's turn starts"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        store.declare(Vector(declared("start-turn", SlotRule.Once(Due, hour))), Due)
        val (run, slot) = begun(inbox.startSlot(scheduled("start-turn"), Some(1), Due))
        val oneShot = inbox.ingest(
          Origin.Task("remind", "main"),
          SourceId("m1"),
          said("once"),
          Account.Local
        )
        (oneShot.flatMap(inbox.startTurn), inbox.startTurn(run)) ==>
          (Right(()), Left(InboxError.SlotRun(slot)))
      }
    }

    test("ingested: the turn a message was recorded as; none for one never recorded") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "ingested")
        val there = Origin.Task("inbox", "elsewhere")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), Account.Local)
        inbox.ingested(here, SourceId("m1")) ==> turn.map(Some(_))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        inbox.ingested(there, SourceId("m1")) ==> Right(None)
      }
    }

    test("recorded names the sources heard or ingested from an origin, and no others") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "recorded")
        val there = Origin.Task("inbox", "recorded-elsewhere")
        val asked = Set(SourceId("m1"), SourceId("m2"), SourceId("m3"))
        inbox.recorded(here, asked) ==> Right(Set.empty)
        inbox.ingest(here, SourceId("m1"), said("one"), Account.Local)
        inbox.hear(here, SourceId("m2"), "two", Account.Local, Said, Reach.Nowhere) ==> Right(
          ()
        )
        inbox.hear(
          there,
          SourceId("m3"),
          "three",
          Account.Local,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        inbox.recorded(here, asked) ==> Right(Set(SourceId("m1"), SourceId("m2")))
      }
    }

    test("ingest: the same source is the same turn, a new one the next turn") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "same-source")
        val first = inbox.ingest(here, SourceId("m1"), said("one"), Account.Local)
        val again = inbox.ingest(here, SourceId("m1"), said("one, redelivered"), Account.Local)
        val second = inbox.ingest(here, SourceId("m2"), said("two"), Account.Local)
        (first.map(_.turnSeq), again, second.map(_.turnSeq)) ==>
          (Right(TurnSeq.First), first, Right(TurnSeq.First.next))
        store.written(here).map(_._1) ==> Vector(
          Payload.Message(said("one")),
          Payload.Message(said("two"))
        )
      }
    }

    test("ingest opens a conversation's first period, and after a close the next, at its turn") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "periods")
        val one = inbox.ingest(here, SourceId("p1"), said("one"), Account.Local)
        store.periods(here).map(p => (p.first, p.state)) ==>
          Vector((TurnSeq.First, PeriodState.Open))
        one.foreach(store.close)
        val two = inbox.ingest(here, SourceId("p2"), said("two"), Account.Local)
        two.map(_.turnSeq) ==> Right(TurnSeq.First.next)
        store.periods(here).map(p => (p.first, p.state == PeriodState.Open)) ==>
          Vector((TurnSeq.First, false), (TurnSeq.First.next, true))
      }
    }

    test("an ingested message records who wrote it, and a redelivery by another keeps the first") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "authors")
        val ana = TestAccounts.account("task:ana")
        val bo = TestAccounts.account("task:bo")
        store.name(ana, "Ana")
        store.name(bo, "Bo")
        inbox.ingest(here, SourceId("a1"), said("one"), ana)
        inbox.ingest(here, SourceId("a1"), said("one"), bo)
        store.written(here) ==> Vector((Payload.Message(said("one")), Some("Ana")))
      }
    }

    test("a turn recorded but never started is Open") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "open")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), Account.Local)
        turn.flatMap(inbox.progress) ==> Right(Progress.Open)
      }
    }

    test(
      "a heard message is recorded once, as heard, under its author's name, and is no turn ingested"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "heard")
        val ana = TestAccounts.account("task:ana")
        store.name(ana, "Ana")
        inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          ana,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          ana,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        store.written(here) ==> Vector((Payload.Heard("standup moves to 10:00"), Some("Ana")))
        inbox.ingested(here, SourceId("m1")) ==> Right(None)
      }
    }

    test("a heard message keeps the reach it was heard with; heard again, the first stands") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "reach")
        val first = Reach(Some("C/1"), Set(TestAccounts.account("task:ben")))
        inbox.hear(here, SourceId("m1"), "ask Ben", Account.Local, Said, first) ==> Right(())
        inbox.hear(here, SourceId("m1"), "ask Ben", Account.Local, Said, Reach.Nowhere) ==>
          Right(())
        inbox.ingest(here, SourceId("m2"), said("hi"), Account.Local).map(_ => ()) ==> Right(())
        store.reached(here) ==> Vector(Some(first), None)
      }
    }

    test("a heard message's entry, and the period it opens, are dated when it was said") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "dated")
        val later = Said.plusSeconds(300)
        inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          Account.Local,
          Said,
          Reach.Nowhere
        ) ==>
          Right(())
        inbox.hear(
          here,
          SourceId("m2"),
          "fine by me",
          Account.Local,
          later,
          Reach.Nowhere
        ) ==> Right(())
        (store.dated(here), store.periods(here).map(_.openedAt)) ==> (
          Vector(Said, later),
          Vector(Said)
        )
      }
    }

    test(
      "a post begins a new conversation as its first entry, grit's own, dated when it was posted, opening its period then, made by the call it names; its source addresses nothing"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "posted")
        val ana = TestAccounts.account("task:ana")
        store.name(ana, "Ana")
        inbox.begun(here) ==> Right(false)
        inbox.posted(here, SourceId("root"), "the summary", PostedAt, Asking, ana) ==> Right(true)
        inbox.hear(here, SourceId("r1"), "why this?", ana, Said, Reach.Nowhere) ==> Right(())
        store.written(here) ==> Vector(
          (Payload.Posted("the summary"), None),
          (Payload.Heard("why this?"), Some("Ana"))
        )
        (store.dated(here), store.periods(here).map(_.openedAt), store.postedBy(here)) ==>
          (Vector(PostedAt, Said), Vector(PostedAt), Some(Asking))
        inbox.begun(here) ==> Right(true)
        inbox.ingested(here, SourceId("root")) ==> Right(None)
      }
    }

    test(
      "a conversation is created at its room's label as declared, kept whoever records in it later, and a run's at its schedule's clearance"
    ) {
      val trial = TestLabels.compartment("trial")
      val internal = Label.at(Level.Internal, trial)
      val confidential = Label.at(Level.Confidential, trial)
      val visibility = (for {
        compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
        rooms <- RoomLabels
          .of(Vector(TestLabels.place("slack:T/C") -> internal), Labelled.Mapped(Label.Public))
          .left
          .map(_.toString)
        v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
      } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
      withInbox(Uncapped, visibility) { (inbox, store) =>
        val asked = Origin.Slack("T", "C", "1.0")
        val heard = Origin.Slack("T", "C", "2.0")
        val posted = Origin.Slack("T", "C", "3.0")
        val elsewhere = Origin.Slack("T", "D", "1.0")
        val _ = inbox.ingest(asked, SourceId("m1"), said("one"), Account.Local)
        inbox.hear(heard, SourceId("m1"), "two", Account.Local, Said, Reach.Nowhere) ==>
          Right(())
        inbox.posted(posted, SourceId("root"), "three", PostedAt, Asking, Account.Grit) ==>
          Right(true)
        val _ = inbox.ingest(elsewhere, SourceId("m1"), said("four"), Account.Local)
        Vector(asked, heard, posted, elsewhere).map(store.labelled) ==>
          Vector(Some(internal), Some(internal), Some(internal), Some(Label.Public))
        store.declare(
          Vector(
            (
              Declarer.Deployment,
              Declared(
                ScheduleKey.of("labelled").fold(sys.error, identity),
                remind,
                SlotRule.Once(Due, hour),
                Count(1),
                confidential
              )
            )
          ),
          Due.minusSeconds(3600)
        )
        val (_, slot) = begun(inbox.startSlot(scheduled("labelled"), Some(1), Due))
        store.labelled(slot.origin(remind.name)) ==> Some(confidential)
      }
    }

    test("a post records nothing once its origin has anything, a repeat of itself included") {
      withInbox(Uncapped) { (inbox, store) =>
        val heard = Origin.Task("inbox", "posted-late")
        inbox.hear(heard, SourceId("r1"), "first", Account.Local, Said, Reach.Nowhere) ==>
          Right(())
        inbox.begun(heard) ==> Right(true)
        inbox.posted(heard, SourceId("root"), "late", PostedAt, Asking, Account.Local) ==>
          Right(false)
        (store.written(heard).map(_._1), store.postedBy(heard)) ==>
          (Vector(Payload.Heard("first")), None)
        val twice = Origin.Task("inbox", "posted-twice")
        inbox.posted(twice, SourceId("root"), "once", PostedAt, Asking, Account.Local) ==>
          Right(true)
        inbox.posted(twice, SourceId("root"), "once", PostedAt, Asking, Account.Local) ==>
          Right(false)
        store.written(twice).map(_._1) ==> Vector(Payload.Posted("once"))
      }
    }

    test(
      "once the day's spend reaches the cap a new message is refused, recording nothing, not even its conversation; one already recorded is still its turn; a heard one and a post are still recorded"
    ) {
      val cap = DailyCap.of("1").fold(e => throw new java.lang.AssertionError(e), identity)
      withInbox(Budget(ZoneOffset.UTC, Some(cap))) { (inbox, store) =>
        val here = Origin.Task("inbox", "capped")
        val first = inbox.ingest(here, SourceId("m1"), said("one"), Account.Local)
        store.spend(BigDecimal("0.4"))
        store.spend(BigDecimal("0.6"))
        inbox.ingest(here, SourceId("m1"), said("one"), Account.Local) ==> first
        val refused = inbox.ingest(here, SourceId("m2"), said("two"), Account.Local)
        refused.left.map {
          case InboxError.OverCap(spent, c, _) => (spent.calls, spent.cost, c)
          case other => other
        } ==> Left((2, Cost.Exact(BigDecimal("1.0")), cap))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        val fresh = Origin.Task("inbox", "capped-first")
        inbox.ingest(fresh, SourceId("m1"), said("one"), Account.Local).left.map {
          case InboxError.OverCap(_, c, _) => c
          case other => other
        } ==> Left(cap)
        store.exists(fresh) ==> false
        val heard = Origin.Task("inbox", "capped-heard")
        inbox.hear(
          heard,
          SourceId("m1"),
          "lunch?",
          Account.Local,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        store.written(heard) ==> Vector((Payload.Heard("lunch?"), None))
        val posted = Origin.Task("inbox", "capped-posted")
        inbox.posted(posted, SourceId("root"), "posted", PostedAt, Asking, Account.Local) ==>
          Right(true)
        store.written(posted).map(_._1) ==> Vector(Payload.Posted("posted"))
      }
    }
  }
}

object InboxContract {

  /** What a test reads and writes of the store under an inbox: `spend` records a call that
    * cost that many dollars now; `exists`, whether a conversation from an origin exists;
    * `written`, the entries of an origin's conversation in order, each with the name its
    * author's account is named; `dated`, when each of those entries is dated;
    * `periods`, an origin's conversation's periods, oldest first; `close` seals the period a
    * turn is in, its last turn that one; `name` names an account; `reached`, the reach kept
    * for each of an origin's conversation's entries, in order (none for one not heard);
    * `postedBy`, the call an origin's conversation's opening post was made by; `declare` makes
    * the declared schedules these, as of a time; `schedule`, one kept, as the store reads it;
    * `waiting`, the schedules a clock pass at a time takes up, those with a run in flight then
    * those due; `end` ends a run, which its job left, without a
    * reply, and returns once it has; `replied` records a slot's run at a version replied, as of a
    * time, as the run's reply does; `labelled`, the label an origin's conversation was created
    * at.
    */
  final case class Store(
      spend: BigDecimal => Unit,
      exists: Origin => Boolean,
      written: Origin => Vector[(Payload, Option[String])],
      dated: Origin => Vector[Instant],
      periods: Origin => Vector[Period],
      close: TurnRef => Unit,
      name: (Account, String) => Unit,
      reached: Origin => Vector[Option[Reach]],
      postedBy: Origin => Option[CallSlot],
      declare: (Vector[(Declarer, Declared[?])], Instant) => Unit,
      schedule: ScheduleId => Option[Schedule],
      waiting: Instant => Vector[ScheduleId],
      end: TurnRef => Unit,
      replied: (Slot, Int, Instant) => Unit,
      labelled: Origin => Option[Label]
  )
}
