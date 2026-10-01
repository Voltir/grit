package grit.core.inbox

import java.time.{Instant, ZoneOffset}

import grit.core.id.{CallSlot, ConversationId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Message}
import grit.core.period.{Period, PeriodState}
import grit.core.speech.Reach
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Origin, Payload}

import utest.*

/** What every [[Inbox]] keeps of recording messages and reading a turn's progress, run
  * against the in-memory fake in core and SqlInbox in grit.dbos.
  */
abstract class InboxContract extends TestSuite {

  /** Runs `body` over a store holding nothing from the origins these tests use, with its
    * inbox, taking new messages as `budget` allows, and the [[InboxContract.Store]] under it.
    */
  protected def withInbox[A](budget: Budget)(body: (Inbox, InboxContract.Store^) => A): A

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

  val tests = Tests {
    test("ingested: the turn a message was recorded as; none for one never recorded") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "ingested")
        val there = Origin.Task("inbox", "elsewhere")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
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
        inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        inbox.hear(here, SourceId("m2"), "two", PrincipalId.Local, Said, Reach.Nowhere) ==> Right(
          ()
        )
        inbox.hear(
          there,
          SourceId("m3"),
          "three",
          PrincipalId.Local,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        inbox.recorded(here, asked) ==> Right(Set(SourceId("m1"), SourceId("m2")))
      }
    }

    test("ingest: the same source is the same turn, a new one the next turn") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "same-source")
        val first = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        val again = inbox.ingest(here, SourceId("m1"), said("one, redelivered"), PrincipalId.Local)
        val second = inbox.ingest(here, SourceId("m2"), said("two"), PrincipalId.Local)
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
        val one = inbox.ingest(here, SourceId("p1"), said("one"), PrincipalId.Local)
        store.periods(here).map(p => (p.first, p.state)) ==>
          Vector((TurnSeq.First, PeriodState.Open))
        one.foreach(store.close)
        val two = inbox.ingest(here, SourceId("p2"), said("two"), PrincipalId.Local)
        two.map(_.turnSeq) ==> Right(TurnSeq.First.next)
        store.periods(here).map(p => (p.first, p.state == PeriodState.Open)) ==>
          Vector((TurnSeq.First, false), (TurnSeq.First.next, true))
      }
    }

    test("an ingested message records who wrote it, and a redelivery by another keeps the first") {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "authors")
        val ana = PrincipalId("task:ana")
        val bo = PrincipalId("task:bo")
        store.enroll(ana, "Ana")
        store.enroll(bo, "Bo")
        inbox.ingest(here, SourceId("a1"), said("one"), ana)
        inbox.ingest(here, SourceId("a1"), said("one"), bo)
        store.written(here) ==> Vector((Payload.Message(said("one")), Some("Ana")))
      }
    }

    test("a turn recorded but never started is Open") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "open")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        turn.flatMap(inbox.progress) ==> Right(Progress.Open)
      }
    }

    test(
      "a heard message is recorded once, as heard, under its author's name, and is no turn ingested"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "heard")
        val ana = PrincipalId("task:ana")
        store.enroll(ana, "Ana")
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
        val first = Reach(Some("C/1"), Set(PrincipalId("task:ben")))
        inbox.hear(here, SourceId("m1"), "ask Ben", PrincipalId.Local, Said, first) ==> Right(())
        inbox.hear(here, SourceId("m1"), "ask Ben", PrincipalId.Local, Said, Reach.Nowhere) ==>
          Right(())
        inbox.ingest(here, SourceId("m2"), said("hi"), PrincipalId.Local).map(_ => ()) ==> Right(())
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
          PrincipalId.Local,
          Said,
          Reach.Nowhere
        ) ==>
          Right(())
        inbox.hear(
          here,
          SourceId("m2"),
          "fine by me",
          PrincipalId.Local,
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
        val ana = PrincipalId("task:ana")
        store.enroll(ana, "Ana")
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

    test("a post records nothing once its origin has anything, a repeat of itself included") {
      withInbox(Uncapped) { (inbox, store) =>
        val heard = Origin.Task("inbox", "posted-late")
        inbox.hear(heard, SourceId("r1"), "first", PrincipalId.Local, Said, Reach.Nowhere) ==>
          Right(())
        inbox.begun(heard) ==> Right(true)
        inbox.posted(heard, SourceId("root"), "late", PostedAt, Asking, PrincipalId.Local) ==>
          Right(false)
        (store.written(heard).map(_._1), store.postedBy(heard)) ==>
          (Vector(Payload.Heard("first")), None)
        val twice = Origin.Task("inbox", "posted-twice")
        inbox.posted(twice, SourceId("root"), "once", PostedAt, Asking, PrincipalId.Local) ==>
          Right(true)
        inbox.posted(twice, SourceId("root"), "once", PostedAt, Asking, PrincipalId.Local) ==>
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
        val first = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        store.spend(BigDecimal("0.4"))
        store.spend(BigDecimal("0.6"))
        inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local) ==> first
        val refused = inbox.ingest(here, SourceId("m2"), said("two"), PrincipalId.Local)
        refused.left.map {
          case InboxError.OverCap(spent, c, _) => (spent.calls, spent.cost, c)
          case other => other
        } ==> Left((2, Cost.Exact(BigDecimal("1.0")), cap))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        val fresh = Origin.Task("inbox", "capped-first")
        inbox.ingest(fresh, SourceId("m1"), said("one"), PrincipalId.Local).left.map {
          case InboxError.OverCap(_, c, _) => c
          case other => other
        } ==> Left(cap)
        store.exists(fresh) ==> false
        val heard = Origin.Task("inbox", "capped-heard")
        inbox.hear(
          heard,
          SourceId("m1"),
          "lunch?",
          PrincipalId.Local,
          Said,
          Reach.Nowhere
        ) ==> Right(())
        store.written(heard) ==> Vector((Payload.Heard("lunch?"), None))
        val posted = Origin.Task("inbox", "capped-posted")
        inbox.posted(posted, SourceId("root"), "posted", PostedAt, Asking, PrincipalId.Local) ==>
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
    * author was enrolled under; `dated`, when each of those entries is dated;
    * `periods`, an origin's conversation's periods, oldest first; `close` seals the period a
    * turn is in, its last turn that one; `enroll` names a person; `reached`, the reach kept
    * for each of an origin's conversation's entries, in order (none for one not heard);
    * `postedBy`, the call an origin's conversation's opening post was made by.
    */
  final case class Store(
      spend: BigDecimal => Unit,
      exists: Origin => Boolean,
      written: Origin => Vector[(Payload, Option[String])],
      dated: Origin => Vector[Instant],
      periods: Origin => Vector[Period],
      close: TurnRef => Unit,
      enroll: (PrincipalId, String) => Unit,
      reached: Origin => Vector[Option[Reach]],
      postedBy: Origin => Option[CallSlot]
  )
}
