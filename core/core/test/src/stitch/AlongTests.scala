package grit.core.stitch

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PrincipalId, TurnRef}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Directory, Scope}
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  Origin,
  Payload
}
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

import utest.*

/** [[Along.read]]: what a reader is shown of its strand. */
object AlongTests extends TestSuite {

  private val Now = Instant.parse("2026-09-30T22:40:00Z")
  private val Day = 86_400L

  private final class World {
    val store = new InMemoryEntryStore
    @caps.unsafe.untrackedCaptures
    var origins = Map.empty[ConversationId, Origin]
    val periods = new InMemoryPeriodStore(store, c => origins(c))
    val stitches = new InMemoryStitchStore(store, c => origins(c))

    def thread(origin: Origin): Conversation = {
      val id = ConversationId(origin.place.written)
      origins = origins.updated(id, origin)
      Conversation(id, origin, PrincipalId("slack:T/U1"), Now, Label.Public)
    }

    def say(c: Conversation, text: String, secondsAgo: Long): Entry = {
      given grit.core.store.Tx = TestTx.fake
      val next = entries(store.lockNext(c.id))
      val at = Now.minusSeconds(secondsAgo)
      entries(periods.openFor(c.id, next.turnSeq, at))
      val e = Entry(
        EntryId(s"${ConversationId.value(c.id)}:${next.seq}"),
        c.id,
        next.turnSeq,
        None,
        next.seq,
        Payload.Heard(text),
        at
      )
      entries(store.insert(e))
      e
    }

    def follow(first: Entry, root: Conversation): Unit = {
      given grit.core.store.Tx = TestTx.fake
      val seen = Seen(ujson.Obj(), Vector.empty, Tuning.Default)
      val placed = Placed.Follows(
        root.id,
        Probability.clamped(0.9),
        seen,
        "jev",
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None)
      )
      entries(stitches.record(first.id, placed, Now))
    }

    /** Seals and purges the period of `c`'s turn that `first` is in. */
    def purge(c: Conversation, first: Entry): Unit = {
      given grit.core.store.Tx = TestTx.fake
      val period = entries(periods.of(TurnRef(c.id, first.turnSeq)))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      entries(
        periods.seal(
          CloseRef(period, first.turnSeq, Now),
          CloseReason.Lapsed,
          TestClosings.prose("x"),
          Now
        )
      )
      entries(periods.purge(period, Now))
    }

    def read(c: Conversation, scope: Scope): Strand.Read = {
      given grit.core.store.Tx = TestTx.fake
      entries(Along.read(stitches, c, scope, Now.minusSeconds(7 * Day), Now))
    }
  }

  private def entries[A](e: Either[grit.core.store.StoreError, A]): A =
    e.fold(x => throw new java.lang.AssertionError(s"store failed: $x"), identity)

  val tests = Tests {
    test("a reader reads its root's opening however old, and its strand's messages in range") {
      val w = new World
      val a = w.thread(Origin.Slack("T", "C1", "1.0"))
      val b = w.thread(Origin.Slack("T", "C1", "2.0"))
      val c = w.thread(Origin.Slack("T", "C1", "3.0"))
      val opening = w.say(a, "the Engine contract term?", 10 * Day)
      val recent = w.say(a, "renegotiation at 100k", 60)
      w.follow(w.say(b, "Is this a real question", 50), a)
      val lol = w.say(c, "lol", 40)
      w.follow(lol, a)
      val read = w.read(b, Scope.Room)
      read.opening.map(_.entry) ==> Some(opening)
      read.said.map(_.entry) ==> Vector(recent, lol)
      read.gone ==> Vector.empty
    }

    test("a reader's strand names each conversation in it, one it shows nothing of among them") {
      val w = new World
      val a = w.thread(Origin.Slack("T", "C1", "1.0"))
      val b = w.thread(Origin.Slack("T", "C1", "2.0"))
      val c = w.thread(Origin.Slack("T", "C1", "3.0"))
      w.say(a, "the Engine contract term?", 10 * Day)
      w.follow(w.say(c, "lol", 8 * Day), a)
      w.follow(w.say(b, "Is this a real question", 50), a)
      val read = w.read(b, Scope.Room)
      read.said ==> Vector.empty
      read.conversations ==> Set(a.id, c.id)
      w.read(b, Scope.Off).conversations ==> Set(a.id, c.id)
    }

    test("a member the scope no longer holds is not read; one not stitchable reads nothing") {
      val w = new World
      val a = w.thread(Origin.Slack("T", "C1", "1.0"))
      val b = w.thread(Origin.Slack("T", "C1", "2.0"))
      w.say(a, "the Engine contract term?", 90)
      w.follow(w.say(b, "Is this a real question", 50), a)
      val off = w.read(b, Scope.Off)
      (off.opening, off.said, off.gone) ==> (None, Vector.empty, Vector.empty)
      // A TUI conversation linked to a purged root, which a read would name as gone.
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      val tui = w.thread(Origin.Tui(dir, "s"))
      val gone = w.thread(Origin.Slack("T", "C1", "3.0"))
      val goneFirst = w.say(gone, "lunch?", 80)
      w.follow(w.say(tui, "and the date?", 40), gone)
      w.purge(gone, goneFirst)
      w.read(tui, Scope.Everywhere) ==> Strand.Read.empty
    }

    test("a root whose first message is purged is gone, to be shown by its record") {
      val w = new World
      val a = w.thread(Origin.Slack("T", "C1", "1.0"))
      val b = w.thread(Origin.Slack("T", "C1", "2.0"))
      val first = w.say(a, "the Engine contract term?", 3 * Day)
      w.follow(w.say(b, "Is this a real question", 50), a)
      w.purge(a, first)
      val read = w.read(b, Scope.Room)
      (read.opening, read.said, read.gone) ==> (None, Vector.empty, Vector(a.id))
    }
  }
}
