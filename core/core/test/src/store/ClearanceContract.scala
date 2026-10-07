package grit.core.store

import java.time.Instant

import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  ToolCallId,
  TurnRef
}
import grit.core.message.Message
import grit.core.period.{CloseOrdinal, CloseReason, TestClosings}
import grit.core.place.{Directory, Place}
import grit.core.recipe.RoomReads
import grit.core.stitch.StitchStore
import grit.core.visibility.{Clearance, Label, TestLabels}

import utest.*

/** What every store that reads entries returns under a transaction's clearance (ADR 0030): only
  * the rows it reads, a room's members everything recorded in their room up to its label, and
  * nothing of another room past what they read everywhere. Run against the in-memory fakes in
  * core and the SQL stores in grit.dbos, over one store set per implementation.
  *
  * Tests share the stores, so each makes its own conversations, in rooms named for it, and
  * filters what spans every conversation to its own.
  */
abstract class ClearanceContract extends TestSuite {

  protected def entries: EntryStore

  protected def periods: PeriodStore

  protected def stitches: StitchStore

  protected def rooms: RoomReads

  /** Runs `body` in one transaction opened at `clearance`, committed when it returns. */
  protected def transaction[A](clearance: Clearance)(body: (Tx^) ?=> A): A

  /** The conversation for `origin`, created at `label` when new. */
  protected def conversation(origin: Origin, label: Label): ConversationId

  /** Every label the contract writes at. */
  private val everything = Clearance.of(TestLabels.Trialled.compartments.top)

  private val public = Clearance.of(Label.Public)

  private def right[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val At = Instant.parse("2026-10-01T09:00:00Z")

  private def at(minute: Long): Instant = At.plusSeconds(60 * minute)

  private def thread(channel: String, ts: String): Origin = Origin.Slack("T1", channel, ts)

  private def tui(path: String, session: String): Origin =
    Origin.Tui(
      Directory.of(path).fold(e => throw new java.lang.AssertionError(e), identity),
      session
    )

  /** What a period of one conversation recorded: a person's message, a tool result, and the
    * closing it was sealed with.
    */
  private final case class Recorded(
      conversation: ConversationId,
      message: EntryId,
      result: EntryId,
      closing: EntryId,
      seqs: Vector[EntrySeq],
      turn: TurnRef,
      next: TurnRef
  ) {
    def ids: Vector[String] = Vector(message, result, closing).map(EntryId.value)
  }

  /** `origin`'s conversation, at `label`, with one period recorded and closed at `minute`, as
    * prose `prose`; its next period opened at `minute + 1` when `reopen`.
    */
  private def record(
      origin: Origin,
      label: Label,
      prose: String,
      minute: Long,
      reopen: Boolean = false
  ): Recorded = {
    val c = conversation(origin, label)
    val name = ConversationId.value(c)
    transaction(everything) {
      val next = right(entries.lockNext(c))
      right(periods.openFor(c, next.turnSeq, at(minute)))
      val message = EntryId(s"$name:message")
      val result = EntryId(s"$name:result")
      right(
        entries.insert(
          Entry(
            message,
            c,
            next.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User(prose)),
            at(minute)
          )
        )
      )
      right(
        entries.insert(
          Entry(
            result,
            c,
            next.turnSeq,
            Some(message),
            next.seq.next,
            Payload.Message(Message.ToolResult(ToolCallId(s"$name:call"), prose, false)),
            at(minute)
          )
        )
      )
      val period = PeriodRef(c, PeriodSeq.First)
      val sealedAs = right(
        periods.seal(
          CloseRef(period, next.turnSeq, at(minute)),
          CloseReason.Lapsed,
          TestClosings.prose(prose),
          at(minute)
        )
      )
      assert(sealedAs == Sealed.Closed(period.closingId))
      if (reopen) {
        val after = right(entries.lockNext(c))
        right(periods.openFor(c, after.turnSeq, at(minute + 1)))
        right(
          entries.insert(
            Entry(
              EntryId(s"$name:again"),
              c,
              after.turnSeq,
              None,
              after.seq,
              Payload.Message(Message.User(s"$prose again")),
              at(minute + 1)
            )
          )
        )
      }
      Recorded(
        c,
        message,
        result,
        period.closingId,
        Vector(next.seq, next.seq.next, next.seq.next.next),
        TurnRef(c, next.turnSeq),
        TurnRef(c, next.turnSeq.next)
      )
    }
  }

  /** What `clearance` reads of `r` through each entry read that names it. */
  private def read(clearance: Clearance, r: Recorded): Vector[Vector[String]] =
    transaction(clearance) {
      Vector(
        ids(right(entries.list(r.conversation))),
        ids(Vector(r.message, r.result, r.closing).flatMap(id => right(entries.get(id)))),
        ids(right(entries.at(r.conversation, r.seqs))),
        ids(right(entries.ofTurn(r.turn))),
        right(periods.closingBefore(r.next)).toVector.map(c => EntryId.value(c.entry.id))
      )
    }

  private def ids(es: Vector[Entry]): Vector[String] = es.map(e => EntryId.value(e.id))

  private def heardIds(said: Vector[grit.core.stitch.Said]): Vector[String] =
    said.map(s => EntryId.value(s.entry.id))

  /** A reader in `origin`'s room, labelled `label`, for an asker cleared for nothing. */
  private def uncleared(origin: Origin, label: Label): Clearance =
    Clearance.inRoom(origin.room, label, Label.Public)

  val tests = Tests {
    test(
      "a {trial} room's entries are not returned to a public reader outside it: get, list, at, ofTurn, closingBefore"
    ) {
      val r = record(thread("CL1", "1.0"), TestLabels.Trial, "kept", 0)
      (read(everything, r), read(public, r)) ==> (
        Vector(r.ids, r.ids, r.ids, r.ids, Vector(EntryId.value(r.closing))),
        Vector.fill(5)(Vector.empty[String])
      )
    }

    test(
      "an uncleared asker reads everything recorded in a {trial} room it is in: messages, a cleared asker's tool result, closings"
    ) {
      val origin = thread("CL2", "1.0")
      val r = record(origin, TestLabels.Trial, "ours", 0)
      val sibling = record(thread("CL2", "2.0"), TestLabels.Trial, "sibling", 1)
      val reader = uncleared(origin, TestLabels.Trial)
      (read(reader, r).map(_.size), read(reader, sibling).head) ==> (
        Vector(3, 3, 3, 3, 1),
        sibling.ids
      )
    }

    test("it reads no entry of another {trial} room") {
      val origin = thread("CL3", "1.0")
      record(origin, TestLabels.Trial, "here", 0)
      val there = record(thread("CL4", "1.0"), TestLabels.Trial, "there", 1)
      read(uncleared(origin, TestLabels.Trial), there) ==> Vector.fill(5)(Vector.empty[String])
    }

    test("a session in /a/b is not in /a's room") {
      val inner = record(tui("/clearance/a/b", "s"), TestLabels.Trial, "inner", 0)
      read(uncleared(tui("/clearance/a", "s"), TestLabels.Trial), inner) ==>
        Vector.fill(5)(Vector.empty[String])
    }

    test(
      "closedAfter returns only the closings it reads, in close order: a {trial} one between two public ones is passed over publicly"
    ) {
      val before =
        right(transaction(everything)(periods.closedAfter(CloseOrdinal.Start, 10000))).lastOption
          .fold(CloseOrdinal.Start)(_.order)
      val one = record(thread("CL5", "1.0"), Label.Public, "first", 0)
      val two = record(thread("CL6", "1.0"), TestLabels.Trial, "second", 1)
      val three = record(thread("CL7", "1.0"), Label.Public, "third", 2)
      val mine = Set(one, two, three).map(_.conversation)
      def closed(clearance: Clearance): Vector[String] =
        right(transaction(clearance)(periods.closedAfter(before, 10000)))
          .filter(p => mine.contains(p.ref.conversationId))
          .map(_.closing.flows.prose)
      (
        closed(everything),
        closed(public),
        closed(uncleared(thread("CL6", "x"), TestLabels.Trial))
      ) ==> (
        Vector("first", "second", "third"),
        Vector("first", "third"),
        Vector("first", "second", "third")
      )
    }

    test(
      "openElsewhere, closedElsewhere and open name only the conversations it reads: another {trial} room's are left out"
    ) {
      val origin = thread("CL8", "1.0")
      val here = record(origin, TestLabels.Trial, "here", 0, reopen = true)
      val near = record(thread("CL8", "2.0"), TestLabels.Trial, "near", 0, reopen = true)
      val far = record(thread("CL9", "1.0"), TestLabels.Trial, "far", 0, reopen = true)
      val open = record(thread("CL10", "1.0"), Label.Public, "open", 0, reopen = true)
      val mine = Set(here, near, far, open).map(_.conversation)
      val names = Map(
        near.conversation -> "near",
        far.conversation -> "far",
        open.conversation -> "open",
        here.conversation -> "here"
      )
      def seen(clearance: Clearance): Vector[Vector[String]] = transaction(clearance) {
        Vector(
          right(periods.openElsewhere(here.conversation)).map(_.conversation),
          right(periods.closedElsewhere(here.conversation)).map(_.conversation),
          right(periods.open()).map(_.activity.period.conversationId)
        ).map(_.filter(mine.contains).flatMap(names.get).sorted)
      }
      (seen(everything), seen(uncleared(origin, TestLabels.Trial))) ==> (
        Vector(
          Vector("far", "near", "open"),
          Vector("far", "near", "open"),
          Vector("far", "here", "near", "open")
        ),
        Vector(Vector("near", "open"), Vector("near", "open"), Vector("here", "near", "open"))
      )
    }

    test(
      "stitching's and a pool's room reads return only the messages it reads: spokenIn, said, openings, RoomReads.said"
    ) {
      val origin = thread("CL11", "1.0")
      val here = record(origin, TestLabels.Trial, "here", 0)
      val there = record(thread("CL12", "1.0"), TestLabels.Trial, "there", 1)
      val both = Vector(here.conversation, there.conversation)
      def heard(clearance: Clearance, room: Place): Vector[Vector[String]] =
        transaction(clearance) {
          Vector(
            heardIds(right(stitches.spokenIn(room, at(-1), at(10)))),
            heardIds(right(stitches.said(both, at(-1), at(10)))),
            heardIds(right(stitches.openings(both))),
            heardIds(right(rooms.said(room, at(-1), at(10), Set.empty, 10)))
          )
        }
      val theirs = thread("CL12", "1.0").room
      (heard(everything, theirs), heard(uncleared(origin, TestLabels.Trial), theirs)) ==> (
        Vector(
          Vector(there.message, there.closing).map(EntryId.value),
          Vector(here.message, there.message).map(EntryId.value),
          Vector(here.message, there.message).map(EntryId.value),
          Vector(EntryId.value(there.message))
        ),
        Vector(
          Vector.empty,
          Vector(EntryId.value(here.message)),
          Vector(EntryId.value(here.message)),
          Vector.empty
        )
      )
    }
  }
}
