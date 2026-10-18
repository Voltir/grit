package grit.core.store

import java.time.Instant

import grit.core.agenda.{Agenda, Placement, Weights}
import grit.core.id.{ConversationId, EntryId, EntrySeq, PeriodSeq, TurnSeq}
import grit.core.message.Message
import grit.core.period.{CloseReason, Closing, Edit, Flows, Section, TestClosings}
import grit.core.visibility.Integrity

import TestClosings.{balance, line}
import utest.*

object EntryAgendaTests extends TestSuite {

  private val c = ConversationId("c")
  private val at = Instant.parse("2026-09-20T10:00:00Z")

  private def entry(id: String, turn: Long, seq: Long, payload: Payload): Entry =
    Entry(EntryId(id), c, TurnSeq(turn), None, EntrySeq(seq), payload, at, Integrity.Member)

  private val photo = line(Section.Agenda, "Photo Rename", 1, 1, Some("renaming photos"))
  private val backup = line(Section.Agenda, "Laptop Backup", 1, 2)

  /** Period 1 (turns 0–1) opened the photo item; its closing carries it and a backup one,
    * the backup item spoken in last.
    */
  private val period1: Vector[Entry] = Vector(
    entry("u0", 0, 0, Payload.Message(Message.User("rename my photos"))),
    entry(
      "t0",
      0,
      1,
      Payload.Agenda(
        Vector(
          Agenda.Event.Opened(Agenda.ItemId("topic:c:0")),
          Agenda.Event
            .Placed(TurnSeq(0), Weights.whole(Agenda.ItemId("topic:c:0")), Placement.First),
          Agenda.Event.Described(Agenda.ItemId("topic:c:0"), "Photo Rename", "renaming photos")
        )
      )
    ),
    entry("u1", 1, 2, Payload.Message(Message.User("and backups?")))
  )

  private val closing: Entry = entry(
    "closing:c:1",
    1,
    3,
    Payload.Closed(
      PeriodSeq.First,
      CloseReason.Lapsed,
      Closing(
        Flows.of("p", None, Vector()).getOrElse(throw new java.lang.AssertionError("f")),
        balance(photo, backup)
      )
    )
  )

  private val period2: Vector[Entry] = Vector(
    entry("u2", 2, 4, Payload.Message(Message.User("back to the photos"))),
    entry(
      "t2",
      2,
      5,
      Payload.Agenda(
        Vector(
          Agenda.Event
            .Placed(TurnSeq(2), Weights.whole(Agenda.ItemId.carried(photo.id)), Placement.First)
        )
      )
    )
  )

  private def shown(t: Agenda) = t.items.map(x => (x.id, x.shown))

  val tests = Tests {
    test("after a close, the agenda is the closing's, the most recently spoken in current") {
      val t = EntryAgenda.before(period1 ++ Vector(closing), TurnSeq(2))
      shown(t) ==> Vector(
        Agenda.ItemId.carried(backup.id) -> "Laptop Backup",
        Agenda.ItemId.carried(photo.id) -> "Photo Rename"
      )
      t.current.map(_.id) ==> Some(Agenda.ItemId.carried(backup.id))
    }

    test("the agenda after a close is the same whether or not the period before was purged") {
      val kept = period1 ++ Vector(closing) ++ period2
      val purged = Vector(closing) ++ period2
      EntryAgenda.through(purged, TurnSeq(2)) ==> EntryAgenda.through(kept, TurnSeq(2))
      EntryAgenda.through(kept, TurnSeq(2)).current.map(_.id) ==> Some(
        Agenda.ItemId.carried(photo.id)
      )
    }

    test("before a turn, its own events are left out") {
      val all = period1 ++ Vector(closing) ++ period2
      EntryAgenda.before(all, TurnSeq(2)).current.map(_.id) ==> Some(
        Agenda.ItemId.carried(backup.id)
      )
    }

    test("a close touches carried items spoken in, adds named new ones, oldest first") {
      val fresh = Agenda.ItemId("topic:c:3")
      val unnamed = Agenda.ItemId("topic:c:4")
      val events =
        Vector(
          Agenda.Event.Opened(fresh),
          Agenda.Event.Placed(TurnSeq(2), Weights.whole(fresh), Placement.First),
          Agenda.Event.Described(fresh, "Tax Return", "filing taxes"),
          Agenda.Event
            .Placed(TurnSeq(3), Weights.whole(Agenda.ItemId.carried(photo.id)), Placement.First),
          Agenda.Event.Opened(unnamed),
          Agenda.Event.Placed(TurnSeq(4), Weights.whole(unnamed), Placement.First)
        )
      EntryAgenda.edits(balance(photo, backup), events) ==> Vector(
        Edit.Add(Section.Agenda, "Tax Return"),
        Edit.Summarise(line(Section.Agenda, "Tax Return", 1, 1).id, "filing taxes"),
        Edit.Touch(photo.id)
      )
    }

    test(
      "a carried item comes with its summary, and a close re-summarises it only when described anew"
    ) {
      val all = period1 ++ Vector(closing)
      EntryAgenda
        .before(all, TurnSeq(2))
        .get(Agenda.ItemId.carried(photo.id))
        .flatMap(_.summary) ==>
        Some("renaming photos")
      val spoken =
        Agenda.Event.Placed(
          TurnSeq(2),
          Weights.whole(Agenda.ItemId.carried(photo.id)),
          Placement.First
        )
      EntryAgenda.edits(
        balance(photo, backup),
        Vector(
          spoken,
          Agenda.Event.Described(Agenda.ItemId.carried(photo.id), "Photo Rename", "renaming photos")
        )
      ) ==> Vector(Edit.Touch(photo.id))
      EntryAgenda.edits(
        balance(photo, backup),
        Vector(
          spoken,
          Agenda.Event.Described(Agenda.ItemId.carried(photo.id), "Photo Rename", "HEIC too")
        )
      ) ==> Vector(Edit.Touch(photo.id), Edit.Summarise(photo.id, "HEIC too"))
    }
  }
}
