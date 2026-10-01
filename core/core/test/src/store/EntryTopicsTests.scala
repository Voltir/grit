package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq}
import grit.core.message.Message
import grit.core.period.{CloseReason, Closing, Edit, Flows, Section, TestClosings}
import grit.core.topic.{Placement, TopicEvent, TopicId, Topics, Weights}

import TestClosings.{balance, line}
import utest.*

object EntryTopicsTests extends TestSuite {

  private val c = ConversationId("c")
  private val at = Instant.parse("2026-09-20T10:00:00Z")

  private def entry(id: String, turn: Long, seq: Long, payload: Payload): Entry =
    Entry(EntryId(id), c, TurnSeq(turn), None, seq, payload, at)

  private val photo = line(Section.Topics, "Photo Rename", 1, 1, Some("renaming photos"))
  private val backup = line(Section.Topics, "Laptop Backup", 1, 2)

  /** Period 1 (turns 0–1) opened the photo topic; its closing carries it and a backup one,
    * the backup topic spoken in last.
    */
  private val period1: Vector[Entry] = Vector(
    entry("u0", 0, 0, Payload.Message(Message.User("rename my photos"))),
    entry(
      "t0",
      0,
      1,
      Payload.Topic(
        Vector(
          TopicEvent.Opened(TopicId("topic:c:0")),
          TopicEvent.Placed(TurnSeq(0), Weights.whole(TopicId("topic:c:0")), Placement.First),
          TopicEvent.Described(TopicId("topic:c:0"), "Photo Rename", "renaming photos")
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
      Payload.Topic(
        Vector(
          TopicEvent.Placed(TurnSeq(2), Weights.whole(TopicId.carried(photo.id)), Placement.First)
        )
      )
    )
  )

  private def shown(t: Topics) = t.topics.map(x => (x.id, x.shown))

  val tests = Tests {
    test("after a close, topics are the closing's, the most recently spoken in current") {
      val t = EntryTopics.before(period1 ++ Vector(closing), TurnSeq(2))
      shown(t) ==> Vector(
        TopicId.carried(backup.id) -> "Laptop Backup",
        TopicId.carried(photo.id) -> "Photo Rename"
      )
      t.current.map(_.id) ==> Some(TopicId.carried(backup.id))
    }

    test("topics after a close are the same whether or not the period before was purged") {
      val kept = period1 ++ Vector(closing) ++ period2
      val purged = Vector(closing) ++ period2
      EntryTopics.through(purged, TurnSeq(2)) ==> EntryTopics.through(kept, TurnSeq(2))
      EntryTopics.through(kept, TurnSeq(2)).current.map(_.id) ==> Some(TopicId.carried(photo.id))
    }

    test("before a turn, its own events are left out") {
      val all = period1 ++ Vector(closing) ++ period2
      EntryTopics.before(all, TurnSeq(2)).current.map(_.id) ==> Some(TopicId.carried(backup.id))
    }

    test("a close touches carried topics spoken in, adds named new ones, oldest first") {
      val fresh = TopicId("topic:c:3")
      val unnamed = TopicId("topic:c:4")
      val events =
        Vector(
          TopicEvent.Opened(fresh),
          TopicEvent.Placed(TurnSeq(2), Weights.whole(fresh), Placement.First),
          TopicEvent.Described(fresh, "Tax Return", "filing taxes"),
          TopicEvent.Placed(TurnSeq(3), Weights.whole(TopicId.carried(photo.id)), Placement.First),
          TopicEvent.Opened(unnamed),
          TopicEvent.Placed(TurnSeq(4), Weights.whole(unnamed), Placement.First)
        )
      EntryTopics.edits(balance(photo, backup), events) ==> Vector(
        Edit.Add(Section.Topics, "Tax Return"),
        Edit.Summarise(line(Section.Topics, "Tax Return", 1, 1).id, "filing taxes"),
        Edit.Touch(photo.id)
      )
    }

    test(
      "a carried topic comes with its summary, and a close re-summarises it only when described anew"
    ) {
      val all = period1 ++ Vector(closing)
      EntryTopics.before(all, TurnSeq(2)).get(TopicId.carried(photo.id)).flatMap(_.summary) ==>
        Some("renaming photos")
      val spoken =
        TopicEvent.Placed(TurnSeq(2), Weights.whole(TopicId.carried(photo.id)), Placement.First)
      EntryTopics.edits(
        balance(photo, backup),
        Vector(
          spoken,
          TopicEvent.Described(TopicId.carried(photo.id), "Photo Rename", "renaming photos")
        )
      ) ==> Vector(Edit.Touch(photo.id))
      EntryTopics.edits(
        balance(photo, backup),
        Vector(spoken, TopicEvent.Described(TopicId.carried(photo.id), "Photo Rename", "HEIC too"))
      ) ==> Vector(Edit.Touch(photo.id), Edit.Summarise(photo.id, "HEIC too"))
    }
  }
}
