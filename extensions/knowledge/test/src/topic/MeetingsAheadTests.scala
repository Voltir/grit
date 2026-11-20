package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}

import grit.core.id.ConversationId
import grit.core.place.Place
import grit.core.stitch.StrandDigest
import grit.core.visibility.{Integrity, Label, TestLabels}

import utest.*

/** Which planned meetings a topics run sorts ahead of time ([[MeetingsAhead.due]]): within a
  * day of its scan, naming two people, in the listed room its topic was most written from.
  */
object MeetingsAheadTests extends TestSuite {
  import TestMeetings.{Kept, doc}

  private val Day: LocalDate = LocalDate.of(2026, 11, 20)
  private val At: Instant = Instant.parse("2026-11-20T08:00:00Z")
  private val A: Place = TestLabels.place("slack:T1/C-a")
  private val B: Place = TestLabels.place("slack:T1/C-b")
  private val C: Place = TestLabels.place("slack:T1/C-c")
  private val Pair: Vector[String] = Vector("Ana", "Ben")

  /** A meeting planned for `start` on `day` naming `who`, a passage a day for each strand of
    * `from` (`(strand, room)`, oldest first).
    */
  private def planned(
      day: LocalDate,
      start: Option[LocalTime],
      from: Vector[(String, Place)] = Vector("s" -> A),
      who: Vector[String] = Pair
  ): Topic =
    Topic.of(
      doc("A meeting"),
      from.zipWithIndex.map { case ((strand, room), i) =>
        Passage(
          Day.minusDays((from.size - i).toLong),
          Lineage.of(
            Vector(Lineage.Tag.Strand(ConversationId(strand), room, StrandDigest.of(Vector.empty)))
          ),
          doc(s"Talked of in $strand.")
        )
      },
      Topic.Kind.Meeting(Meeting.of(Meeting.State.Planned(Meeting.When(day, start)), who))
    )

  /** What is due at [[At]] over `topics` kept by name, raising in `rooms`: each due's topic
    * name and the due, and the counts.
    */
  private def found(rooms: Vector[Place], topics: (String, Topic)*) = {
    val kept = Kept(topics.map((n, t) => (n, t, Label.Public, Integrity.Member))*)
    val docs = kept.read()
    val f = MeetingsAhead.due(docs, rooms, ZoneOffset.UTC, At)
    val names = docs
      .map(d =>
        d.version -> topics
          .map(_._1)
          .find(n => grit.core.id.DocKey.value(d.key) == s"topic:strand-$n")
      )
      .toMap
    (
      f.asking.map(d => (names.get(d.version).flatten.getOrElse("?"), d)),
      (f.within, f.fewer, f.left)
    )
  }

  /** As [[found]]: each due's topic name, room and until, and the counts. */
  private def due(rooms: Vector[Place], topics: (String, Topic)*) = {
    val (asking, counts) = found(rooms, topics*)
    (asking.map((n, d) => (n, d.room, d.until)), counts)
  }

  private def at(h: Int, m: Int = 0, day: LocalDate = Day): Instant =
    day.atTime(h, m).toInstant(ZoneOffset.UTC)

  val tests = Tests {
    test(
      "a meeting starting within a day after the scan is due until its start; one with no start, only on the scan's day, until its end"
    ) {
      due(
        Vector(A, B, C),
        "soon" -> planned(Day, Some(LocalTime.of(9, 0))),
        "tomorrow" -> planned(Day.plusDays(1), Some(LocalTime.of(7, 30)), Vector("t" -> B)),
        "far" -> planned(Day.plusDays(1), Some(LocalTime.of(9, 0)), Vector("f" -> C)),
        "begun" -> planned(Day, Some(LocalTime.of(7, 0))),
        "allday" -> planned(Day, None, Vector("d" -> C)),
        "nextday" -> planned(Day.plusDays(1), None)
      ) ==> (
        Vector(
          ("soon", A, at(9)),
          ("allday", C, at(0, day = Day.plusDays(1))),
          ("tomorrow", B, at(7, 30, Day.plusDays(1)))
        ),
        (3, 0, 0)
      )
    }

    test(
      "a meeting is due Lead before its start, or at the scan when that is past; one with no start, at Undated on its day"
    ) {
      found(
        Vector(A, B, C),
        "soon" -> planned(Day, Some(LocalTime.of(9, 0))),
        "close" -> planned(Day, Some(LocalTime.of(8, 5)), Vector("c" -> B)),
        "allday" -> planned(Day, None, Vector("d" -> C))
      )._1.map((n, d) => (n, d.slot)) ==> Vector(
        ("close", At),
        ("soon", at(8, 50)),
        ("allday", at(8, 45))
      )
    }

    test("a meeting naming fewer than two people is not due") {
      due(
        Vector(A),
        "one" -> planned(Day, Some(LocalTime.of(9, 0)), who = Vector("Ana")),
        "none" -> planned(Day, Some(LocalTime.of(10, 0)), who = Vector.empty)
      ) ==> (Vector.empty, (2, 2, 0))
    }

    test(
      "its room is the listed one its topic's strands were most written from, the latest on a tie, and none listed none"
    ) {
      due(
        Vector(A, B, C, Place.Everywhere),
        "most" -> planned(Day, Some(LocalTime.of(9, 0)), Vector("m1" -> A, "m2" -> A, "m3" -> C)),
        "tie" -> planned(Day, Some(LocalTime.of(10, 0)), Vector("t1" -> A, "t2" -> C)),
        "once" -> planned(
          Day,
          Some(LocalTime.of(11, 0)),
          Vector("o1" -> Place.Everywhere, "o1" -> Place.Everywhere, "o2" -> B)
        )
      )._1.map((n, r, _) => (n, r)) ==> Vector(("most", A), ("tie", C), ("once", B))
    }

    test("a room not listed is passed over for the next listed one, or the meeting is not due") {
      due(
        Vector(B),
        "next" -> planned(Day, Some(LocalTime.of(9, 0)), Vector("n1" -> A, "n2" -> A, "n3" -> B)),
        "unlisted" -> planned(Day, Some(LocalTime.of(10, 0)), Vector("u1" -> C))
      ) ==> (Vector(("next", B, at(9))), (1, 0, 0))
    }

    test("only the soonest of each room is due, at most Asks, the rest left") {
      due(
        Vector(A, B, C, Place.Everywhere),
        "a1" -> planned(Day, Some(LocalTime.of(9, 0))),
        "a2" -> planned(Day, Some(LocalTime.of(8, 30))),
        "b" -> planned(Day, Some(LocalTime.of(10, 0)), Vector("b" -> B)),
        "c" -> planned(Day, Some(LocalTime.of(11, 0)), Vector("c" -> C)),
        "e" -> planned(Day, Some(LocalTime.of(12, 0)), Vector("e" -> Place.Everywhere))
      ) ==> (
        Vector(("a2", A, at(8, 30)), ("b", B, at(10)), ("c", C, at(11))),
        (5, 0, 2)
      )
    }
  }
}
