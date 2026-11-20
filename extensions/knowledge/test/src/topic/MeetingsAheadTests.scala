package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneId, ZoneOffset}

import grit.core.act.{MoveError, ScriptedKeeping}
import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.job.JobRun
import grit.core.occasion.{Occasion, OccasionKey}
import grit.core.place.Place
import grit.core.stitch.StrandDigest
import grit.core.visibility.{Integrity, Label, Level, Recorded, TestLabels}

import utest.*

/** The job that names a meeting's occasions: which meetings are due on a run's day, where, under
  * what key and until when; and what a run replies of the moves it made.
  */
object MeetingsAheadTests extends TestSuite {
  import TestMeetings.{Kept, meeting, talkedIn}

  private val Day = LocalDate.of(2026, 11, 20)
  private val Nominal = Instant.parse("2026-11-20T08:30:00Z")
  private val Old = TestLabels.place("slack:T1/C-old")
  private val Room = TestLabels.place("slack:T1/C-plan")
  private val Floor = Label.at(Level.Internal)

  private def key(k: String): OccasionKey =
    OccasionKey.of(k).getOrElse(throw new java.lang.AssertionError(k))

  /** `t`'s passages, the first written from a strand of [[Old]], the rest of [[Room]]. */
  private def movedTo(t: Topic): Topic = {
    val strand = (p: Passage, room: Place) =>
      p.copy(from =
        Lineage.of(
          Vector(
            Lineage.Tag.Strand(ConversationId(s"s-${p.day}"), room, StrandDigest.of(Vector.empty))
          )
        )
      )
    Topic.of(
      t.lead,
      t.passages.zipWithIndex.map((p, i) => strand(p, if (i == 0) Old else Room)),
      t.kind
    )
  }

  private def kept(topics: (String, Topic)*): Vector[Document] =
    Kept(topics.map((n, t) => (n, t, Floor, Integrity.Member))*).read(Floor)

  private def planned(start: Option[LocalTime], day: LocalDate = Day): Meeting.State =
    Meeting.State.Planned(Meeting.When(day, start))

  private val Undated = Meeting.State.Proposed(None)

  private def version(docs: Vector[Document], i: Int): DocumentVersionId =
    docs.lift(i).map(_.version).getOrElse(throw new java.lang.AssertionError(s"no document $i"))

  /** [[Room]] labelled at [[Floor]], so a run reads it. */
  private val AtFloor: Recorded =
    Recorded(Map(Room -> Recorded.Kept(None, Some(Floor), quiet = false, None)), Map.empty)

  /** The job, its days in `zone`, run for its slot at `nominal` at [[Floor]] over `docs`' store,
    * `rooms` labelled as recorded, grit not speaking: its reply.
    */
  private def ran(
      store: Kept,
      zone: ZoneId = ZoneOffset.UTC,
      nominal: Instant = Nominal,
      rooms: Recorded = AtFloor
  ): String = {
    val job = new MeetingsAhead(zone, 100)
    job.run(
      TestTainted.trusted(JobRun(AheadParams(), nominal, nominal)),
      new ScriptedKeeping(
        job.limits,
        Floor,
        TestMeetings.Name,
        TestMeetings.Terms,
        _ => Left(MoveError.Model("no ask")),
        (_, _, _) => Left(MoveError.Model("no call")),
        (_, _) => Left(MoveError.Model("no ask")),
        _ => Left(MoveError.Model("no judgment")),
        store.docs,
        Instant.EPOCH,
        Integrity.Authority,
        rooms
      )
    )
  }

  val tests = Tests {
    test(
      "a meeting planned for the run's day is Ahead in its newest strand's room, keyed ahead:{topic}:{day}, until its start; one with no start last, until the day's end"
    ) {
      val docs = kept(
        "allday" -> movedTo(
          meeting(planned(None), Vector.empty, "Offsite", Day.minusDays(3), Day.minusDays(1))
        ),
        "planning" -> movedTo(
          meeting(
            planned(Some(LocalTime.of(10, 0))),
            Vector("Ana"),
            "Planning",
            Day.minusDays(4),
            Day.minusDays(2)
          )
        )
      )
      val named = MeetingsAhead.named(docs, Day, ZoneOffset.UTC, Nominal)
      val planning =
        docs.indexWhere(d => grit.core.document.DocText.value(d.text).contains("Planning"))
      named ==> MeetingsAhead.Named(
        Vector(
          Occasion(
            Room,
            key("ahead:6bd54b9cf5232c4b:2026-11-20"),
            Occasion.Reason.Ahead,
            version(docs, planning),
            Instant.parse("2026-11-20T10:00:00Z")
          ),
          Occasion(
            Room,
            key(s"ahead:${MeetingsAhead.keyed(TopicKeys.of("allday"))}:2026-11-20"),
            Occasion.Reason.Ahead,
            version(docs, 1 - planning),
            Instant.parse("2026-11-21T00:00:00Z")
          )
        ),
        0
      )
    }

    test(
      "a meeting proposed with no day, last talked of 3 days before, is Open until 12 hours after the slot; one talked of 9 days before, or on the day, is not"
    ) {
      val docs = kept(
        "recent" -> talkedIn(meeting(Undated, Vector.empty, "Retro", Day.minusDays(3)), Room),
        "stale" -> talkedIn(meeting(Undated, Vector.empty, "Kickoff", Day.minusDays(9)), Room),
        "today" -> talkedIn(meeting(Undated, Vector.empty, "Review", Day), Room)
      )
      val recent = docs.indexWhere(d => grit.core.document.DocText.value(d.text).contains("Retro"))
      MeetingsAhead.named(docs, Day, ZoneOffset.UTC, Nominal) ==> MeetingsAhead.Named(
        Vector(
          Occasion(
            Room,
            key(s"open:${MeetingsAhead.keyed(TopicKeys.of("recent"))}"),
            Occasion.Reason.Open,
            version(docs, recent),
            Instant.parse("2026-11-20T20:30:00Z")
          )
        ),
        0
      )
    }

    test(
      "a meeting proposed for the day, held on it, or planned for another is neither Ahead nor Open"
    ) {
      val docs = kept(
        "tentative" -> talkedIn(
          meeting(
            Meeting.State.Proposed(Some(Meeting.When(Day, None))),
            Vector.empty,
            "Sync",
            Day.minusDays(2)
          ),
          Room
        ),
        "held" -> talkedIn(
          meeting(Meeting.State.Held(Day), Vector.empty, "Standup", Day.minusDays(2)),
          Room
        ),
        "tomorrow" -> talkedIn(
          meeting(planned(None, Day.plusDays(1)), Vector.empty, "Demo", Day.minusDays(2)),
          Room
        )
      )
      MeetingsAhead.named(docs, Day, ZoneOffset.UTC, Nominal) ==> MeetingsAhead.Named(
        Vector.empty,
        0
      )
    }

    test("a meeting talked of in no strand names no occasion") {
      val docs =
        kept("planning" -> meeting(planned(None), Vector.empty, "Planning", Day.minusDays(1)))
      MeetingsAhead.named(docs, Day, ZoneOffset.UTC, Nominal) ==> MeetingsAhead.Named(
        Vector.empty,
        0
      )
    }

    test("a topic of a key near a document key's longest is keyed in 16 hex digits") {
      val long = "x" * 177
      val docs = kept(
        long -> talkedIn(meeting(planned(None), Vector.empty, "Planning", Day.minusDays(1)), Room)
      )
      MeetingsAhead
        .named(docs, Day, ZoneOffset.UTC, Nominal)
        .occasions
        .map(o => OccasionKey.value(o.key)) ==>
        Vector("ahead:60dc4787cd06e889:2026-11-20")
    }

    test("ahead ones come first, and a run makes at most 3, counting the rest past") {
      val store = Kept(
        (Vector("o1", "o2", "o3").map(n =>
          (
            n,
            talkedIn(meeting(Undated, Vector.empty, s"Open $n", Day.minusDays(2)), Room),
            Floor,
            Integrity.Member
          )
        ) ++ Vector("a1", "a2").map(n =>
          (
            n,
            talkedIn(meeting(planned(None), Vector.empty, s"Ahead $n", Day.minusDays(2)), Room),
            Floor,
            Integrity.Member
          )
        ))*
      )
      (
        MeetingsAhead
          .named(store.read(Floor), Day, ZoneOffset.UTC, Nominal)
          .occasions
          .map(_.reason),
        ran(store)
      ) ==> (
        Vector.fill(2)(Occasion.Reason.Ahead) ++ Vector.fill(3)(Occasion.Reason.Open),
        "Meetings ahead for 2026-11-20: 0 started, 3 held (3 off), 0 already noted, 2 past the 3 a run."
      )
    }

    test("the day is the slot's in the job's zone, not the clock's") {
      val store = Kept(
        (
          "planning",
          talkedIn(meeting(planned(None), Vector.empty, "Planning", Day.minusDays(1)), Room),
          Floor,
          Integrity.Member
        )
      )
      // 20:00 UTC on the 19th is the 20th in Auckland; the scripted clock reads 1970.
      ran(store, ZoneId.of("Pacific/Auckland"), Instant.parse("2026-11-19T20:00:00Z")) ==>
        "Meetings ahead for 2026-11-20: 0 started, 1 held (1 off), 0 already noted, 0 past the 3 a run."
    }

    test(
      "an occasion in a room at another label than the run's is replied mislabelled, and the run goes on"
    ) {
      val elsewhere = TestLabels.place("slack:T1/C-board")
      val store = Kept(
        (
          "board",
          talkedIn(meeting(planned(None), Vector.empty, "Board", Day.minusDays(1)), elsewhere),
          Floor,
          Integrity.Member
        ),
        (
          "planning",
          talkedIn(meeting(planned(None), Vector.empty, "Planning", Day.minusDays(1)), Room),
          Floor,
          Integrity.Member
        )
      )
      val rooms = Recorded(
        Map(
          elsewhere -> Recorded.Kept(None, Some(Label.at(Level.Confidential)), quiet = false, None),
          Room -> Recorded.Kept(None, Some(Floor), quiet = false, None)
        ),
        Map.empty
      )
      ran(store, rooms = rooms) ==>
        "Meetings ahead for 2026-11-20: 0 started, 2 held (1 mislabelled, 1 off), 0 already noted, 0 past the 3 a run."
    }

    test("a run's reply counts what failed apart from what was held") {
      MeetingsAhead.reply(
        Day,
        Vector(Left(MoveError.Store("down")), Right(grit.core.act.Occasioned.Already)),
        1,
        0
      ) ==>
        "Meetings ahead for 2026-11-20: 0 started, 0 held, 1 already noted, 0 past the 3 a run, 2 failed (1 down, 1 unkeyed)."
    }

    test("what a run's keep read is recorded in its pinned form") {
      // A pin of the step record: a run in flight replays by it.
      val j = summon[Journaled[MeetingsAhead.Named]]
      val named = MeetingsAhead.Named(
        Vector(
          Occasion(
            Room,
            key("open:00ff"),
            Occasion.Reason.Open,
            DocumentVersionId.of(7).getOrElse(throw new java.lang.AssertionError("7")),
            Instant.parse("2026-11-20T20:30:00Z")
          )
        ),
        1
      )
      (j.encode(named), j.decode(j.encode(named))) ==> (
        s"""{"occasions":[{"room":"${Room.written}","key":"open:00ff","reason":"open","about":7,"until":"2026-11-20T20:30:00Z"}],"unkeyed":1}""",
        Right(named)
      )
    }

    test("a run's parameters write its day only when given, and a day that is no date is refused") {
      val job = new MeetingsAhead(ZoneOffset.UTC, 100)
      (
        job.write(AheadParams()),
        job.read(job.write(AheadParams(Some(Day)))),
        job.read(ujson.Obj()),
        job.read(ujson.Obj("day" -> "soon"))
      ) ==> (
        ujson.Obj(),
        Right(AheadParams(Some(Day))),
        Right(AheadParams(None)),
        Left("a meetings-ahead run's day is a date: \"soon\"")
      )
    }
  }

  /** The topic keys [[Kept]] gives its topics. */
  private object TopicKeys {
    def of(name: String): grit.knowledge.ref.TopicKey =
      grit.knowledge.ref.TopicKey.strand(ConversationId(name)).fold(sys.error, identity)
  }
}
