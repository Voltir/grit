package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneId, ZoneOffset}

import grit.core.act.ScriptedMoves.Replied
import grit.core.act.{Dreaming, MoveError, ScriptedDreaming}
import grit.core.classify.{Answer as Judged, Question, Request}
import grit.core.files.InMemoryPieces
import grit.core.flow.TestTainted
import grit.core.job.JobRun
import grit.core.need.Needs
import grit.core.occasion.OccasionKey
import grit.core.place.Place
import grit.core.stitch.{Strand, StrandWorld}
import grit.core.store.Conversation
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx

import utest.*

/** The job that names an occasion for a meeting about to start: which meetings pass its checks,
  * which it sorts, what the occasion it names holds, and what a run replies. Its strands are
  * threads in memory ([[StrandWorld]]), its sorts scripted, and grit does not speak, so a named
  * occasion is held `off`.
  */
object MeetingsAheadTests extends TestSuite {
  import TestMeetings.{Kept, meeting}

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private val Day: LocalDate = StrandWorld.At.atZone(ZoneOffset.UTC).toLocalDate

  /** Five hours after the world's first message: the scan's instant, 14:00 UTC. */
  private val Now: Instant = StrandWorld.at(5 * 3600)

  private val Pair: Vector[String] = Vector("Ana", "Ben")

  /** A thread of `channel` in which someone said `says` `at` seconds after the world's start. */
  private def thread(w: StrandWorld, channel: String, says: String, at: Long = 0): Conversation = {
    val t = w.thread(channel, "1.0")
    w.say(t, says, at)
    t
  }

  /** `t`'s strand as a scan at [[Now]] digests it. */
  private def digested(w: StrandWorld, t: Conversation): Strand.Digested =
    right(w.shelf.units(StrandWorld.At.minusSeconds(1), Now, Strand.Cut.Default)(using TestTx.fake))
      .find(_.root == t.id)
      .getOrElse(throw new java.lang.AssertionError("the thread is no unit"))

  /** A meeting planned for `start` on [[Day]] (none: no start) naming `who`, its one passage
    * written from `from` as it was digested then.
    */
  private def planned(
      from: Strand.Digested,
      start: Option[LocalTime],
      who: Vector[String] = Pair,
      day: LocalDate = Day
  ): Topic = {
    val t = meeting(Meeting.State.Planned(Meeting.When(day, start)), who, "Planning call", day)
    val tag = Lineage.Tag.Strand(from.root, from.room, from.digest, from.id.first)
    Topic.of(t.lead, t.passages.map(_.copy(from = Lineage.of(Vector(tag)))), t.kind)
  }

  private val NoJson
      : (String, Vector[grit.core.message.Message]) -> Either[MoveError, Replied[ujson.Value]] =
    (_, _) => Left(MoveError.Model("no JSON ask"))

  private def store(topics: (String, Topic)*): Kept = {
    val kept = Kept(topics.map((n, t) => (n, t, Label.Public, Integrity.Member))*)
    right(kept.docs.declare(Vector(TestMeetings.Name -> TestMeetings.Terms))(using TestTx.fake))
    kept
  }

  /** Every sort answers `unsettled` at `p`, `settled` the rest. */
  private def sorting(p: Double): Request -> Either[MoveError, Replied[Vector[Judged]]] =
    r =>
      Right(
        Replied(
          r.questions.map {
            case c: Question.Choice =>
              Judged.Choice(
                if (p >= 0.5) "unsettled" else "settled",
                c.keys.map(k => Judged.Weight(k.name, if (k.name == "unsettled") p else 1 - p)),
                1.0
              )
            case _ => Judged.YesNo(0)
          },
          Label.Public
        )
      )

  /** A run at [[Now]] over `kept` and `w`'s strands, raising in `rooms`, its days in `zone`:
    * its reply.
    */
  private def ran(
      kept: Kept,
      w: StrandWorld,
      rooms: Vector[Place],
      unsettled: Double = 0.9,
      zone: ZoneId = ZoneOffset.UTC
  ): String = {
    val job = new MeetingsAhead(zone, 100)
    job.run(
      TestTainted.trusted(JobRun(AheadParams(rooms), Now, Now)),
      new ScriptedDreaming(
        job.limits,
        Label.Public,
        TestMeetings.Name,
        TestMeetings.Terms,
        Dreaming.Reads(
          w.shelf,
          _ => Needs.over(TestMeetings.Name, Vector.empty),
          w.excerpts,
          kept.docs,
          new InMemoryPieces
        ),
        NoJson,
        documents = kept.docs,
        at = Now,
        judgments = sorting(unsettled)
      )
    )
  }

  private def reply(
      within: Int,
      fewer: Int = 0,
      covered: Int = 0,
      left: Int = 0,
      settled: Int = 0,
      held: Int = 0,
      failed: String = ""
  ): String =
    s"Meetings ahead at 14:00: $within in its rooms starting within 3 hours, $fewer naming " +
      s"fewer than two people, $covered talked of in the last 3 hours, $left left for a later " +
      s"run; $settled judged settled, 0 started, " +
      (if (held == 0) "0 held" else s"$held held ($held off)") +
      s", 0 already noted$failed."

  val tests = Tests {
    test(
      "a meeting in a listed room starting within the lead, two named, untalked of lately, judged unsettled, is sorted and named"
    ) {
      val w = new StrandWorld
      val t = thread(w, "plan", "Planning call Monday at three?")
      val s = digested(w, t)
      ran(store("planning" -> planned(s, Some(LocalTime.of(15, 0)))), w, Vector(s.room)) ==>
        reply(within = 1, held = 1)
    }

    test(
      "the occasion it names is in its topic's room, keyed ahead:{hash}:{day}, until its start"
    ) {
      val due = MeetingsAhead.Due(
        right(grit.core.id.DocumentVersionId.of(7).toRight("v")),
        Place.Everywhere,
        Instant.parse("2026-10-12T15:00:00Z"),
        MeetingsAhead.key(
          right(grit.knowledge.ref.TopicKey.strand(grit.core.id.ConversationId("planning"))),
          Day
        )
      )
      val o = MeetingsAhead.occasion(due)
      (o.room, OccasionKey.value(o.key), o.brief, o.about, o.until) ==> (
        Place.Everywhere,
        "ahead:" + grit.core.id.ShortHash.of("topic:strand-planning") + ":2026-10-12",
        MeetingsAhead.Brief,
        due.version,
        due.start
      )
    }

    test(
      "a meeting in a room not listed is not looked at, and a run listing no room reads nothing"
    ) {
      val w = new StrandWorld
      val s = digested(w, thread(w, "plan", "Planning call?"))
      val kept = store("planning" -> planned(s, Some(LocalTime.of(15, 0))))
      (ran(kept, w, Vector(Place.Everywhere)), ran(kept, w, Vector.empty)) ==>
        (reply(within = 0), MeetingsAhead.Roomless)
    }

    test(
      "a meeting starting past the lead, one started already, and one with no start are not looked at"
    ) {
      val w = new StrandWorld
      val s = digested(w, thread(w, "plan", "Planning call?"))
      val kept = store(
        "later" -> planned(s, Some(LocalTime.of(17, 30))),
        "started" -> planned(s, Some(LocalTime.of(13, 30))),
        "unset" -> planned(s, None)
      )
      ran(kept, w, Vector(s.room)) ==> reply(within = 0)
    }

    test("a meeting naming fewer than two people is not sorted") {
      val w = new StrandWorld
      val s = digested(w, thread(w, "plan", "Planning call?"))
      val kept = store(
        "one" -> planned(s, Some(LocalTime.of(15, 0)), Vector("Ana")),
        "none" -> planned(s, Some(LocalTime.of(15, 30)), Vector.empty)
      )
      ran(kept, w, Vector(s.room)) ==> reply(within = 2, fewer = 2)
    }

    test(
      "a meeting whose strand was spoken in within Covered, or holds talk its topic has not read, is not sorted"
    ) {
      val w = new StrandWorld
      val recent = thread(w, "plan", "Planning call?", at = 4 * 3600)
      val moved = w.thread("other", "1.0")
      w.say(moved, "Planning call agenda?", 0)
      val before = digested(w, moved)
      w.say(moved, "Agenda is settled, see the doc.", 3600)
      val kept = store(
        "recent" -> planned(digested(w, recent), Some(LocalTime.of(15, 0))),
        "moved" -> planned(before, Some(LocalTime.of(15, 30)))
      )
      ran(kept, w, Vector(digested(w, recent).room, before.room)) ==>
        reply(within = 2, covered = 2)
    }

    test("a meeting judged settled names no occasion") {
      val w = new StrandWorld
      val s = digested(w, thread(w, "plan", "Planning call?"))
      ran(store("planning" -> planned(s, Some(LocalTime.of(15, 0)))), w, Vector(s.room), 0.2) ==>
        reply(within = 1, settled = 1)
    }

    test("only the soonest meeting of a room is sorted, and at most Asks a run") {
      val w = new StrandWorld
      val rooms = (1 to 4).map(i => digested(w, thread(w, s"room$i", s"Call $i?"))).toVector
      val topics = rooms.zipWithIndex.map((s, i) =>
        s"m$i" -> planned(s, Some(LocalTime.of(15, 0).plusMinutes(i.toLong)))
      ) :+ ("again" -> planned(rooms.head, Some(LocalTime.of(16, 0))))
      ran(store(topics*), w, rooms.map(_.room)) ==> reply(within = 5, left = 2, held = 3)
    }

    test("the scan's instant and a meeting's day are read in the job's zone") {
      val w = new StrandWorld
      val s = digested(w, thread(w, "plan", "Planning call?"))
      // 15:00 UTC is 04:00 the next day at +13:00; 15:00 there is 02:00 UTC, past the lead.
      val zone = ZoneOffset.ofHours(13)
      val kept = store(
        "utc" -> planned(s, Some(LocalTime.of(15, 0))),
        "there" -> planned(s, Some(LocalTime.of(4, 0)), day = Day.plusDays(1))
      )
      ran(kept, w, Vector(s.room), zone = zone) ==>
        reply(within = 1, held = 1).replace("at 14:00", "at 03:00")
    }

    test("a run's parameters are written as its rooms and read back") {
      val job = new MeetingsAhead(ZoneOffset.UTC, 1)
      val p = AheadParams(Vector(Place.Everywhere))
      (job.write(p), job.read(job.write(p))) ==>
        (ujson.Obj("rooms" -> ujson.Arr("everywhere")), Right(p))
    }
  }
}
