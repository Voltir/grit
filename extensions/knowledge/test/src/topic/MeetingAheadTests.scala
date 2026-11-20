package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}

import grit.core.act.ScriptedMoves.Replied
import grit.core.act.{Dreaming, MoveError, ScriptedDreaming}
import grit.core.classify.{Answer, Question, Request}
import grit.core.files.InMemoryPieces
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.place.Place
import grit.core.stitch.{StrandDigest, StrandWorld}
import grit.core.visibility.{Integrity, Label, TestLabels}
import grit.knowledge.ref.TopicKey
import grit.knowledge.topic.TestMeetings.{Kept, Name, Terms, doc}

import utest.*

/** A booked run of `meeting-ahead` over one meeting kept as a topics run keeps it, its moves
  * scripted ([[ScriptedDreaming]]): grit never speaking, so a named occasion is held `off`.
  */
object MeetingAheadTests extends TestSuite {

  private val Room: Place = TestLabels.place("slack:T1/C-a")
  private val Day: LocalDate = LocalDate.parse("2026-11-20")
  private val Nine: Instant = Day.atTime(9, 0).toInstant(ZoneOffset.UTC)
  private val Slot: Instant = Nine.minusSeconds(600)
  private val Key: TopicKey = TopicKey.strand(ConversationId("m")).fold(sys.error, identity)

  private def meeting(state: Meeting.State, who: Vector[String] = Vector("Ana", "Ben")): Topic =
    Topic.of(
      doc("A meeting"),
      Vector(
        Passage(
          Day.minusDays(1),
          Lineage.of(
            Vector(Lineage.Tag.Strand(ConversationId("m"), Room, StrandDigest.of(Vector.empty)))
          ),
          doc("Talked of.")
        )
      ),
      Topic.Kind.Meeting(Meeting.of(state, who))
    )

  private def planned(at: LocalTime): Meeting.State =
    Meeting.State.Planned(Meeting.When(Day, Some(at)))

  /** What a run booked for the meeting at nine replies, over `topic` kept, its sort judged
    * unsettled at `unsettled`, when its booking saw the version `booked` (the kept one when
    * none); and how many sorts it asked.
    */
  private def ran(
      topic: Topic,
      unsettled: Double = 1.0,
      booked: Option[DocumentVersionId] = None
  ): (String, Int) = {
    val kept = new Kept(("m", topic, Label.Public, Integrity.Member))
    kept.docs
      .declare(Vector(Name -> Terms))(using grit.dbos.sql.TestTx.fake)
      .fold(e => sys.error(e.toString), identity)
    val version = kept.read().headOption.map(_.version).getOrElse(sys.error("kept"))
    val sorts = new Sorts
    val judged: Request -> Either[MoveError, Replied[Vector[Answer]]] = r => {
      sorts.made += 1
      Right(
        Replied(
          r.questions.map {
            case c: Question.Choice =>
              Answer.Choice(
                "unsettled",
                c.keys.map(k =>
                  Answer.Weight(k.name, if (k.name == "unsettled") unsettled else 1 - unsettled)
                ),
                1.0
              )
            case _ => Answer.YesNo(0.0)
          },
          Label.Public
        )
      )
    }
    val jsons: (String, Vector[Message]) -> Either[MoveError, Replied[ujson.Value]] =
      (_, _) => Left(MoveError.Model("no JSON"))
    val w = new StrandWorld
    val job = new MeetingAhead(ZoneOffset.UTC)
    val moves = new ScriptedDreaming(
      job.limits,
      Label.Public,
      Name,
      Terms,
      Dreaming.Reads(
        w.shelf,
        _ => Needs.over(Name, Vector.empty),
        w.excerpts,
        kept.docs,
        new InMemoryPieces
      ),
      jsons,
      documents = kept.docs,
      at = Slot,
      judgments = judged
    )
    val due = MeetingsAhead.Due(Key, Day, booked.getOrElse(version), Room, Slot, Nine)
    val reply = job.run(TestTainted.trusted(JobRun(due, Slot, Slot), Integrity.Member), moves)
    (reply, sorts.made)
  }

  /** How many sorts a run asked. */
  private final class Sorts {
    // Counted by the fake's judgments, on the test's one thread; read after the run.
    @caps.unsafe.untrackedCaptures
    var made: Int = 0
  }

  val tests = Tests {
    test("a meeting as booked names its occasion, asking no sort") {
      ran(meeting(planned(LocalTime.of(9, 0)))) ==> ("Meeting ahead: held (off).", 0)
    }

    test("a meeting held, moved, or naming one person names none") {
      (
        ran(meeting(Meeting.State.Held(Day))),
        ran(meeting(planned(LocalTime.of(10, 0)))),
        ran(meeting(planned(LocalTime.of(9, 0)), Vector("Ana")))
      ) ==> (
        ("Meeting ahead: no longer due.", 0),
        ("Meeting ahead: no longer due.", 0),
        ("Meeting ahead: no longer due.", 0)
      )
    }

    test(
      "a topic written since its booking is sorted again: named when unsettled, not when settled"
    ) {
      val stale = DocumentVersionId.of(999).getOrElse(sys.error("a version"))
      (
        ran(meeting(planned(LocalTime.of(9, 0))), booked = Some(stale)),
        ran(meeting(planned(LocalTime.of(9, 0))), unsettled = 0.2, booked = Some(stale))
      ) ==> (("Meeting ahead: held (off).", 1), ("Meeting ahead: judged settled.", 1))
    }
  }
}
