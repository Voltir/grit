package grit.knowledge.drive

import java.time.{Instant, LocalTime, ZoneId}

import grit.core.act.Answer
import grit.core.id.{CorpusName, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.place.Service
import grit.core.tool.{Outcome, ToolName}
import grit.core.triage.Corpus
import grit.core.visibility.{Compartment, Label, Level}
import grit.knowledge.card.UnitVersion
import grit.knowledge.sync.{Fetched, Step, SyncSchedule, Way}

import utest.*

object DriveCardsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  val service: Service = right(Service.of("drive"))
  val corpus: Corpus =
    Corpus(right(CorpusName.of("shared")), "the team's shared drives", service.place, Some(service))
  val list: ToolName = right(ToolName.of("drive_list"))
  val read: ToolName = right(ToolName.of("drive_read"))
  private val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))
  val nightly: SyncSchedule = right(SyncSchedule.of(right(ScheduleKey.of("nightly")), rule, 5))

  val internal: Label = Label.at(Level.Internal)
  val confidential: Label = Label.at(Level.Confidential)
  private val team = right(Compartment.of("team"))
  private val acme = right(Compartment.of("acme"))

  /** A source at `labels`, holding at most `maxUnits` cards. */
  def cards(labels: Vector[Label], maxUnits: Int = 100): DriveCards =
    right(DriveCards.of(corpus, list, read, labels, maxUnits, service.place, Vector(nightly)))

  private def refusal(labels: Vector[Label]): Either[String, DriveCards] =
    DriveCards.of(corpus, list, read, labels, 100, service.place, Vector(nightly))

  val tests = Tests {
    test("a file is carded at the least declared label dominating what its listing was read at") {
      val source = cards(Vector(confidential, internal))
      assert(source.carding(Label.Public) == Some(internal))
      assert(source.carding(internal) == Some(internal))
      assert(source.carding(Label.at(Level.Confidential)) == Some(confidential))
      assert(source.carding(Label.at(Level.Internal, team)) == None)
      assert(source.carding(Label.at(Level.Restricted)) == None)
    }

    test("labels that are not a chain are refused, naming two that neither dominates") {
      val got = refusal(Vector(Label.at(Level.Internal, team), Label.at(Level.Internal, acme)))
      assert(
        got == Left(
          "a Drive source's labels are a chain, each dominating the one below: " +
            s"${Label.written(Label.at(Level.Internal, team))} and " +
            s"${Label.written(Label.at(Level.Internal, acme))} are not"
        )
      )
    }

    test("a label holding what no one labelled is refused") {
      val marked = confidential.join(Label.Unmapped)
      val got = refusal(Vector(internal, marked))
      assert(
        got == Left(
          "a Drive source's label is never one holding what no one labelled: " +
            Label.written(marked)
        )
      )
    }

    test("no labels, or one twice, is refused") {
      assert(refusal(Vector.empty) == Left("a Drive source has at least one label"))
      assert(
        refusal(Vector(internal, internal)) == Left("a Drive source's labels are each given once")
      )
    }

    test("the shared drives are listed with no arguments, a drive or folder by its id, a file read by its id") {
      val source = cards(Vector(internal))
      assert(source.call(Step.List(DriveWalk.Drives, Way.Run)) == (list, ujson.Obj()))
      assert(
        source.call(Step.List("folder-3", Way.Through("drive-a"))) ==
          (list, ujson.Obj("folder" -> "folder-3"))
      )
      assert(
        source.call(Step.Read("file-text", Way.Through("folder-3"))) ==
          (read, ujson.Obj("file" -> "file-text"))
      )
    }

    test("a read's facts give its text at the time it last changed, its answer's text unread") {
      val stated = FactsGoldens.stated("read-file-text")
      val got = cards(Vector(internal)).fetched(Answer("not the facts", stated))
      assert(
        got == Fetched.Text(
          "text text.\ntext text, text.",
          UnitVersion.Changed(Instant.parse("2026-09-13T10:00:00Z"))
        )
      )
    }

    test("a read cut for size is carded from what it gave") {
      val v = FactsGoldens.json("read-file-text")
      v("parts").arr(0)("complete") = false
      val got = cards(Vector(internal)).fetched(Answer("", FactsGoldens.stated(v)))
      assert(
        got == Fetched.Text(
          "text text.\ntext text, text.",
          UnitVersion.Changed(Instant.parse("2026-09-13T10:00:00Z"))
        )
      )
    }

    test("a read stating no facts is unversioned, saying why") {
      val got = cards(Vector(internal)).fetched(Answer("text", Outcome.Stated.None))
      assert(got == Fetched.Unversioned("a read states one part, not 0"))
    }
  }
}
