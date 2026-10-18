package grit.knowledge.plugin

import java.time.Instant

import grit.core.context.Shown
import grit.core.document.{DocText, Document, DocumentKeeper, InMemoryDocuments}
import grit.core.id.{ConversationId, DocKey, LineId, PeriodRef, PeriodSeq, PluginName}
import grit.core.message.Message
import grit.core.period.{
  Change,
  CloseOrdinal,
  CloseReason,
  Closing,
  Flows,
  Ground,
  Line,
  Probability,
  Section,
  TestClosings
}
import grit.core.store.{ClosedPeriod, Origin}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.dbos.sql.TestTx

import utest.*

object StandingDocumentsTests extends TestSuite {

  private val name =
    PluginName.of("standing").getOrElse(throw new java.lang.AssertionError("name"))

  private val thread = Origin.Slack("T1", "C1", "1.0")
  private val other = Origin.Slack("T1", "C1", "2.0")

  private def standing(text: String, since: Long, ground: Ground = Ground.Person): Line =
    TestClosings.line(Section.Standing, text, since, since, ground = ground)

  /** The `n`th close (ordinal `n`, on October `n`th at noon) of `conversation`'s `period`, at
    * `origin`: `changes` made, leaving `kept`.
    */
  private def closed(
      n: Long,
      changes: Vector[Change],
      kept: Vector[Line],
      reason: CloseReason = CloseReason.Resolved(Probability.One),
      period: Long = 1,
      conversation: String = "c1",
      origin: Origin = thread,
      outcome: String = "Staging deploys from main."
  ): ClosedPeriod =
    ClosedPeriod(
      PeriodRef(
        ConversationId(conversation),
        PeriodSeq.of(period).getOrElse(throw new java.lang.AssertionError(period))
      ),
      origin,
      reason,
      Closing(
        Flows
          .of("We talked about deploys.", Some(outcome), changes)
          .getOrElse(throw new java.lang.AssertionError("flows")),
        TestClosings.balance(kept*)
      ),
      Instant.parse(f"2026-10-${n}%02dT12:00:00Z"),
      CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n)),
      Integrity.Outside
    )

  /** A fresh plugin's documents, and its keeper over them. */
  private final class Kept {
    val docs = new InMemoryDocuments
    val keeper: DocumentKeeper = docs.keeper(name, StandingDocuments.Terms)

    def post(p: ClosedPeriod, label: Label = Label.Public): Unit =
      new StandingDocuments(name).documents
        .flatMap(_.posting)
        .fold(Right(()))(
          _.post(p, keeper)(using TestTx.fake(Clearance.inRoom(p.origin.room, label, label)))
        ) ==> Right(())

    def newest: Vector[Document] =
      keeper
        .newest(100)(using TestTx.fake(Clearance.of(TestLabels.Trialled.compartments.top)))
        .getOrElse(sys.error("in-memory store"))

    def texts: Vector[String] = newest.map(d => DocText.value(d.text))
  }

  private def key(d: Document): String = DocKey.value(d.key)

  private def id(l: Line): String = LineId.value(l.id)

  private val deploys = standing("Staging deploys from main", 1)
  private val freeze = standing("Hotfixes skip the freeze", 1, Ground.Tool)
  private val claimed = standing("Port 3000 is the usual choice", 1, Ground.Claimed)

  private def added(lines: Line*): Vector[Change] = lines.toVector.map(Change.Added(_))

  val tests = Tests {
    test(
      "a closing's person- and tool-grounded lines are one document under its conversation and period: its headline, then a line each, at the conversation's label, believed at the closing's integrity, at its thread, written at its close"
    ) {
      val kept = new Kept
      val topic = TestClosings.line(Section.Topics, "deploys", 1, 1)
      kept.post(
        closed(1, added(deploys, topic, freeze), Vector(deploys, topic, freeze)),
        TestLabels.Trial
      )
      kept.newest.map(d =>
        (key(d), d.label, d.integrity, d.place, d.written, d.pointer, DocText.value(d.text))
      ) ==> Vector(
        (
          "standing:c1/1",
          TestLabels.Trial,
          Integrity.Outside,
          thread.place,
          Instant.parse("2026-10-01T12:00:00Z"),
          None,
          "Staging deploys from main.\n- Staging deploys from main\n- Hotfixes skip the freeze"
        )
      )
    }

    test("a claimed line is left out, and a closing of only claimed lines keeps nothing") {
      val kept = new Kept
      kept.post(closed(1, added(deploys, claimed), Vector(deploys, claimed)))
      kept.post(closed(2, added(claimed), Vector(claimed), conversation = "c2", origin = other))
      kept.texts ==> Vector("Staging deploys from main.\n- Staging deploys from main")
    }

    test(
      "the data names the conversation, the period, the adding closing's day, its headline and each line with its ground"
    ) {
      val kept = new Kept
      kept.post(closed(1, added(deploys, freeze), Vector(deploys, freeze)))
      kept.newest.map(_.data) ==> Vector(
        ujson.Obj(
          "conversation" -> "c1",
          "period" -> 1,
          "day" -> "2026-10-01",
          "headline" -> "Staging deploys from main.",
          "lines" -> ujson.Arr(
            ujson.Obj("line" -> id(deploys), "ground" -> "person"),
            ujson.Obj("line" -> id(freeze), "ground" -> "tool")
          )
        )
      )
    }

    test("a kept document is shown to a window under the plugin's label") {
      val kept = new Kept
      kept.post(closed(1, added(deploys), Vector(deploys)))
      kept.newest.map(Shown.document(_, StandingDocuments.Terms.label)) ==> Vector(
        Message.User(
          "[doc] what a conversation settled, at slack:T1/C1/1.0, 2026-10-01:\n" +
            "Staging deploys from main.\n- Staging deploys from main"
        )
      )
    }

    test(
      "a later closing that resolves one of two lines writes the document again at its close without it, under the adding closing's headline and day; resolving the last withdraws it"
    ) {
      val kept = new Kept
      kept.post(closed(1, added(deploys, freeze), Vector(deploys, freeze)))
      kept.post(
        closed(
          2,
          Vector(Change.Resolved(deploys, "moved to a release branch")),
          Vector(freeze),
          period = 2,
          outcome = "Releases branch off main."
        )
      )
      val rewritten =
        kept.newest.map(d => (key(d), d.written, DocText.value(d.text), d.data("day").str))
      kept.post(closed(3, Vector(Change.Dropped(freeze, "no longer true")), Vector(), period = 3))
      (rewritten, kept.newest) ==> (
        Vector(
          (
            "standing:c1/1",
            Instant.parse("2026-10-02T12:00:00Z"),
            "Staging deploys from main.\n- Hotfixes skip the freeze",
            "2026-10-01"
          )
        ),
        Vector()
      )
    }

    test("a later closing closed unearned, or a job's run, still writes again or withdraws") {
      val kept = new Kept
      val third = standing("Reviews need two approvals", 1)
      kept.post(closed(1, added(deploys, freeze, third), Vector(deploys, freeze, third)))
      kept.post(
        closed(
          2,
          Vector(Change.Evicted(deploys)),
          Vector(freeze, third),
          CloseReason.Unearned,
          period = 2
        )
      )
      val rewritten = kept.texts
      kept.post(
        closed(
          3,
          Vector(Change.Resolved(freeze, "done"), Change.Dropped(third, "gone")),
          Vector(),
          CloseReason.Ran,
          period = 3
        )
      )
      (rewritten, kept.newest) ==> (
        Vector(
          "Staging deploys from main.\n- Hotfixes skip the freeze\n- Reviews need two approvals"
        ),
        Vector()
      )
    }

    test("a line added and refused at once, or added in another section, keeps nothing") {
      val kept = new Kept
      val open = TestClosings.line(Section.Open, "Who owns staging?", 1, 1)
      kept.post(
        closed(
          1,
          Vector(Change.Added(deploys), Change.Refused(deploys), Change.Added(open)),
          Vector(open)
        )
      )
      kept.newest ==> Vector()
    }

    test("a line a later closing carries unchanged writes no new version") {
      val kept = new Kept
      kept.post(closed(1, added(deploys), Vector(deploys)))
      val first = kept.newest.map(_.version)
      val later = standing("Hotfixes skip the freeze", 2, Ground.Tool)
      kept.post(
        closed(2, added(later), Vector(deploys, later), period = 2, outcome = "Hotfixes skip it.")
      )
      kept.newest.filter(key(_) == "standing:c1/1").map(_.version) ==> first
    }

    test("a period closed unearned, or a job's run, keeps nothing") {
      val kept = new Kept
      kept.post(closed(1, added(deploys), Vector(deploys), CloseReason.Unearned))
      kept.post(
        closed(
          2,
          added(freeze),
          Vector(freeze),
          CloseReason.Ran,
          conversation = "c2",
          origin = Origin.Task("nightly", "run 2")
        )
      )
      kept.newest ==> Vector()
    }

    test(
      "the same line in two conversations is two documents, and resolving one leaves the other"
    ) {
      val kept = new Kept
      kept.post(closed(1, added(deploys), Vector(deploys)))
      kept.post(closed(2, added(deploys), Vector(deploys), conversation = "c2", origin = other))
      kept.post(closed(3, Vector(Change.Resolved(deploys, "done")), Vector(), period = 2))
      kept.newest.map(d => (key(d), d.place)) ==> Vector(("standing:c2/1", other.place))
    }
  }
}
