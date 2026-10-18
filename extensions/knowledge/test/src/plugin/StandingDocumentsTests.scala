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

  /** The `n`th close (ordinal `n`, on October `n`th at noon) of `conversation`, at `origin`:
    * `changes` made, leaving `kept`.
    */
  private def closed(
      n: Long,
      changes: Vector[Change],
      kept: Vector[Line],
      reason: CloseReason = CloseReason.Resolved(Probability.One),
      conversation: String = "c1",
      origin: Origin = thread,
      outcome: String = "Staging deploys from main."
  ): ClosedPeriod =
    ClosedPeriod(
      PeriodRef(ConversationId(conversation), PeriodSeq.First),
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

  val tests = Tests {
    test(
      "each Standing line an earned closing adds is kept under its conversation and line, at the conversation's label, believed at the closing's integrity, at its thread, written at its close"
    ) {
      val kept = new Kept
      val topic = TestClosings.line(Section.Topics, "deploys", 1, 1)
      kept.post(
        closed(1, Vector(Change.Added(deploys), Change.Added(topic)), Vector(deploys, topic)),
        TestLabels.Trial
      )
      kept.newest.map(d =>
        (key(d), d.label, d.integrity, d.place, d.written, d.pointer)
      ) ==> Vector(
        (
          s"standing:c1/${id(deploys)}",
          TestLabels.Trial,
          Integrity.Outside,
          thread.place,
          Instant.parse("2026-10-01T12:00:00Z"),
          None
        )
      )
    }

    test(
      "a document's text is its line on its ground, one only the assistant said as not confirmed, then what its closing came to"
    ) {
      val kept = new Kept
      val claimed = standing("Port 3000 is the usual choice", 1, Ground.Claimed)
      val lines = Vector(deploys, freeze, claimed)
      kept.post(closed(1, lines.map(Change.Added(_)), lines))
      kept.texts.sorted ==> Vector(
        "Hotfixes skip the freeze (a tool's result showed it)\n" +
          "From a conversation that came to: Staging deploys from main.",
        "Open: whether \"Port 3000 is the usual choice\" (the assistant said so; nothing confirmed it)\n" +
          "From a conversation that came to: Staging deploys from main.",
        "Staging deploys from main (a person said so)\n" +
          "From a conversation that came to: Staging deploys from main."
      )
    }

    test("a kept line is shown to a window as a document under the plugin's label") {
      val kept = new Kept
      kept.post(closed(1, Vector(Change.Added(deploys)), Vector(deploys)))
      kept.newest.map(Shown.document(_, StandingDocuments.Terms.label)) ==> Vector(
        Message.User(
          "[doc] standing: a decision or fact a conversation settled, at slack:T1/C1/1.0, " +
            "2026-10-01:\nStaging deploys from main (a person said so)\n" +
            "From a conversation that came to: Staging deploys from main."
        )
      )
    }

    test(
      "a later closing that resolves, drops or evicts a line withdraws its document, and leaves the rest"
    ) {
      val kept = new Kept
      val third = standing("Reviews need two approvals", 1)
      val fourth = standing("The bot posts in #deploys", 1)
      val all = Vector(deploys, freeze, third, fourth)
      kept.post(closed(1, all.map(Change.Added(_)), all))
      kept.post(
        closed(
          2,
          Vector(
            Change.Resolved(deploys, "moved to a release branch"),
            Change.Dropped(freeze, "no longer true"),
            Change.Evicted(third)
          ),
          Vector(fourth),
          reason = CloseReason.Lapsed
        )
      )
      kept.newest.map(key(_)) ==> Vector(s"standing:c1/${id(fourth)}")
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
      kept.post(closed(1, Vector(Change.Added(deploys)), Vector(deploys)))
      val first = kept.newest.map(_.version)
      kept.post(
        closed(
          2,
          Vector(Change.Added(freeze)),
          Vector(deploys, freeze),
          outcome = "Hotfixes skip the freeze."
        )
      )
      (first.size, kept.newest.filter(d => key(d).endsWith(id(deploys))).map(_.version)) ==>
        (1, first)
    }

    test("a period closed unearned, or a job's run, keeps nothing") {
      val kept = new Kept
      kept.post(closed(1, Vector(Change.Added(deploys)), Vector(deploys), CloseReason.Unearned))
      kept.post(
        closed(
          2,
          Vector(Change.Added(freeze)),
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
      kept.post(closed(1, Vector(Change.Added(deploys)), Vector(deploys)))
      kept.post(
        closed(
          2,
          Vector(Change.Added(deploys)),
          Vector(deploys),
          conversation = "c2",
          origin = other
        )
      )
      kept.post(closed(3, Vector(Change.Resolved(deploys, "done")), Vector()))
      kept.newest.map(d => (key(d), d.place)) ==> Vector(
        (s"standing:c2/${id(deploys)}", other.place)
      )
    }
  }
}
