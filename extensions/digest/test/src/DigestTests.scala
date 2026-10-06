package grit.digest

import java.time.Instant

import scala.util.chaining.*

import grit.core.document.{DocText, Document, DocumentKeeper, InMemoryDocuments}
import grit.core.id.{
  CallSlot,
  ConversationId,
  PeriodRef,
  PeriodSeq,
  PluginName,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.message.AssistantBlock
import grit.core.period.{CloseOrdinal, CloseReason, Closing, Probability, TestClosings}
import grit.core.place.{Directory, Place}
import grit.core.plugin.{InMemoryPlugins, Needs, PluginReads}
import grit.core.store.{ClosedPeriod, Db, Origin, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.dbos.sql.TestTx

import utest.*

object DigestTests extends TestSuite {

  /** The call each test's tool is run as. */
  private val slot: CallSlot = CallSlot
    .of(TurnRef(ConversationId("c"), TurnSeq.First), 0, 0)
    .getOrElse(throw new java.lang.AssertionError("a slot"))

  private val name = PluginName.of("digest").getOrElse(throw new java.lang.AssertionError("name"))

  final class FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  private def closing(prose: String, outcome: Option[String]): Closing =
    TestClosings.prose(prose, outcome)

  private def closed(
      n: Long,
      origin: Origin,
      reason: CloseReason,
      c: Closing,
      at: String
  ): ClosedPeriod =
    ClosedPeriod(
      PeriodRef(ConversationId("c"), PeriodSeq.First),
      origin,
      reason,
      c,
      Instant.parse(at),
      CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))
    )

  private val home: Directory = Directory.of("/home/nick").fold(e => sys.error(e), identity)

  /** A closing at the task room `nightly`, run `n`, closed on September `n`th at 03:00. */
  private def nightly(n: Long): ClosedPeriod =
    closed(
      n,
      Origin.Task("nightly", s"run $n"),
      CloseReason.Lapsed,
      closing(s"Run $n.", None),
      f"2026-09-${n}%02dT03:00:00Z"
    )

  private def nightlyLine(n: Long): String =
    f"2026-09-${n}%02d 03:00 · task nightly · lapsed · Run $n."

  private val periods: Vector[ClosedPeriod] = Vector(
    closed(
      1,
      Origin.Tui(home, "default"),
      CloseReason.Resolved(Probability.One),
      closing("We set up staging. It works.", Some("staging deploys from main")),
      "2026-09-20T14:05:00Z"
    ),
    closed(
      2,
      Origin.Slack("T1", "eng", "1700.1"),
      CloseReason.Lapsed,
      closing("Someone asked about the flaky test. Nobody knew.", None),
      "2026-09-21T09:30:00Z"
    ),
    closed(
      3,
      Origin.Task("nightly", "2026-09-22"),
      CloseReason.Lapsed,
      closing("Nothing to report", None),
      "2026-09-22T03:00:00Z"
    )
  )

  /** A fresh digest's documents, and its keeper over them. */
  private final class Kept {
    val digest = new Digest(name)
    val docs = new InMemoryDocuments
    val keeper: DocumentKeeper = docs.keeper(name, Digest.Terms)

    def post(p: ClosedPeriod): Either[StoreError, Unit] =
      digest.documents.fold(Right(()))(_.post(p, keeper)(using TestTx.fake))

    def newest: Vector[Document] =
      keeper.newest(100)(using TestTx.fake).getOrElse(sys.error("in-memory store"))

    def reads: PluginReads = PluginReads(new InMemoryPlugins().docs(name), docs.shelf(name))

    def recent(n: Int): Either[StoreError, Vector[String]] =
      digest.service(reads).recent(n)(using TestTx.fake)
  }

  private def posted(ps: ClosedPeriod*): Kept =
    new Kept().tap(k => ps.foreach(p => k.post(p) ==> Right(())))

  /** A document's text, its header line left out. */
  private def lines(d: Document): Vector[String] =
    DocText.value(d.text).linesIterator.toVector.drop(1)

  private def run(db: FakeDb, kept: Kept, args: (String, ujson.Value)*): Outcome =
    Digest.RecentActivity
      .bind(kept.reads, Needs.over(name, Vector.empty))
      .fold(u => sys.error(u.toString), identity)
      .pipe(r =>
        Toolbox.of[caps.CapSet^{db}](Digest.RecentActivity.described.over(n => r.run(n, db)))
      )
      .fold(d => sys.error(d.toString), identity)
      .bind(
        AssistantBlock.ToolCall(ToolCallId("c1"), "recent_activity", ujson.Obj.from(args)),
        Repairs.All
      ) match {
      case Right(free: Bound.Free) => free(slot)
      case other => sys.error(s"not free: $other")
    }

  val tests = Tests {
    test(
      "a closing's line goes into its room's document, kept at the room, newest first, written as of its close"
    ) {
      val later = closed(
        4,
        Origin.Tui(home, "other"),
        CloseReason.Lapsed,
        closing("We looked at logs. Nothing found.", None),
        "2026-09-23T08:00:00Z"
      )
      posted(periods(0), later).newest.map(d =>
        (d.place, DocText.value(d.text), d.written)
      ) ==> Vector(
        (
          Place.of(home),
          "Closed here, newest first (UTC):\n" +
            "2026-09-23 08:00 · tui other · lapsed · We looked at logs.\n" +
            "2026-09-20 14:05 · tui default · resolved · staging deploys from main",
          Instant.parse("2026-09-23T08:00:00Z")
        )
      )
    }

    test("a room's document holds its newest 10 closings' lines") {
      posted((1L to 12L).map(nightly)*).newest.map(lines) ==>
        Vector((12L to 3L by -1L).map(nightlyLine).toVector)
    }

    test("a closing already kept, or older than every line kept, posted again writes nothing") {
      val kept = posted((1L to 12L).map(nightly)*)
      val before = kept.newest.map(_.version)
      kept.post(nightly(12)) ==> Right(())
      kept.post(nightly(1)) ==> Right(())
      kept.newest.map(_.version) ==> before
    }

    // A listened channel's chatter closes unearned all day: a line each would bury the rest.
    test("a period closed unearned writes nothing") {
      val unearned = closed(
        4,
        Origin.Slack("T1", "eng", "1700.2"),
        CloseReason.Unearned,
        closing("Heard 3 messages; nothing kept.", None),
        "2026-09-22T04:00:00Z"
      )
      posted(unearned).newest ==> Vector()
    }

    test("two rooms keep two documents, each at its room") {
      posted(periods(1), periods(2)).newest.map(d => (d.place, lines(d))) ==> Vector(
        (
          Origin.Task("nightly", "x").room,
          Vector("2026-09-22 03:00 · task nightly · lapsed · Nothing to report")
        ),
        (
          Origin.Slack("T1", "eng", "x").room,
          Vector("2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test.")
        )
      )
    }

    test("recent merges every room's lines, newest first, as many as asked; none under 1") {
      val kept = posted((periods ++ Vector(nightly(23), nightly(24)))*)
      kept.recent(3) ==> Right(
        Vector(
          nightlyLine(24),
          nightlyLine(23),
          "2026-09-22 03:00 · task nightly · lapsed · Nothing to report"
        )
      )
      (kept.recent(0), kept.recent(-1)) ==> (Right(Vector()), Right(Vector()))
    }

    test("recent_activity lists the newest lines, as many as asked, newest first") {
      val db = new FakeDb
      val kept = posted(periods*)
      run(db, kept, "n" -> 2) ==> Outcome.Done(
        "2026-09-22 03:00 · task nightly · lapsed · Nothing to report\n" +
          "2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test."
      )
      run(db, kept) ==> Outcome.Done(
        "2026-09-22 03:00 · task nightly · lapsed · Nothing to report\n" +
          "2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test.\n" +
          "2026-09-20 14:05 · tui default · resolved · staging deploys from main"
      )
      run(db, new Kept) ==> Outcome.Done("No conversation has closed yet.")
    }

    test("recent_activity shows the newest 10 when not told how many") {
      run(new FakeDb, posted((1L to 12L).map(nightly)*)) ==> Outcome.Done(
        (12L to 3L by -1L).map(nightlyLine).mkString("\n")
      )
    }
  }
}
