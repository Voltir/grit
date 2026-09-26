package grit.digest

import java.time.Instant

import grit.core.id.{ConversationId, PeriodRef, PeriodSeq, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.period.{CloseOrdinal, CloseReason, Closing, Probability, TestClosings}
import grit.core.plugin.{InMemoryPlugins, PluginName}
import grit.core.store.{ClosedPeriod, Db, Origin, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.dbos.sql.TestTx

import utest.*

object DigestTests extends TestSuite {

  private val name = PluginName.of("digest").getOrElse(throw new java.lang.AssertionError("name"))

  final class FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using TestTx.fake)
  }

  private def closing(prose: String, outcome: Option[String]): Closing =
    TestClosings.prose(prose, outcome)

  private def closed(n: Long, origin: Origin, reason: CloseReason, c: Closing, at: String): ClosedPeriod =
    ClosedPeriod(
      PeriodRef(ConversationId("c"), PeriodSeq.First),
      origin,
      reason,
      c,
      Instant.parse(at),
      CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))
    )

  private val periods: Vector[ClosedPeriod] = Vector(
    closed(1, Origin.Tui("default"), CloseReason.Resolved(Probability.One), closing("We set up staging. It works.", Some("staging deploys from main")), "2026-09-20T14:05:00Z"),
    closed(2, Origin.Slack("T1", "eng", "1700.1"), CloseReason.Lapsed, closing("Someone asked about the flaky test. Nobody knew.", None), "2026-09-21T09:30:00Z"),
    closed(3, Origin.Task("nightly", "2026-09-22"), CloseReason.Lapsed, closing("Nothing to report", None), "2026-09-22T03:00:00Z")
  )

  /** Every period posted to a fresh digest. */
  private def posted: InMemoryPlugins = {
    val plugins = new InMemoryPlugins
    val digest = new Digest(name)
    periods.foreach(p => digest.post(p, plugins.docs(name))(using TestTx.fake))
    plugins
  }

  private def run(db: FakeDb, plugins: InMemoryPlugins, args: (String, ujson.Value)*): Outcome =
    Toolbox.of[{db}](Digest.recentActivity(db, plugins.docs(name))).fold(d => sys.error(d.toString), identity)
      .bind(AssistantBlock.ToolCall(ToolCallId("c1"), "recent_activity", ujson.Obj.from(args)), Repairs.All) match {
      case Right(free: Bound.Free) => free()
      case other => sys.error(s"not free: $other")
    }

  val tests = Tests {
    test("a closed period's line: when, where, why, and its outcome or first sentence") {
      val docs = posted.docs(name)
      docs.newest("", 10)(using TestTx.fake).map(_.map((k, v) => k -> Digest.shown(v))) ==> Right(
        Vector(
          "00000000000000000003" -> Some("2026-09-22 03:00 · task nightly · lapsed · Nothing to report"),
          "00000000000000000002" -> Some("2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test."),
          "00000000000000000001" -> Some("2026-09-20 14:05 · tui default · resolved · staging deploys from main")
        )
      )
    }

    test("recent_activity lists the newest lines, as many as asked, newest first") {
      val db = new FakeDb
      val plugins = posted
      run(db, plugins, "n" -> 2) ==> Outcome.Done(
        "2026-09-22 03:00 · task nightly · lapsed · Nothing to report\n" +
          "2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test."
      )
      run(db, plugins) ==> Outcome.Done(
        "2026-09-22 03:00 · task nightly · lapsed · Nothing to report\n" +
          "2026-09-21 09:30 · slack #eng · lapsed · Someone asked about the flaky test.\n" +
          "2026-09-20 14:05 · tui default · resolved · staging deploys from main"
      )
      run(db, new InMemoryPlugins) ==> Outcome.Done("No conversation has closed yet.")
    }
  }
}
