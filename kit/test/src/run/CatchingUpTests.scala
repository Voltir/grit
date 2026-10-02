package grit.kit.run

import java.time.{Instant, LocalDate}

import scala.jdk.CollectionConverters.*

import grit.core.id.{CloseRef, ConversationId, PeriodRef, PeriodSeq, PluginName, SettleRef, TurnSeq}
import grit.core.period.CloseOrdinal
import grit.core.plugin.PostRef
import grit.dbos.engine.Swept

import utest.*

/** What a catch-up decides without an edge or the database. */
object CatchingUpTests extends TestSuite {

  private val close = CloseRef(
    PeriodRef(ConversationId("c1"), PeriodSeq.First),
    TurnSeq(0),
    Instant.parse("2026-09-27T10:00:00Z")
  )

  private val settle = SettleRef(close.period, TurnSeq(0), close.due)

  private val post = PostRef(
    PluginName.of("digest").getOrElse(throw new java.lang.AssertionError("a plugin name")),
    1,
    CloseOrdinal.of(1).getOrElse(throw new java.lang.AssertionError("an ordinal")),
    0
  )

  val tests = Tests {
    test(
      "drain sweeps until a sweep, each made once no workflow is queued or running, enqueues, asks and posts nothing"
    ) {
      val looks = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      var counts = List(2, 0, 1, 0)
      var sweeps = List(
        Swept(enqueued = Vector(close)),
        Swept(asked = Vector(settle)),
        Swept(posted = Vector(post)),
        Swept.nothing
      )
      val made = CatchingUp.drain(
        () => {
          looks.add("sweep")
          val s = sweeps.headOption.getOrElse(Swept.nothing)
          sweeps = sweeps.drop(1)
          Right(s)
        },
        () => {
          val n = counts.headOption.getOrElse(0)
          counts = counts.drop(1)
          looks.add(s"unfinished $n")
          Right(n)
        },
        () => ()
      )
      (made, looks.asScala.toVector) ==> (
        Right(4),
        Vector(
          "unfinished 2",
          "unfinished 0",
          "sweep",
          "unfinished 1",
          "unfinished 0",
          "sweep",
          "unfinished 0",
          "sweep",
          "unfinished 0",
          "sweep"
        )
      )
    }

    test(
      "a channel's line names it, its counts, the day it reads from, and each bound rounded up"
    ) {
      CatchingUp.line(
        "#standup (C123ABC456)",
        Estimate(812, 143, BigDecimal("0.0190001"), BigDecimal("0.2145")),
        LocalDate.parse("2026-09-26")
      ) ==> "backfill #standup (C123ABC456): 812 messages in 143 threads since 2026-09-26; up to $0.2336 (triage $0.0191, closings up to $0.2145)"
    }
  }
}
