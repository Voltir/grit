package grit.app.serve

import java.time.{Instant, LocalDate}

import grit.core.id.{CloseRef, ConversationId, PeriodRef, PeriodSeq, TurnSeq}
import grit.dbos.engine.Swept

import scala.jdk.CollectionConverters.*

import utest.*

/** What `grit backfill` decides without Slack or the database. */
object BackfillTests extends TestSuite {

  private val close = CloseRef(
    PeriodRef(ConversationId("c1"), PeriodSeq.First),
    TurnSeq(0),
    Instant.parse("2026-09-27T10:00:00Z")
  )

  val tests = Tests {
    test(
      "drain stops only at a sweep made once no workflow is left that enqueues nothing"
    ) {
      val looks = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      var counts = List(2, 0, 1, 0)
      var sweeps = List(Swept(enqueued = Vector(close)), Swept.nothing)
      val made = Backfill.drain(
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
        Right(2),
        Vector("unfinished 2", "unfinished 0", "sweep", "unfinished 1", "unfinished 0", "sweep")
      )
    }

    test("the days read are GRIT_BACKFILL_DAYS, 2 when unset, refused unless a whole number above zero") {
      (
        Backfill.days(Map.empty),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> " 7 ")),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> "0")),
        Backfill.days(Map("GRIT_BACKFILL_DAYS" -> "1.5"))
      ) ==> (
        Right(2),
        Right(7),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '0'"),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '1.5'")
      )
    }

    test("a channel's line names it, its counts, the day it reads from, and each bound rounded up") {
      Backfill.line(
        "#standup (C123ABC456)",
        Estimate(812, 143, BigDecimal("0.0190001"), BigDecimal("0.2145")),
        LocalDate.parse("2026-09-26")
      ) ==> "backfill #standup (C123ABC456): 812 messages in 143 threads since 2026-09-26; up to $0.2336 (triage $0.0191, closings up to $0.2145)"
    }
  }
}
