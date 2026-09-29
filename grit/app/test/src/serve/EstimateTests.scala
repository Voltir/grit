package grit.app.serve

import java.time.Instant

import grit.slack.event.{ChannelId, Event, TeamId, Ts, UserId}

import utest.*

/** [[Estimate.of]]: the bound printed before a backfill hears anything. */
object EstimateTests extends TestSuite {

  private def said(thread: String, ts: String, text: String): Event.Said =
    Event.Said(
      TeamId("T1"),
      ChannelId("C1"),
      Ts(ts),
      Ts(thread),
      UserId("U1"),
      text,
      false,
      Instant.EPOCH
    )

  val tests = Tests {
    test(
      "each message is triaged with the thread before it, cut to its last 2000 characters, and each thread written a closing"
    ) {
      val e = Estimate.of(
        Vector(
          said("1.0", "1.0", "a" * 100),
          said("2.0", "2.0", "c" * 10),
          said("1.0", "1.1", "b" * 50),
          said("3.0", "3.0", "x" * 3000),
          said("3.0", "3.1", "y" * 2)
        )
      )
      // Characters: 2100, 2010, 2150 (100 before it), 5000, 4002 (2000 of 3000 before it):
      // 15262, 3815.5 tokens at $0.042 a million.
      e ==> Estimate(5, 3, BigDecimal("0.000160251"), BigDecimal("0.0045"))
      e.total ==> BigDecimal("0.004660251")
    }
  }
}
