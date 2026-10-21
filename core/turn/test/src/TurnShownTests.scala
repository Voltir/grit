package grit.turn

import java.time.{Instant, ZoneId, ZoneOffset}

import utest.*

/** What [[TurnShown]] tells a turn's model besides its window: the line dating its message. */
object TurnShownTests extends TestSuite {

  private val At = Instant.parse("2026-10-21T15:03:00Z")

  private val (pacific, eastern) = (ZoneId.of("America/Los_Angeles"), ZoneId.of("America/New_York"))

  /** The tail every zoned line carries, its times read as `short`. */
  private def tail(short: String): String =
    s"People here give times in their own zone, not UTC: read the times the person gives as $short " +
      s"unless they say otherwise, and answer with times in $short. Times grit shows you in your " +
      "view are UTC."

  val tests = Tests {
    test(
      "the answering line dates the message in the person's zone, says whose, then UTC, and how to read times"
    ) {
      val personal = "The message you are answering was said Wed 2026-10-21 08:03 PDT, the " +
        s"person's time (America/Los_Angeles); 15:03 UTC. ${tail("PDT")}"
      (
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, pacific)), direct = true),
        TurnShown.answering(At, None, direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = false)
      ) ==> (
        personal,
        "The message you are answering was said Wed 2026-10-21 08:03 PDT, this deployment's " +
          "time (America/Los_Angeles); the person's own zone is not known; 15:03 UTC. " +
          tail("PDT"),
        "The message you are answering was said Wed 2026-10-21 15:03 UTC.",
        s"$personal Read times other people here give as EDT, this deployment's time, unless " +
          "they say otherwise."
      )
    }

    test("the UTC time carries its date when it falls on another day than the dated one") {
      TurnShown.answering(
        Instant.parse("2026-10-22T06:30:00Z"),
        Some(TurnOffer.Zoned(Some(pacific), eastern)),
        direct = true
      ) ==> "The message you are answering was said Wed 2026-10-21 23:30 PDT, the person's time " +
        s"(America/Los_Angeles); 06:30 UTC Thu 2026-10-22. ${tail("PDT")}"
    }

    test("a deployment in UTC, under any of its names, is said as UTC") {
      Vector(ZoneOffset.UTC, ZoneId.of("UTC"), ZoneId.of("Etc/UTC")).map(z =>
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, z)), direct = true)
      ) ==> Vector.fill(3)(
        "The message you are answering was said Wed 2026-10-21 15:03 UTC, this deployment's " +
          "time (UTC); the person's own zone is not known; 15:03 UTC. " + tail("UTC")
      )
    }
  }
}
