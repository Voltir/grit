package grit.turn

import java.time.{Instant, ZoneId}

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
  }
}
