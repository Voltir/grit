package grit.core.job

import java.time.Instant

import utest.*

/** [[DeskRefusal.said]], the line a model is shown of a refusal. */
object DeskRefusalTests extends TestSuite {

  val tests = Tests {
    test("a refusal says its times as grit says every time to a model: UTC, to the second") {
      val (at, now) = (Instant.parse("2026-10-06T14:00:00Z"), Instant.parse("2026-10-06T14:03:12Z"))
      (DeskRefusal.Past(at, now).said, DeskRefusal.TooFar(now, at).said) ==> (
        "Nothing was scheduled: 2026-10-06 14:00:00 UTC is not after now, 2026-10-06 14:03:12 UTC.",
        "Nothing was scheduled: 2026-10-06 14:03:12 UTC is after 2026-10-06 14:00:00 UTC, the " +
          "furthest ahead one can be."
      )
    }
  }
}
