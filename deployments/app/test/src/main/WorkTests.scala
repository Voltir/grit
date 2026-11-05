package grit.app.main

import java.nio.file.Path

import grit.core.id.SourceId
import grit.core.inbox.TurnEnding

import utest.*

/** What `grit work` reads before it starts anything: its arguments and the source of its text. */
object WorkTests extends TestSuite {

  val tests: Tests = Tests {

    test("a text is said under work: and its SHA-256, so the same text names one source") {
      (Work.source("hi"), Work.source("hi"), Work.source("hi ") == Work.source("hi")) ==> (
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"),
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"),
        false
      )
    }

    test("work takes a file, or --dry and a file; anything else is refused with the form") {
      (
        Work.asked(Vector("brief.md")),
        Work.asked(Vector("--dry", "brief.md")),
        Work.asked(Vector.empty),
        Work.asked(Vector("brief.md", "--dry")),
        Work.asked(Vector("a.md", "b.md"))
      ) ==> (
        Right(Work.Asked(Path.of("brief.md"), dry = false)),
        Right(Work.Asked(Path.of("brief.md"), dry = true)),
        Left(Work.Usage),
        Left(Work.Usage),
        Left(Work.Usage)
      )
    }

    test("a task's turn n after its first is said under its text's source and :n") {
      (Work.source("hi", 1), Work.source("hi", 2)) ==> (
        Work.source("hi"),
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4:2")
      )
    }

    test(
      "a turn that ended at its limit goes on to the next until GRIT_WORK_TURNS, said in one line; any other ending stops"
    ) {
      (
        Work.after("w1", TurnEnding.AtLimit, 1, 8),
        Work.after("w8", TurnEnding.AtLimit, 8, 8),
        Work.after("w3", TurnEnding.Otherwise, 3, 8)
      ) ==> (
        Work.After.GoOn(
          2,
          "[main] w1 reached its limit; going on in a fresh turn (turn 2 of at most 8)"
        ),
        Work.After.Stop(
          Some(
            "[main] w8 reached its limit at turn 8, the most GRIT_WORK_TURNS allows; raise GRIT_WORK_TURNS and rerun"
          )
        ),
        Work.After.Stop(None)
      )
    }

    test("GRIT_WORK_TURNS: unset is 8; under 1 or not a whole number is an error") {
      (
        Work.turns(Map.empty),
        Work.turns(Map("GRIT_WORK_TURNS" -> " 3 ")),
        Work.turns(Map("GRIT_WORK_TURNS" -> "0")),
        Work.turns(Map("GRIT_WORK_TURNS" -> "many"))
      ) ==> (
        Right(8),
        Right(3),
        Left("GRIT_WORK_TURNS is a whole number of at least 1, not 0"),
        Left("GRIT_WORK_TURNS is a whole number of at least 1, not many")
      )
    }
  }
}
