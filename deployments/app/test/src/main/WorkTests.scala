package grit.app.main

import java.nio.file.Path

import grit.core.id.SourceId

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
  }
}
