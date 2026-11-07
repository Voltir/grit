package grit.core.files

import scala.concurrent.duration.*

import utest.*

/** How a file error says how long files are kept. */
object FileErrorTests extends TestSuite {

  val tests = Tests {
    test("a span is said in its largest whole unit up to hours, singular for one") {
      (
        FileError.span(1.hour),
        FileError.span(24.hours),
        FileError.span(90.minutes),
        FileError.span(1.minute),
        FileError.span(61.seconds)
      ) ==> ("for 1 hour", "for 24 hours", "for 90 minutes", "for 1 minute", "for 61 seconds")
    }
  }
}
