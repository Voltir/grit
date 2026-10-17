package grit.core.tool

import grit.core.host.Clipped
import grit.core.visibility.{Integrity, Part, Provenance, Readers, TestLabels}

import utest.*

object TrimmedTests extends TestSuite {

  private def part(text: String): Part =
    Part(
      Provenance(TestLabels.place("service:drive/folder-1"), Integrity.Member),
      Readers.AtPlace,
      text
    )

  val tests = Tests {
    test(
      "parts are kept only with room after the head for what a reader keeping none of them is shown"
    ) {
      // The head and its one part fit by three bytes; the head and NoneShown would not.
      val head = "h" * (Clipped.MaxBytes - 6)
      assert(Clipped.fits(s"$head\nab"), !Clipped.fits(s"$head\n${Outcome.NoneShown}"))
      (Trimmed.of("found:", Vector(part("ab"))).parts, Trimmed.of(head, Vector(part("ab"))).parts) ==>
        (Some(Vector(part("ab"))), None)
    }
  }
}
