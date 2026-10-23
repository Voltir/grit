package grit.core.files

import utest.*

/** [[Anchor]]: a passage's place in a file's text, found again after the text changes. */
object AnchorTests extends TestSuite {

  private val Passage = "All accounts are reviewed every ninety days by the system owner."

  private def anchored(text: String): Anchor = {
    val start = text.indexOf(Passage)
    Anchor.of(text, start, start + Passage.length)
  }

  private def at(text: String, quote: String): Option[(Int, Int)] =
    Option(text.indexOf(quote)).filter(_ >= 0).map(s => (s, s + quote.length))

  val tests = Tests {
    test("an anchor is its passage's first eight words, the characters either side, and where") {
      val text = ("x" * 40) + " " + Passage + " Then more."
      anchored(text) ==> Anchor(
        "All accounts are reviewed every ninety days by",
        "x" * 31 + " ",
        " the system owner. Then more.",
        41.0 / text.length
      )
    }

    test("found exactly by its context, though its words also lie nearer its old position") {
      val text = "Intro. " + Passage + " Closing words of the first part."
      val anchor = anchored(text)
      // The same words, in other context, now lie where the passage was; the passage moved on.
      val moved =
        "Elsewhere: " + Passage.take(46) + " the clerk. " + ("filler " * 20) + text
      Anchor.find(anchor, moved) ==> Some {
        val s = moved.indexOf("Intro. ") + "Intro. ".length
        (s, s + anchor.quote.length)
      }
    }

    test("among twins its context does not tell apart, the one nearest its position") {
      val twin = "Note: " + Passage + " End. "
      val text = (("pad " * 50) + twin) * 3 + ("pad " * 50)
      val second = text.indexOf(twin, text.indexOf(twin) + 1) + "Note: ".length
      val anchor = Anchor.of(text, second, second + Passage.length)
      Anchor.find(anchor, "lead " + text) ==> Some((second + 5, second + 5 + anchor.quote.length))
    }

    test("with its words edited, the best match at least 0.8 alike; less alike, gone") {
      val anchor = anchored("Policy. " + Passage + " Next.")
      val edited = "Changed policy text. All accounts are reviewed every 90 days by the owner."
      val unlike = "Changed policy text. Some users get checked yearly by an auditor."
      (Anchor.find(anchor, edited), Anchor.find(anchor, unlike)) ==>
        (at(edited, "All accounts are reviewed every 90 days by"), None)
    }

    test("a passage deleted from the text is gone") {
      val anchor = anchored("Policy. " + Passage + " Next.")
      Anchor.find(anchor, "Policy. Next.") ==> None
    }
  }
}
