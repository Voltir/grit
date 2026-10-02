package grit.core.stitch

import utest.*

/** A reader's thread by part: what it shows, and what it leaves out of the conversation's own
  * messages. Every text here is synthetic.
  */
object FitTests extends TestSuite {

  private val own = "Ben: one\nAna: two\nBen: three"

  /** Each case `thread` and `fit` must agree on: no strand, room for some, no room, no own. */
  private val cases: Vector[(String, String, Int)] = Vector(
    ("", own, 10),
    ("", own, 100),
    ("Cy: before", own, 21),
    ("Cy: before", own, 10),
    ("Cy: before", "", 20),
    ("", "", 5)
  )

  val tests = Tests {
    test("a fitted thread's text is the thread, byte for byte") {
      cases.map((s, o, c) => Stitching.fit(s, o, c).text) ==>
        Vector(
          "Ben: three",
          own,
          "Cy: before\nBen: three",
          "Cy: before",
          "Cy: before",
          ""
        )
      cases.map((s, o, c) => Stitching.fit(s, o, c).text) ==>
        cases.map((s, o, c) => Stitching.thread(s, o, c))
    }

    test(
      "what is cut, then what is shown, are all the own messages, the cut at the shown's start"
    ) {
      cases.map((s, o, c) => Stitching.fit(s, o, c)).map(f => (f.cut + f.own, f.at)) ==>
        Vector((own, 0), (own, 0), (own, 11), (own, 10), ("", 10), ("", 0))
      Stitching.fit("Cy: before", own, 21).cut ==> "Ben: one\nAna: two\n"
    }
  }
}
