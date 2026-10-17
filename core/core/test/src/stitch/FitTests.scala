package grit.core.stitch

import utest.*
import Stitching.Line

/** A reader's thread by part: what it shows, and what it leaves out of the conversation's own
  * messages. Every text here is synthetic.
  */
object FitTests extends TestSuite {

  private val own: Vector[Line] =
    Vector(Line("Ben", "one"), Line("Ana", "two"), Line("Ben", "three four five"))
  private val all = "Ben: one\nAna: two\nBen: three four five"

  /** Each case `thread` and `fit` must agree on: no strand, room for some, room for one whole
    * message, no room, no own.
    */
  private val cases: Vector[(String, Vector[Line], Int)] = Vector(
    ("", own, 15),
    ("", own, 100),
    ("Cy: before", own, 27),
    ("Cy: before", own, 32),
    ("Cy: before", own, 10),
    ("Cy: before", Vector.empty, 20),
    ("", Vector.empty, 5)
  )

  val tests = Tests {
    test("a fitted thread's text is the thread, byte for byte") {
      cases.map((s: String, o: Vector[Line], c: Int) => Stitching.fit(s, o, "\n", c).text) ==>
        Vector(
          "Ben: …four five",
          all,
          "Cy: before\nBen: …four five",
          "Cy: before\n…Ben: three four five",
          "Cy: before",
          "Cy: before",
          ""
        )
      cases.map((s: String, o: Vector[Line], c: Int) => Stitching.fit(s, o, "\n", c).text) ==>
        cases.map((s: String, o: Vector[Line], c: Int) => Stitching.thread(s, o, "\n", c))
    }

    test(
      "what is cut, then what is shown, are all the own messages, the cut at the shown's start"
    ) {
      cases
        .map((s: String, o: Vector[Line], c: Int) => Stitching.fit(s, o, "\n", c))
        .map(f => (f.cut + f.own, f.at)) ==>
        Vector((all, 6), (all, 0), (all, 17), (all, 12), (all, 10), ("", 10), ("", 0))
      Stitching.fit("Cy: before", own, "\n", 32).cut ==> "Ben: one\nAna: two\n"
    }

    test("a message cut inside keeps its speaker's whole name, within the room") {
      val named: Vector[Line] = Vector(Line("Ana Lee", "one two three"))
      Stitching.thread("", named, "\n", 20) ==> "Ana Lee: …two three"
      Stitching.thread("", named, "\n\n", 15) ==> "Ana Lee: …three"
    }
  }
}
