package grit.core.stitch

import java.time.Instant

import utest.*
import Stitching.{Excerpt, Line}

/** A reader's thread by part: what it shows of a strand and of the conversation's own messages,
  * and what it leaves out. Every text here is synthetic.
  */
object FitTests extends TestSuite {

  private def line(speaker: String, text: String) = Line(speaker, text, Instant.EPOCH)
  private val own: Vector[Line] =
    Vector(line("Ben", "one"), line("Ana", "two"), line("Ben", "three four five"))
  private val all = "Ben: one\nAna: two\nBen: three four five"
  private val before = Excerpt(Some(line("Cy", "before")), 0, Vector.empty)

  /** No strand, room for part of the newest; room for all; a strand, room for part of the
    * newest; room for the newest whole; room for the strand alone; no own; nothing.
    */
  private val cases: Vector[(Excerpt, Vector[Line], Int)] = Vector(
    (Excerpt.empty, own, 15),
    (Excerpt.empty, own, 100),
    (before, own, 30),
    (before, own, 34),
    (before, own, 10),
    (before, Vector.empty, 20),
    (Excerpt.empty, Vector.empty, 5)
  )
  private val fitted: Vector[Stitching.Fitted] =
    cases.map((s: Excerpt, o: Vector[Line], c: Int) => Stitching.fit(s, o, "\n", c))

  val tests = Tests {
    test("a fitted thread's text is the strand, a … line for what is left out, then the newest") {
      fitted.map(_.text) ==>
        Vector(
          "…\nBen: …five",
          all,
          "Cy: before\n…\nBen: …four five",
          "Cy: before\n…\nBen: three four five",
          "Cy: before",
          "Cy: before",
          ""
        )
    }

    test("the oldest messages are left out whole; only the newest alone is cut inside") {
      fitted.map(f => (f.cut.size, f.shown.map(_.text))) ==> Vector(
        (2, Vector("…five")),
        (0, own.map(_.text)),
        (2, Vector("…four five")),
        (2, Vector("three four five")),
        (3, Vector.empty),
        (0, Vector.empty),
        (0, Vector.empty)
      )
      fitted.take(5).map(f => (f.cut ++ f.shown).map(_.speaker)) ==>
        Vector.fill(5)(own.map(_.speaker))
    }

    test("a message cut inside keeps its speaker's whole name, within the room") {
      val named: Vector[Line] = Vector(line("Ana Lee", "one two three"))
      Stitching.fit(Excerpt.empty, named, "\n", 20).text ==> "Ana Lee: …two three"
      Stitching.fit(Excerpt.empty, named, "\n\n", 15).text ==> "Ana Lee: …three"
    }

    test(
      "an excerpt within less room leaves out more of its latest, counted, then cuts its opening"
    ) {
      val strand = Excerpt(
        Some(line("Nick", "where did we land")),
        0,
        Vector(line("Ana", "one"), line("Bo", "two"))
      )
      Vector(40, 33, 25, 20, 7).map(strand.within).map(e => (e.left, e.text)) ==> Vector(
        (0, "Nick: where did we land\nAna: one\nBo: two"),
        (1, "Nick: where did we land\n…\nBo: two"),
        (2, "Nick: where did we land\n…"),
        (2, "Nick: where did…\n…"),
        (0, "")
      )
    }
  }
}
