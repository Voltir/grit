package grit.lifecycle.close

import grit.core.period.{Edit, Section}

import utest.*

object ClosingSummaryTests extends TestSuite {
  import CloseFixtures.replyOf

  private def read(text: String, asked: Asked = Asked.Every): Option[ClosingSummary.Written] =
    ClosingSummary.read(replyOf(text), asked)

  /** What a writer's parts come to: decisions and facts as standing adds, then open ones. */
  private def closing(
      prose: String,
      outcome: Option[String] = None,
      decisions: Vector[String] = Vector(),
      facts: Vector[String] = Vector(),
      open: Vector[String] = Vector()
  ): Option[ClosingSummary.Written] =
    Some(
      ClosingSummary.Written(
        prose,
        outcome,
        (decisions ++ facts).map(Edit.Add(Section.Standing, _)) ++ open.map(
          Edit.Add(Section.Open, _)
        )
      )
    )

  val tests = Tests {
    test("each labelled part is read, a list one item a line, its bullet or number taken off") {
      read(
        """Summary: We fixed the flaky test.
          |It was a race.
          |Outcome: green again
          |Decisions:
          |- retry once
          |* keep the timeout
          |Facts:
          |1. the race is in Follow
          |2) it shows under load
          |Open:
          |• check grit/app/src/chat/Follow.scala again
          |Sources:
          |- none""".stripMargin
      ) ==> closing(
        "We fixed the flaky test. It was a race.",
        Some("green again"),
        Vector("retry once", "keep the timeout"),
        Vector("the race is in Follow", "it shows under load"),
        Vector("check grit/app/src/chat/Follow.scala again")
      )
    }

    test("labels in markdown emphasis or as headings, in any case, are read") {
      read(
        """**Summary:** We talked.
          |## decisions:
          |- ship it
          |__OUTCOME__: shipped""".stripMargin
      ) ==> closing("We talked.", Some("shipped"), Vector("ship it"))
    }

    test("only the sections asked for are kept; none is no outcome") {
      read(
        "Summary: We talked.\nOutcome: none\nDecisions:\n- ship it\nFacts:\n- a fact",
        Asked(outcome = true, decisions = false, facts = true, open = false)
      ) ==> closing("We talked.", facts = Vector("a fact"))
    }

    test("text before the first label is the prose when Summary is missing; unlabelled, all") {
      read("We talked about knots.\nDecisions:\n- the bowline") ==>
        closing("We talked about knots.", decisions = Vector("the bowline"))
      read("Just some text\nover two lines.") ==> closing("Just some text over two lines.")
    }

    test("a reply with no prose is none") {
      read("") ==> None
      read("Decisions:\n- ship it") ==> None
    }

    test("the request asks only for the sections asked, the transcript's end as the message") {
      val some = ClosingSummary.request("x" * 50_000, Asked(true, false, false, true))
      some.system ==>
        (ClosingSummary.System + "\n" +
          "Summary: two to four sentences: what was asked, and what came of it.\n" +
          "Outcome: one line: what the conversation came to.\n" +
          "Open: then one line per item, each starting with \"- \": each question left unanswered or task left unfinished.\n" +
          "Write none under a list with nothing in it.")
      some.messages.map(_.toString.length) ==> Vector(s"User(${"x" * 40_000})".length)
      ClosingSummary.request("t", Asked(false, false, false, false)).system ==>
        (ClosingSummary.System + "\nSummary: two to four sentences: what was asked, and what came of it.")
    }
  }
}
