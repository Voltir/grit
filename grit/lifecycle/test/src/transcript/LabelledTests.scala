package grit.lifecycle.transcript

import grit.core.period.Ground
import grit.core.store.Entry

import TestTranscripts.{replied, result, said}
import utest.*

/** [[PeriodTranscript.labelled]] and [[Labelled]]: the transcript the closing writer cites
  * into, and the ground its citations give.
  */
object LabelledTests extends TestSuite {

  /** A period: a question, two searches (one empty, one failed), a read, a declined edit,
    * the person stating something, and an answer.
    */
  private val period: Vector[Entry] = Vector(
    said(0, "What port does the api use?"),
    result(1, "search \"port\" .", "No matches."),
    result(2, "read missing.yml", "missing.yml does not exist", error = true),
    result(3, "read config.yml", "1\tport: 3000\n2\thost: localhost"),
    result(4, "edit config.yml", "The person declined this call; it did not run.", error = true),
    said(5, "We decided the api stays on 3000."),
    replied(6, "The api uses port 3000.")
  )

  val tests = Tests {
    test(
      "the writer's transcript labels each line, a tool call as one clipped line with its result"
    ) {
      PeriodTranscript.labelled(period).text ==>
        Vector(
          "[u1] User: What port does the api use?",
          "[t2] search \"port\" . → No matches.",
          "[t3] read missing.yml → failed: missing.yml does not exist",
          "[t4] read config.yml → 1 port: 3000 2 host: localhost",
          "[t5] edit config.yml → failed: The person declined this call; it did not run.",
          "[u6] User: We decided the api stays on 3000.",
          "[a7] Assistant: The api uses port 3000."
        ).mkString("\n\n")
      val long = PeriodTranscript
        .labelled(Vector(result(0, "read big.txt", "x" * 500)))
        .text
      long.length ==> PeriodTranscript.ToolChars
      long ==> ("[t1] read big.txt → " + "x" * (PeriodTranscript.ToolChars - 21) + "…")
    }

    test("a cited person line grounds Person, then a tool line Tool; otherwise Claimed") {
      val t = PeriodTranscript.labelled(period)
      t.ground(Vector("u6", "t4")) ==> Ground.Person
      t.ground(Vector("t4", "a7")) ==> Ground.Tool
      t.ground(Vector("T4")) ==> Ground.Tool
      t.ground(Vector("a7")) ==> Ground.Claimed
      t.ground(Vector.empty) ==> Ground.Claimed
      // A label this transcript does not have counts for nothing.
      t.ground(Vector("u99", "t42", "s1")) ==> Ground.Claimed
    }

    test("under the question rule, a cited question of the person's grounds nothing") {
      val asking = PeriodTranscript.labelled(period, questionsGround = false)
      // u1 "What port does the api use?" is a question; u6 states a decision.
      asking.ground(Vector("u1")) ==> Ground.Claimed
      asking.ground(Vector("u1", "a7")) ==> Ground.Claimed
      asking.ground(Vector("u6")) ==> Ground.Person
      asking.ground(Vector("u1", "t4")) ==> Ground.Tool
      // Without it, a question grounds as the person's word.
      PeriodTranscript.labelled(period).ground(Vector("u1")) ==> Ground.Person
    }

    test("a tool line whose result is an error grounds nothing") {
      val t = PeriodTranscript.labelled(period)
      t.ground(Vector("t3")) ==> Ground.Claimed
      t.ground(Vector("t5")) ==> Ground.Claimed
      t.ground(Vector("t3", "a7")) ==> Ground.Claimed
    }

    test("whole lines are kept from the end under the cap, and a label cut grounds nothing") {
      val t = PeriodTranscript.labelled(period)
      val last =
        "[u6] User: We decided the api stays on 3000.\n\n[a7] Assistant: The api uses port 3000."
      t.within(last.length).text ==> last
      t.within(last.length - 1).text ==> "[a7] Assistant: The api uses port 3000."
      // u6 was cut: a citation of it counts for nothing.
      t.within(last.length - 1).ground(Vector("u6")) ==> Ground.Claimed
      t.within(last.length).ground(Vector("u6")) ==> Ground.Person
    }
  }
}
