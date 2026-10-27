package grit.lifecycle.transcript

import grit.core.id.{EntryId, ToolCallId}
import grit.core.period.{Cited, Ground}
import grit.core.persona.Persona
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Integrity

import TestTranscripts.{called, heard, labelledBy, replied, result, said}
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

  /** `lines` as the transcript's fence holds them, a blank line between. */
  private def fenced(lines: String*): String =
    lines.mkString("<transcript>\n", "\n\n", "\n</transcript>")

  /** A tool line: `head`, then `result` in its own fence, its `source` attribute as written. */
  private def tool(head: String, source: String, result: String): String =
    s"$head\n<tool_output source=\"$source\">\n$result\n</tool_output>"

  val tests = Tests {
    test(
      "the writer's transcript labels each line in one fence, a tool call as one line with its result in its own"
    ) {
      TestTranscripts.labelled(period*).text ==>
        fenced(
          "[u1] User: What port does the api use?",
          tool("[t2] search \"port\" . →", "search &quot;port&quot; .", "No matches."),
          tool("[t3] read missing.yml → failed:", "read missing.yml", "missing.yml does not exist"),
          tool("[t4] read config.yml →", "read config.yml", "1 port: 3000 2 host: localhost"),
          tool(
            "[t5] edit config.yml → failed:",
            "edit config.yml",
            "The person declined this call; it did not run."
          ),
          "[u6] User: We decided the api stays on 3000.",
          "[a7] Assistant: The api uses port 3000."
        )
    }

    test(
      "a tool line that can ground shows up to 4,000 chars of its result; an error, or one below Outside, 120"
    ) {
      def shown(e: Entry) = TestTranscripts.labelled(e).text
      def xs(n: Int) = Vector.fill(n)("x").mkString(" ")
      val big = "x " * 2500
      (
        shown(result(0, "read big.txt", big)),
        shown(result(0, "read big.txt", big).copy(integrity = Integrity.Outside)),
        shown(result(0, "read big.txt", big, error = true)),
        shown(result(0, "read big.txt", big).copy(integrity = Integrity.Untrusted))
      ) ==> (
        fenced(tool("[t1] read big.txt →", "read big.txt", xs(2000) + "…")),
        fenced(tool("[t1] read big.txt →", "read big.txt", xs(2000) + "…")),
        fenced(tool("[t1] read big.txt → failed:", "read big.txt", xs(60) + "…")),
        fenced(tool("[t1] read big.txt →", "read big.txt", xs(60) + "…"))
      )
    }

    test(
      "a tool line over the bound keeps ToolChars of its result, cut before an earlier line is dropped"
    ) {
      val t = TestTranscripts.labelled(
        said(0, "What does big.txt hold?"),
        result(1, "read big.txt", "x " * 1000),
        replied(2, "Only x.")
      )
      (t.within(500).text, t.within(500).ground(Vector("u1"))) ==> (
        fenced(
          "[u1] User: What does big.txt hold?",
          tool("[t2] read big.txt →", "read big.txt", Vector.fill(60)("x").mkString(" ") + "…"),
          "[a3] Assistant: Only x."
        ),
        Some(Ground.Person)
      )
    }

    test(
      "grit's post a thread begins with is grit's line, a period of it and heard replies still overheard"
    ) {
      val posted = TestTranscripts.entry(0, Payload.Posted("The engine's open issues."))
      labelledBy(Map(1L -> "Nick"))(posted, heard(1, "why this?")).text ==>
        fenced("[a1] Assistant: The engine's open issues.", "[h2] Nick: why this?")
      PeriodTranscript.overheard(Vector(posted, heard(1, "why this?"))) ==> true
    }

    test(
      "a declined call's reason is a line of the person the turn answers, naming the call, and grounds Person"
    ) {
      val declined = labelledBy(Map(0L -> "Ana"))(
        said(0, "Move the api to 8080."),
        result(1, "edit config.yml", "Declined: it did not run.", error = true),
        TestTranscripts.entry(2, Payload.Declined(ToolCallId("c1"), "keep it on 3000")),
        replied(3, "Left as it is.")
      )
      declined.text ==> fenced(
        "[u1] Ana: Move the api to 8080.",
        tool("[t2] edit config.yml → failed:", "edit config.yml", "Declined: it did not run."),
        "[u3] Ana declined the call edit config.yml, and wrote:\nkeep it on 3000",
        "[a4] Assistant: Left as it is."
      )
      declined.ground(Vector("u3")) ==> Some(Ground.Person)
    }

    test("a cited person line grounds Person, then a tool line Tool; otherwise Claimed") {
      val t = TestTranscripts.labelled(period*)
      t.ground(Vector("u6", "t4")) ==> Some(Ground.Person)
      t.ground(Vector("t4", "a7")) ==> Some(Ground.Tool)
      t.ground(Vector("T4")) ==> Some(Ground.Tool)
      t.ground(Vector("a7")) ==> Some(Ground.Claimed)
      t.ground(Vector.empty) ==> Some(Ground.Claimed)
      // A label this transcript does not have counts for nothing.
      t.ground(Vector("u99", "t42", "s1")) ==> Some(Ground.Claimed)
    }

    test(
      "each person's line is under their name when known, a heard one labelled h; else User"
    ) {
      labelledBy(Map(0L -> "Ana", 1L -> "Ben"))(
        heard(0, "Is the freeze on Thursday?"),
        heard(1, "It moved to Friday."),
        said(2, "When is the freeze?"),
        replied(3, "Friday.")
      ).text ==>
        fenced(
          "[h1] Ana: Is the freeze on Thursday?",
          "[h2] Ben: It moved to Friday.",
          "[u3] User: When is the freeze?",
          "[a4] Assistant: Friday."
        )
      labelledBy(Map(0L -> "Ana"))(said(0, "Ship it.")).text ==> fenced("[u1] Ana: Ship it.")
    }

    test(
      "a person's mention of the assistant is their words to it, never the assistant speaking; another's mention is kept"
    ) {
      val persona = Persona.of("Pip").fold(sys.error, identity)
      labelledBy(Map(0L -> "Ana", 1L -> "Ben", 2L -> "Cy"), persona)(
        heard(0, "@Pip what moved the freeze?"),
        heard(1, "thanks @pip, and @Ana too"),
        said(2, "@Pipkin is on it")
      ).text ==>
        fenced(
          "[h1] Ana, to the assistant: what moved the freeze?",
          "[h2] Ben, to the assistant: thanks, and @Ana too",
          "[u3] Cy: @Pipkin is on it"
        )
    }

    test(
      "a heard line supports nothing: cited alone it gives no ground, beside another line that line's"
    ) {
      val t = labelledBy(Map(0L -> "Ana"))(
        heard(0, "The freeze moved to Friday."),
        said(1, "The freeze is on Friday, then."),
        replied(2, "Noted.")
      )
      t.ground(Vector("h1")) ==> None
      t.ground(Vector("h1", "u99")) ==> None
      t.ground(Vector("h1", "u2")) ==> Some(Ground.Person)
      t.ground(Vector("h1", "a3")) ==> Some(Ground.Claimed)
    }

    test(
      "an untrusted line grounds nothing, as a heard one does: cited alone it gives no ground, where one at Outside does"
    ) {
      val t = labelledBy(Map(1L -> "Ana"))(
        result(0, "search", "42 hits").copy(integrity = Integrity.Untrusted),
        said(1, "The freeze is on Friday, then."),
        replied(2, "Noted.")
      )
      val outside = labelledBy(Map(0L -> "Ana"))(
        said(0, "The freeze is on Friday, then.").copy(integrity = Integrity.Outside)
      )
      t.ground(Vector("t1")) ==> None
      t.ground(Vector("t1", "u2")) ==> Some(Ground.Person)
      t.ground(Vector("t1", "a3")) ==> Some(Ground.Claimed)
      outside.ground(Vector("u1")) ==> Some(Ground.Person)
    }

    test(
      "a cited label names its entry, a tool line its call's tool, an unknown label nothing; heard, untrusted and error lines are named too"
    ) {
      val t = labelledBy(Map(1L -> "Ana"))(
        called(0, 2, "search_code"),
        heard(1, "Is the freeze on Friday?"),
        result(2, "search \"freeze\"", "freeze: Friday").copy(integrity = Integrity.Untrusted),
        called(3, 4, "read_file"),
        result(4, "read missing.yml", "missing.yml does not exist", error = true),
        result(5, "read lost.yml", "lost.yml: 3 lines"),
        replied(6, "The freeze is on Friday.")
      )
      (
        t.cited(Vector("a5", "t2", "h1", "T3", "t4", "u99")),
        t.cited(Vector.empty)
      ) ==> (
        Cited.of(
          Vector("e1", "e2", "e4", "e5", "e6").map(EntryId(_)),
          Set("search_code", "read_file")
        ),
        Cited.Empty
      )
    }

    test("a tool line whose result is an error grounds nothing") {
      val t = TestTranscripts.labelled(period*)
      t.ground(Vector("t3")) ==> Some(Ground.Claimed)
      t.ground(Vector("t5")) ==> Some(Ground.Claimed)
      t.ground(Vector("t3", "a7")) ==> Some(Ground.Claimed)
    }

    test("whole lines are kept from the end under the cap, and a label cut grounds nothing") {
      val t = TestTranscripts.labelled(period*)
      val lines =
        Vector(
          "[u6] User: We decided the api stays on 3000.",
          "[a7] Assistant: The api uses port 3000."
        )
      // The cap counts the lines, a blank line between, not the fence around them.
      val last = lines.mkString("\n\n")
      t.within(last.length).text ==> fenced(lines*)
      t.within(last.length - 1).text ==> fenced("[a7] Assistant: The api uses port 3000.")
      // u6 was cut: a citation of it counts for nothing.
      t.within(last.length - 1).ground(Vector("u6")) ==> Some(Ground.Claimed)
      t.within(last.length).ground(Vector("u6")) ==> Some(Ground.Person)
    }
  }
}
