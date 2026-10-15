package grit.eval.harness.loss

import grit.core.durable.StepRecord
import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.period.Probability
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.eval.harness.stats.Proportion
import grit.turn.Withheld

import utest.*

/** The lost-tool measure, over `gate:n` outputs written as a turn records them. */
object LossTests extends TestSuite {

  /** A `gate:n` step's recorded output: `level`, the decided items' JSON and the withheld
    * tools' JSON, as the turn's journal writes them.
    */
  private def gate(n: Int, level: String, decided: String, withheld: String): StepRecord =
    StepRecord(
      s"gate:$n",
      Some(s"""{"ok":{"level":"$level","decided":[$decided],"withheld":[$withheld]}}"""),
      None
    )

  private def hidden(ref: String, source: String, name: String): String =
    s"""{"item":{"ref":$ref,"integrity":"untrusted","source":"$source"},""" +
      s""""fate":{"kind":"hidden","name":"$name","costs":["post"]}}"""

  private def lowered(ref: String, source: String): String =
    s"""{"item":{"ref":$ref,"integrity":"untrusted","source":"$source"},"fate":{"kind":"lowered"}}"""

  private val closing = """{"kind":"own","seq":3}"""
  private val result = """{"kind":"result","call":"t1"}"""
  private val nearbyClosing = """{"kind":"nearby","conversation":"c9","seq":4}"""
  private val post = """{"tool":"post","requires":"member"}"""
  private val peek = """{"tool":"peek","requires":"outside"}"""

  private def turn(conversation: String, seq: Long): TurnRef =
    TurnRef(ConversationId(conversation), TurnSeq(seq))

  private val writing = Gates(Vector(Withheld(ToolName("post"), Integrity.Member)), 0, 0)
  private val reading = Gates(Vector(Withheld(ToolName("peek"), Integrity.Outside)), 0, 0)
  private val open = Gates(Vector.empty, 0, 0)

  val tests = Tests {
    test("a turn's withheld tools are each named once, and only those requiring member write out") {
      val gates = Gates.of(
        Vector(
          StepRecord("record-window", Some("""{"ok":[]}"""), None),
          gate(0, "member", "", ""),
          gate(1, "untrusted", lowered(result, "peek"), s"$post,$peek"),
          gate(2, "untrusted", "", post)
        )
      )
      gates.map(g => (g.withheld.map(_.tool), g.writesOut.map(_.tool))) ==>
        Right((Vector(ToolName("post"), ToolName("peek")), Vector(ToolName("post"))))
    }

    test("the items hidden are those the last round still hid, closing records counted apart") {
      // Round 0 hid a result and two closings; round 1 shows the result, lowering, and still
      // hides both closings, its own conversation's and a nearby one's.
      val gates = Gates.of(
        Vector(
          gate(
            0,
            "member",
            Vector(
              hidden(result, "peek", "h1"),
              hidden(closing, "a closing record", "h2"),
              hidden(nearbyClosing, "a closing record", "h3")
            ).mkString(","),
            ""
          ),
          gate(
            1,
            "untrusted",
            Vector(
              lowered(result, "peek"),
              hidden(closing, "a closing record", "h2"),
              hidden(nearbyClosing, "a closing record", "h3"),
              hidden("""{"kind":"result","call":"t2"}""", "fetch", "h4")
            ).mkString(","),
            post
          )
        )
      )
      gates.map(g => (g.hidden, g.closings)) ==> Right((1, 2))
    }

    test("a gate whose output does not read is named") {
      val read = Gates.of(Vector(gate(0, "member", "", ""), StepRecord("gate:1", Some("{}"), None)))
      assert(read.left.exists(_.startsWith("gate:1:")))
    }

    test("only the turns that had a write-out tool withheld are asked about their root") {
      Measure.toAsk(
        Vector(turn("a", 1) -> writing, turn("a", 2) -> reading, turn("b", 1) -> open)
      ) ==> Vector(turn("a", 1))
    }

    test(
      "the share lost is over conversations with a turn in the window, a root at one half asking"
    ) {
      val m = Measure.of(
        Vector(
          turn("a", 1) -> writing, // asks, at one half: lost
          turn("a", 2) -> writing, // a second lost turn in the same conversation
          turn("b", 1) -> writing, // does not ask
          turn("c", 1) -> reading, // a tool withheld, none that writes out
          turn("d", 1) -> open,
          turn("e", 1) -> writing // Jev did not answer
        ),
        Map(
          turn("a", 1) -> Right(Probability.clamped(0.5)),
          turn("a", 2) -> Right(Probability.clamped(0.9)),
          turn("b", 1) -> Right(Probability.clamped(0.49)),
          turn("e", 1) -> Left("unavailable")
        )
      )
      (m.lost.hits, m.lost.n, m.withheld, m.writesOut, m.unjudged) ==> (1L, 5L, 4, 3, 1)
      m.lost.interval ==> Proportion.Interval.TooFewClusters
    }
  }
}
