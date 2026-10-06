package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryUsageLedger,
  StoreError
}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec, Toolbox}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*
import TurnFixtures.Scripted

/** A finished turn's tally, read from the stores its body wrote: the one line a served
  * deployment logs per turn.
  */
object TurnTallyTests extends TestSuite {
  import TurnFixtures.*

  /** Each call of a round priced at a tenth of a cent. */
  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001")))

  private def calling(text: String, calls: (String, String, String)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map((id, name, p) =>
        AssistantBlock.ToolCall(ToolCallId(id), name, ujson.Obj("path" -> p))
      ),
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      usage,
      "m"
    )

  /** A free tool that answers without reading anything. */
  private val glance: Tool[String] =
    Tool(
      ToolSpec(
        ToolName("glance"),
        "Glances at a file.",
        Args.of((path = Field.text("The file."))).map(_.path)
      ),
      Gate.Free,
      p => p,
      _ => Outcome.Done("seen")
    )

  val tests = Tests {
    test(
      "a turn's line names its place, rounds, tools in order of first call with repeats folded, and the cost of every row its workflow recorded"
    ) {
      // peek and glance, then peek again, then the answer: peek's repeat is not adjacent.
      val provider = new Scripted((_, n) =>
        Right(n match {
          case 0 => calling("looking", ("t1", "peek", "a.txt"), ("t2", "glance", "b.txt"))
          case 1 => calling("", ("t3", "peek", "c.txt"))
          case _ => calling("alpha and gamma")
        })
      )
      val entries = new InMemoryEntryStore
      val ledger = new InMemoryUsageLedger
      val ws = new Files(Map("a.txt" -> "alpha", "c.txt" -> "gamma"))
      val box = Toolbox
        .of[caps.CapSet^{ws}](peek(ws), glance)
        .fold(d => throw new java.lang.AssertionError(d), identity)
      val turn = say(entries, "read them")
      val said = new InMemoryDurable().run(turn.workflowId)(
        tooledBody(entries, provider, ledger, new StubProvider(), NoClassifier, ws, box, 5)
      )
      // The stub's summary is priced at nothing: the three rounds' calls are the cost.
      TurnTally
        .read(hosting().conversations, entries, ledger, turn, said)(using TestTx.fake)
        .map(_.line) ==> Right(
        "turn c1:0 at fs:/checkout: 3 rounds, tools peek×2, glance; $0.003; " +
          "replied: reply:c1:0; summarised: summary:c1:0"
      )
    }

    test("a turn whose conversation is gone is Invalid, naming the turn") {
      val gone = TurnRef(ConversationId("c9"), TurnSeq(0))
      TurnTally.read(
        new InMemoryConversationStore,
        new InMemoryEntryStore,
        new InMemoryUsageLedger,
        gone,
        "replied"
      )(using TestTx.fake) ==> Left(StoreError.Invalid("turn c9:0's conversation is gone"))
    }
  }
}
