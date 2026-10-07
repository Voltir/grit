package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.tool.ToolSetId
import grit.eval.harness.corpus.Spent
import grit.turn.{Turn, TurnLoop, TurnOffer, TurnRecord}

import utest.*

/** A recorded turn as markdown ([[SentMarkdown]]), from a hand-built [[TurnSent]]. */
object SentMarkdownTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c1"), TurnSeq(0))

  private val asked = Message.User("read it")

  private val calling: Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj("path" -> "a.txt"))),
    StopReason.ToolUse,
    Usage.Zero,
    "m"
  )

  private def result(content: String) = Message.ToolResult(ToolCallId("t1"), content, false)

  private def request(messages: Message*) = ModelRequest("sys", messages.toVector)

  private val answer: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("done")), StopReason.EndTurn, Usage.Zero, "m")

  /** Its ledger row for a call whose request estimated as `estimated`. */
  private def row(entry: EntryId, estimated: Tokens) =
    entry -> Spent(None, "m", Usage(Tokens(10), Tokens(1), Tokens.Zero, None), estimated)

  /** A turn of one tool round, then an answer sent `last`; its rows recorded `first` and
    * `answered` as their estimates; run under `epoch`.
    */
  private def sent(
      last: ModelRequest,
      first: ModelRequest = request(asked),
      recorded: Option[Tokens] = None,
      epoch: String = Turn.Epoch
  ): TurnSent = {
    val looped = TurnRecord.Call(0, EntryId("call:c1:0:0"), first)
    val off = last.copy(
      messages = last.messages :+ Message.User(TurnLoop.LastCall),
      use = ToolUse.Off
    )
    val answered = TurnRecord.Answered(1, turn.replyId, last, off)
    TurnSent(
      turn.workflowId,
      turn,
      Place.read("slack:T/C").fold(e => throw new java.lang.AssertionError(e), identity),
      epoch,
      None,
      TurnOffer.Root.Addressed,
      Vector.empty,
      ToolSetId.of("0123456789abcdef").fold(e => throw new java.lang.AssertionError(e), identity),
      Vector.empty,
      TurnRecord.Calls(Vector(looped), Some(answered), TurnRecord.Schemas.Sent),
      Some(Picked.of(answered, recorded.orElse(Some(CharEstimate.request(last))))),
      Vector(
        row(EntryId("call:c1:0:0"), CharEstimate.request(first)),
        row(turn.replyId, recorded.getOrElse(CharEstimate.request(last)))
      ),
      Some(Answer(answer, draft = false)),
      None,
      Vector.empty,
      None,
      None,
      None,
      None,
      None,
      Map.empty
    )
  }

  private def shown(t: TurnSent, cut: Cut): String =
    SentMarkdown.render(t, "grit_test", cut)

  val tests = Tests {
    test(
      "a rebuilt request is shown numbered by role, a tool result past the cut cut with its length stated"
    ) {
      val t = sent(request(asked, calling, result("x" * 25)))
      val cut = shown(t, Cut.Within(10))
      assert(
        cut.contains("```text\nsys\n```"),
        cut.contains("**1 · user** (a message)\n```text\nread it\n```"),
        cut.contains("**2 · assistant**\n- calls `peek` (`t1`) with `{\"path\":\"a.txt\"}`"),
        cut.contains(
          "**3 · tool result** for `t1`\n```text\nxxxxxxxxxx\n```\n… 25 chars, 15 not shown; `--full` shows all."
        ),
        !cut.contains("x" * 11)
      )
      assert(shown(t, Cut.Whole).contains(s"```text\n${"x" * 25}\n```"))
    }
  }
}
