package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.tool.ToolSetId
import grit.eval.harness.capture.Spent
import grit.turn.{Turn, TurnLoop, TurnOffer, TurnRecord, TurnSummary}

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
    * `answered` as their estimates; run under `epoch`; its summary rebuilt as `summary`, its
    * row recording that request's estimate.
    */
  private def sent(
      last: ModelRequest,
      first: ModelRequest = request(asked),
      recorded: Option[Tokens] = None,
      epoch: String = Turn.Epoch,
      summary: Option[ModelRequest] = None
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
      TurnRecord.Calls(
        Vector(looped),
        Some(answered),
        TurnRecord.Schemas.Sent,
        summary,
        Vector.empty
      ),
      Some(Picked.of(answered, recorded.orElse(Some(CharEstimate.request(last))))),
      Vector(
        row(EntryId("call:c1:0:0"), CharEstimate.request(first)),
        row(turn.replyId, recorded.getOrElse(CharEstimate.request(last)))
      ) ++ summary.map(s =>
        TurnSummary.id(turn) ->
          Spent(None, "m", Usage(Tokens(90), Tokens(2), Tokens(80), None), CharEstimate.request(s))
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

    test(
      "each call gives its rebuilt estimate as a share of the input its ledger row recorded, and says whether the row recorded that estimate"
    ) {
      val last = request(asked, calling, result("alpha"))
      val first = Tokens.value(CharEstimate.request(request(asked)))
      val answered = Tokens.value(CharEstimate.request(last))
      // Each row records 10 in; the share is the estimate over it, to two places.
      def share(estimate: Long): String = (BigDecimal(estimate) / 10).setScale(2).toString
      val lines = shown(sent(last, recorded = Some(Tokens(7))), Cut.Whole).linesIterator.toVector
      (
        lines.find(_.startsWith("- **Call 0**")).map(_.dropWhile(_ != ';')),
        lines.find(_.startsWith("- **Call 1**")).map(_.dropWhile(_ != ';'))
      ) ==> (
        Some(s"; estimate $first, ${share(first)} of its input, as its row recorded."),
        Some(s"; estimate $answered, ${share(answered)} of its input; its row recorded 7.")
      )
    }

    test(
      "each call with a row says what its cache read, what its cache misses cost, its cache breaks and its new tokens"
    ) {
      val lines =
        shown(sent(request(asked, calling, result("alpha"))), Cut.Whole).linesIterator.toVector
      def under(call: String) =
        lines.indexWhere(_.startsWith(s"- **Call $call**")) match {
          case -1 => None
          case i => lines.lift(i + 1)
        }
      // Each row: 10 in, none cached or written, 1 out, on a model with no published rates.
      (under("0"), under("1")) ==> (
        Some(
          "  Cache: 0.0% of its input read (0 of 10, 0 written); cache misses unpriced (no " +
            "published rates for `m`); 10 new."
        ),
        Some(
          "  Cache: 0.0% of its input read (0 of 10, 0 written); cache misses unpriced (no " +
            "published rates for `m`); cache breaks: 0; -1 new."
        )
      )
    }

    test("each call's input by part sums to the input its row counted") {
      val lines = shown(sent(request(asked, calling, result("alpha"))), Cut.Whole).linesIterator
      val rows = lines
        .filter(l => l.startsWith("| 0 |") || l.startsWith("| 1 |"))
        .map(_.split('|').map(_.trim).filter(_.nonEmpty).toVector)
        .toVector
      rows.map(cells =>
        (
          cells.lift(1),
          cells.drop(3).flatMap(_.takeWhile(_ != ' ').toLongOption).sum
        )
      ) ==> Vector((Some("10"), 10L), (Some("10"), 10L))
    }

    test("what no later output references is called unreferenced, in both places") {
      val t = sent(request(asked, calling, result("alpha")))
      (
        shown(t, Cut.Whole).linesIterator.contains("### Unreferenced"),
        SentMarkdown.efficiencyLines(t).lastOption.map(_.takeWhile(_ != ':'))
      ) ==> (true, Some("unreferenced"))
    }

    test(
      "a summary rebuilt on the answering call's prefix is said to be, beside its ledger row's cached count; one asked alone is said to be alone"
    ) {
      val last = request(asked, calling, result("alpha"))
      val ask = Message.User(TurnSummary.Ask)
      val onPrefix = last.copy(messages = last.messages :+ answer :+ ask)
      val alone = ModelRequest(TurnSummary.System, Vector(Message.User("<transcript>")))
      // The summary's row counts 90 in and recorded the request's own estimate.
      def estimated(r: ModelRequest): String = {
        val e = Tokens.value(CharEstimate.request(r))
        s"$e, ${(BigDecimal(e) / 90).setScale(2, BigDecimal.RoundingMode.HALF_UP)} of its input, as its row recorded"
      }
      val line =
        s"- **Summary** (`${EntryId.value(TurnSummary.id(turn))}`): `m`, 90 in (80 cached), " +
          "2 out, unpriced; estimate"
      (
        shown(sent(last, summary = Some(onPrefix)), Cut.Whole).linesIterator
          .find(_.startsWith("- **Summary**")),
        shown(sent(last, summary = Some(alone)), Cut.Whole).linesIterator
          .find(_.startsWith("- **Summary**")),
        shown(sent(last), Cut.Whole).contains("- **Summary**")
      ) ==> (
        Some(
          s"$line ${estimated(onPrefix)}. It was sent on the " +
            "answering call's prefix: messages 1–3 above, then its reply and the ask."
        ),
        Some(
          s"$line ${estimated(alone)}. It was asked alone: its own " +
            "system prompt and messages, not the turn's."
        ),
        false
      )
    }

    test(
      "a turn run under this build's epoch and a deployment's version is not said to differ; one of another epoch is"
    ) {
      val same = shown(sent(request(asked), epoch = s"${Turn.Epoch}+1"), Cut.Whole)
      val other = shown(sent(request(asked), epoch = "1970-01-01+1"), Cut.Whole)
      assert(
        same.contains(
          s"- Ran under epoch `${Turn.Epoch}+1`; this build is epoch `${Turn.Epoch}`.\n"
        ),
        other.contains("**they differ**")
      )
    }
  }
}
