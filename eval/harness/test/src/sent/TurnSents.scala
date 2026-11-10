package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.provider.ModelRequest
import grit.core.tool.ToolSetId
import grit.turn.{Turn, TurnLoop, TurnOffer, TurnRecord, TurnSummary}

/** A recorded turn built by hand, for the suites that read one as its markdown ([[SentMarkdown]]). */
object TurnSents {

  val turn = TurnRef(ConversationId("c1"), TurnSeq(0))

  val asked = Message.User("read it")

  val calling: Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj("path" -> "a.txt"))),
    StopReason.ToolUse,
    Usage.Zero,
    "m"
  )

  def result(content: String) = Message.ToolResult(ToolCallId("t1"), content, false)

  def request(messages: Message*) = ModelRequest("sys", messages.toVector)

  val answer: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("done")), StopReason.EndTurn, Usage.Zero, "m")

  /** Its ledger row for a call whose request estimated as `estimated`. */
  def row(entry: EntryId, estimated: Tokens) =
    entry -> Spent(None, "m", Usage(Tokens(10), Tokens(1), Tokens.Zero, None), estimated)

  /** A turn of one tool round, then an answer sent `last`; its rows recorded `first` and
    * `answered` as their estimates; run under `epoch`; its summary rebuilt as `summary`, its
    * row recording that request's estimate.
    */
  def sent(
      last: ModelRequest,
      first: ModelRequest = request(asked),
      recorded: Option[Tokens] = None,
      epoch: String = Turn.Epoch,
      summary: Option[ModelRequest] = None
  ): TurnSent = {
    val looped = TurnRecord.Call(0, EntryId("call:c1:0:0"), first)
    val told = last.copy(messages = last.messages :+ Message.User(TurnLoop.LastCall))
    val answered = TurnRecord.Answered(1, turn.replyId, last, told)
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
}
