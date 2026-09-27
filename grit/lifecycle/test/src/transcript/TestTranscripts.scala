package grit.lifecycle.transcript

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Entry, Payload}

/** Entries of one period, as a transcript test writes them: `seq` orders them. */
object TestTranscripts {

  def entry(seq: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$seq"), ConversationId("c"), TurnSeq(0), None, seq, payload, Instant.EPOCH)

  /** The person's message `text`. */
  def said(seq: Long, text: String): Entry = entry(seq, Payload.Message(Message.User(text)))

  /** The assistant's reply `text`. */
  def replied(seq: Long, text: String): Entry = entry(
    seq,
    Payload.Message(
      Message.Assistant(
        Vector(AssistantBlock.Text(text)),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
    )
  )

  /** A tool call shown as `shown`, its result `content`, an error when `error`. */
  def result(seq: Long, shown: String, content: String, error: Boolean = false): Entry =
    entry(seq, Payload.Result(Message.ToolResult(ToolCallId(s"c$seq"), content, error), shown))

  /** `entries` as the writer's transcript. */
  def labelled(entries: Entry*): Labelled = PeriodTranscript.labelled(entries.toVector)
}
