package grit.core.inbox

import grit.core.id.{ConversationId, EntryId, SourceId}

/** The id an inbound message's entry is kept under: its conversation and its source's id there
  * (an edge's own, e.g. a Slack message's ts). Defined once; [[source]] reads it back.
  */
object InboundId {

  def of(conversation: ConversationId, source: SourceId): EntryId =
    EntryId(s"$Prefix${ConversationId.value(conversation)}:${SourceId.value(source)}")

  /** The conversation and source `id` was made from; `None` when it is not an inbound id.
    * Read back whole only when the conversation's id holds no `:` (grit's are UUIDs).
    */
  def source(id: EntryId): Option[(ConversationId, SourceId)] = {
    val s = EntryId.value(id)
    Option
      .when(s.startsWith(Prefix))(s.drop(Prefix.length))
      .flatMap { rest =>
        rest.indexOf(':') match {
          case i if i > 0 => Some((ConversationId(rest.take(i)), SourceId(rest.drop(i + 1))))
          case _ => None
        }
      }
  }

  private val Prefix = "in:"
}
