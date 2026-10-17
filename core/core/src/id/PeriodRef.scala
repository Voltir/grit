package grit.core.id

/** One period of one conversation. */
final case class PeriodRef(conversationId: ConversationId, seq: PeriodSeq) {

  /** The id of its closing entry, `closing:{conversationId}:{seq}`. */
  def closingId: EntryId =
    EntryId(s"closing:${ConversationId.value(conversationId)}:${PeriodSeq.value(seq)}")

  /** What the workflow id of every `name` workflow about this period starts with, and no other
    * period's: `name:{conversationId}:{seq}:`. Durable: DBOS keeps those ids.
    */
  def workflowPrefix(name: String): String =
    s"$name:${ConversationId.value(conversationId)}:${PeriodSeq.value(seq)}:"
}

object PeriodRef {

  /** The period a workflow id's conversation and sequence parts spell, or `None` if `seq` is
    * not a period's sequence. `conversation` is non-empty; the caller checks that.
    */
  private[id] def parse(conversation: String, seq: String): Option[PeriodRef] =
    seq.toLongOption.flatMap(PeriodSeq.of).map(PeriodRef(ConversationId(conversation), _))
}
