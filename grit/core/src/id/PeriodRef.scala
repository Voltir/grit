package grit.core.id

/** One period of one conversation. */
final case class PeriodRef(conversationId: ConversationId, seq: PeriodSeq) {

  /** The id of its closing entry, `closing:{conversationId}:{seq}`. */
  def closingId: EntryId =
    EntryId(s"closing:${ConversationId.value(conversationId)}:${PeriodSeq.value(seq)}")
}
