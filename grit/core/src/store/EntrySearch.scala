package grit.core.store

import grit.core.id.{ConversationId, EntryId, TurnSeq}

/** Ranks a conversation's entries against a text query. Read-only. */
trait EntrySearch {

  /** The entries of `conversation`, in turns from `from` and before `before`, that match `query`: best
    * first, at most `limit`, and equally good matches latest first. Empty when nothing
    * matches or `query` is blank. What is searched is a message's text (the user's, the
    * reply's text blocks, a tool result) and a summary's; never reasoning, tool-call
    * arguments or any other payload.
    */
  def search(
      conversation: ConversationId,
      from: TurnSeq,
      before: TurnSeq,
      query: String,
      limit: Int
  )(using
      Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]]
}

object EntrySearch {

  /** A matching entry, its turn, and how well it matched: positive, higher is better, and
    * comparable only with other hits from the same search.
    */
  final case class Hit(id: EntryId, turnSeq: TurnSeq, score: Double)
}
