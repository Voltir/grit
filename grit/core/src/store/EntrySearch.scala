package grit.core.store

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}

/** Ranks entries against a text query: a conversation's own, or other conversations' open
  * periods'. Read-only.
  */
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

  /** The entries of `open`'s periods (each conversation's turns from its period's `first`
    * on, so never a closing entry) that match `query`: best first, at most `limit`, and
    * equally good matches latest first. What is searched is as in [[search]]. Empty when
    * nothing matches, `query` is blank or `open` is empty.
    */
  def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
      Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]]

  /** The closing entries of `conversations` that match `query`: best first, at most `limit`,
    * and equally good matches latest first; each searched by what [[search]] searches of a
    * closing. Empty when nothing matches, `query` is blank or `conversations` is empty.
    */
  def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
      Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]]
}

object EntrySearch {

  /** A matching entry, the turn it belongs to, and how well it matched: positive, higher is
    * better. Hits for the same query read in one transaction share one scale, from
    * [[search]], [[nearby]] or [[closings]] alike.
    */
  final case class Hit(id: EntryId, turn: TurnRef, score: Double)
}
