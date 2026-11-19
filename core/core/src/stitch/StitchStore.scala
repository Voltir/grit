package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, SourceId}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}

/** Where each stitchable conversation's first message was placed among the exchanges of its
  * room ([[Stitching]]), kept beside that entry and deleted with it; and what stitching reads
  * of a room, which is only the entries its transaction's clearance reads
  * ([[grit.core.store.Tx.clearance]]).
  */
trait StitchStore {

  /** Keeps `placed` for `root`, the first entry of its conversation, made `at`. `false`,
    * writing nothing, when one is kept for `root` already or `root` is gone. `Invalid` when
    * `root` is not its conversation's first entry. Deleted with `root`, or with the
    * conversation it follows.
    */
  def record(root: EntryId, placed: Placed, at: Instant)(using Tx^): Either[StoreError, Boolean]

  /** The placement kept for `root`, if any. */
  def placed(root: EntryId)(using Tx^): Either[StoreError, Option[Placed]]

  /** What was said in conversations at places within `room` from `from` until before `until`
    * that its transaction reads, oldest first: each person's message, to grit or heard, grit's
    * replies and posts ([[grit.core.store.Payload.Posted]]), and each closing entry; never a
    * draft.
    */
  def spokenIn(room: Place, from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]]

  /** What was said in `conversations` from `from` until before `until` that its transaction
    * reads, oldest first: each person's message, to grit or heard, and grit's replies and posts.
    */
  def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]]

  /** The first message said in each of `conversations` whose first message is still kept and
    * read by its transaction.
    */
  def openings(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Said]]

  /** Each message, reply and post said in a channel's top level or a thread under it
    * ([[grit.core.store.Origin.Room]], [[grit.core.store.Origin.Thread]]) from `from` until
    * before `until` that its transaction reads, oldest first, as [[said]] reads them; no closing
    * entry.
    */
  def inRooms(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Said]]

  /** The person's message `conversation` holds from `source` (kept under its inbound id,
    * [[grit.core.inbox.InboundId]]), heard or said to grit, when its transaction reads it;
    * `None` for grit's own reply or post, any other entry, or none.
    */
  def asked(conversation: ConversationId, source: SourceId)(using
      Tx^
  ): Either[StoreError, Option[Said]]

  /** Every link begun by, or naming as its root, any of `conversations`. */
  def links(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Link]]

  /** Every link whose conversation, or whose root, had a message said from `from` until before
    * `until` that its transaction reads, each once, oldest placed first. "Said" as [[said]]
    * reads it: people's messages, grit's replies and posts.
    */
  def touched(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Link]]
}
