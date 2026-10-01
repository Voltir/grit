package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}

/** Where each stitchable conversation's first message was placed among the exchanges of its
  * room ([[Stitching]]), kept beside that entry and deleted with it; and what stitching reads
  * of a room.
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

  /** What was said in conversations at places within `room` from `from` until before `until`,
    * oldest first: each person's message, to grit or heard, grit's replies and posts
    * ([[grit.core.store.Payload.Posted]]), and each closing entry; never a draft.
    */
  def spokenIn(room: Place, from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]]

  /** What was said in `conversations` from `from` until before `until`, oldest first: each
    * person's message, to grit or heard, and grit's replies and posts.
    */
  def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]]

  /** The first message said in each of `conversations` whose first message is still kept. */
  def openings(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Said]]

  /** Every link begun by, or naming as its root, any of `conversations`. */
  def links(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Link]]
}
