package grit.core.recipe

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.place.Place
import grit.core.stitch.Said
import grit.core.store.{StoreError, Tx}

/** What a room said, as a pool reads it. A read of messages takes those its transaction's
  * clearance reads ([[grit.core.store.Tx.clearance]]), said in conversations at places within
  * `room`, in [`from`, `until`), in none of `outside`: each person's message,
  * to grit or heard, and grit's replies and posts, never a draft or a closing. It keeps the
  * latest `most` (none when `most` is under 1), latest first; of two said at once, the greater
  * entry id first.
  */
trait RoomReads {

  /** The messages anyone said. */
  def said(
      room: Place,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using Tx^): Either[StoreError, Vector[Said]]

  /** The messages `author` wrote. */
  def saidBy(
      room: Place,
      author: PrincipalId,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using Tx^): Either[StoreError, Vector[Said]]

  /** Who wrote `entry`, when it is an inbound message still kept. */
  def author(entry: EntryId)(using Tx^): Either[StoreError, Option[PrincipalId]]
}
