package grit.core.edge

import java.time.Instant

import grit.core.id.{ToolCallId, TurnRef}
import grit.core.identity.Account
import grit.core.store.{Entry, Payload, StoreError, Tx}

/** Which turns' replies an edge has yet to post outside grit, where each goes, and each part
  * of a reply as it is posted: what lets an edge that restarts post every reply, and post
  * none twice.
  */
trait Deliveries {

  /** Marks `turn`'s reply as awaited, to go where `to` says (the edge's own address, such as a
    * Slack channel and thread); nothing when it is already awaited or delivered.
    */
  def await(turn: TurnRef, to: String)(using Tx^): Either[StoreError, Unit]

  /** Every awaited turn not yet delivered, in the order it was awaited, with where it goes and
    * the parts of its reply being posted or posted.
    */
  def pending()(using Tx^): Either[StoreError, Vector[Pending]]

  /** Part `part` of `turn`'s reply is about to be posted: after a crash it is [[Part.Posting]],
    * and may or may not have reached the other side. `Invalid` when `turn` is not awaited.
    */
  def posting(turn: TurnRef, part: Int)(using Tx^): Either[StoreError, Unit]

  /** Part `part` of `turn`'s reply was posted, as `id` outside grit. `Invalid` when `turn` is
    * not awaited.
    */
  def posted(turn: TurnRef, part: Int, id: String)(using Tx^): Either[StoreError, Unit]

  /** `turn`'s reply is wholly delivered: it is no longer pending, and awaiting it again does
    * nothing. `Invalid` when `turn` is not awaited.
    */
  def delivered(turn: TurnRef)(using Tx^): Either[StoreError, Unit]

  /** `turn`'s open asks, oldest first: an ask whose call is settled, or whose `until` has passed,
    * is not among them, and nor is one recorded with no `until`, nor any of a turn whose first
    * entry no account wrote. Read in a transaction opened for `Subject.Turn(turn)`: under any
    * other subject the turn's entries are not read, and it answers none.
    */
  def asks(turn: TurnRef)(using Tx^): Either[StoreError, Vector[OpenAsk]]
}

/** An ask an edge is to show its turn's asker: the gated call `call`, `shown` as it asks,
  * waiting until `until`, put to `to`, the account that wrote the turn's first entry, through
  * which the edge reaches them.
  */
final case class OpenAsk(call: ToolCallId, shown: String, to: Account, until: Instant)

object OpenAsk {

  /** The open asks of a turn whose entries, in order, are `entries`, its first written by `to`,
    * at `now`, as [[Deliveries.asks]] lists them.
    */
  def of(entries: Vector[Entry], to: Option[Account], now: Instant): Vector[OpenAsk] =
    to.fold(Vector.empty[OpenAsk]) { by =>
      Payload
        .waiting(entries.map(_.payload), now)
        .flatMap(ask => ask.until.map(OpenAsk(ask.call, ask.shown, by, _)))
    }
}

/** An awaited turn: where its reply goes, and each part begun, by its number from 0. */
final case class Pending(turn: TurnRef, to: String, parts: Map[Int, Part])

/** A part of a reply an edge has begun to post. */
enum Part {

  /** Being posted: it may or may not have reached the other side. */
  case Posting

  /** Posted, as `id` outside grit. */
  case Posted(id: String)
}
