package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, QuestionName, ShadowName}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.store.{Origin, StoreError, Tx}

/** What a person said, by reaction, of grit's speech decision on a heard message. */
enum Verdict extends caps.Pure {

  /** A reply there would have been welcome: the message was to the room. */
  case Welcome

  /** A reply would have interrupted, though the message was to the room. */
  case Interruption

  /** The message was meant for someone in particular, even if an answer would have helped. */
  case CutIn

  /** Whether a reply would have been right: only for `Welcome`. */
  def speaks: Boolean = this == Welcome

  /** Whether the message was meant for a person rather than the room: only for `CutIn`. */
  def toPerson: Boolean = this == CutIn
}

/** The verdict standing on a prompt: `rater`'s, given `at`. */
final case class Label(verdict: Verdict, rater: Account, at: Instant)

/** A picked message's prompt as its edge posts it: the heard `entry`, in the conversation
  * `origin` names; why it was picked against `shadow`; live's settled decision; and what the
  * shadow answered. It holds neither the message's text nor a draft's.
  */
final case class Prompt(
    entry: EntryId,
    origin: Origin,
    shadow: ShadowName,
    reason: Reason,
    live: Settled,
    answers: VectorMap[QuestionName, Answer]
) extends caps.Pure

/** A heard message a review considered against `shadow`, at `at`, as `as`, with the label
  * standing on its prompt.
  */
final case class Reviewed(
    entry: EntryId,
    conversation: ConversationId,
    shadow: ShadowName,
    at: Instant,
    as: Considered,
    label: Option[Label]
)

/** What an edge does with one kind of review: posts the prompts of subjects `S` waiting for a
  * place, and keeps the verdict `V` their rater reacted with, each by the address its edge
  * posted at.
  */
trait Reviews[S <: caps.Pure, V <: caps.Pure] {

  /** Every subject waiting whose prompt `to` may receive: `to` is writable ([[Tx.writable]]) at
    * a label dominating the subject's, and the subject is one the transaction reads; none when
    * `to` is not writable. The earliest waiting first. A subject holding text (a document's) is
    * read only in a transaction cleared for `to`'s label (`Db.read` at `to`'s clearance, never
    * `Subject.Public`, which reads public rows alone).
    */
  def unposted(to: Place)(using Tx^): Either[StoreError, Vector[S]]

  /** Keeps that `subject`'s prompt was posted at `address`, its edge's own address form (as
    * [[grit.core.speech.Reach.replyTo]]), `at`; `false`, writing nothing, when it is posted
    * already, was never waiting, or another prompt is at `address`.
    */
  def posted(subject: S, address: String, at: Instant)(using Tx^): Either[StoreError, Boolean]

  /** Keeps that `subject`'s prompt will never be posted, `at`, as when it is gone from its edge:
    * [[unposted]] offers it no more, and [[posted]] refuses it. `false`, writing nothing, when it
    * is posted or dropped already, or was never waiting.
    */
  def dropped(subject: S, at: Instant)(using Tx^): Either[StoreError, Boolean]

  /** Keeps `verdict` by `rater`, given `at`, on the prompt posted at `address`, replacing the
    * one standing; `false`, writing nothing, when no prompt is there.
    */
  def reacted(address: String, rater: Account, verdict: V, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Withdraws the verdict standing on the prompt posted at `address` when it is `verdict` by
    * `rater`; `false`, writing nothing, otherwise.
    */
  def unreacted(address: String, rater: Account, verdict: V)(using
      Tx^
  ): Either[StoreError, Boolean]
}

object Reviews {

  /** `reviews` over subjects `T` and verdicts `W`: each waiting subject `read` (one it reads as
    * `None` is left waiting, never offered), each `T` posted or dropped as the `S` it was read
    * from (`from`), each `W` kept as `verdict` of it.
    */
  def as[S <: caps.Pure, V <: caps.Pure, T <: caps.Pure, W <: caps.Pure](reviews: Reviews[S, V])(
      read: S -> Option[T],
      from: T -> S,
      verdict: W -> V
  ): Reviews[T, W] = new As(reviews, read, from, verdict)

  private final class As[S <: caps.Pure, V <: caps.Pure, T <: caps.Pure, W <: caps.Pure](
      reviews: Reviews[S, V],
      read: S -> Option[T],
      from: T -> S,
      verdict: W -> V
  ) extends Reviews[T, W] {

    def unposted(to: Place)(using Tx^): Either[StoreError, Vector[T]] =
      reviews.unposted(to).map(_.flatMap(read(_)))

    def posted(subject: T, address: String, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = reviews.posted(from(subject), address, at)

    def dropped(subject: T, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      reviews.dropped(from(subject), at)

    def reacted(address: String, rater: Account, w: W, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = reviews.reacted(address, rater, verdict(w), at)

    def unreacted(address: String, rater: Account, w: W)(using
        Tx^
    ): Either[StoreError, Boolean] = reviews.unreacted(address, rater, verdict(w))
  }
}

/** Reviews of heard messages' triage, as the engine keeps them: an edge's part, whose prompts
  * are offered the earliest picked first (ties by entry id), one whose heard message is gone
  * never, and kept with their message's conversation after its entry is gone, by entry alone;
  * and which messages are considered for a prompt, which a deployment's kit decides
  * ([[Review.pick]]).
  */
trait ReviewStore extends Reviews[Prompt, Verdict] {

  /** Up to `limit` heard messages said at or after `since` that `shadow` answered as a set,
    * whose live decision is settled, that no review has considered, and that `to` may receive
    * (as [[Reviews.unposted]]); the earliest said first (ties by entry id).
    */
  def candidates(shadow: ShadowName, since: Instant, limit: Int, to: Place)(using
      Tx^
  ): Either[StoreError, Vector[Candidate]]

  /** Keeps what a pick round made of `candidate`, at `at`; `false`, writing nothing, when it
    * was considered already.
    */
  def considered(candidate: Candidate, as: Considered, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Every message considered at or after `since`, the earliest considered first (ties by entry
    * id).
    */
  def reviewed(since: Instant)(using Tx^): Either[StoreError, Vector[Reviewed]]
}
