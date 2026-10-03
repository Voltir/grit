package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, PrincipalId, QuestionName, ShadowName}
import grit.core.store.{Origin, StoreError, Tx}

/** What a person said, by reaction, of grit's speech decision on a heard message. */
enum Verdict {

  /** A reply there would have been welcome: the message was to the room. */
  case Welcome

  /** A reply would have interrupted, though the message was to the room. */
  case Interruption

  /** The message was meant for someone in particular, even if an answer would have helped. */
  case CutIn
}

/** The verdict standing on a prompt: `rater`'s, given `at`. */
final case class Label(verdict: Verdict, rater: PrincipalId, at: Instant)

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
)

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

/** What an edge does with reviews: posts picked prompts and keeps what their rater reacted
  * with. Prompts and labels are kept with their message's conversation, after its entry is
  * gone.
  */
trait Reviews {

  /** Every picked prompt not yet posted, the earliest picked first (ties by entry id); one
    * whose heard message is gone is not offered.
    */
  def unposted()(using Tx^): Either[StoreError, Vector[Prompt]]

  /** Keeps that `entry`'s prompt was posted at `address`, its edge's own address form (as
    * [[grit.core.speech.Reach.replyTo]]), `at`; `false`, writing nothing, when it is posted
    * already, was never picked, or another prompt was posted at `address`.
    */
  def posted(entry: EntryId, address: String, at: Instant)(using Tx^): Either[StoreError, Boolean]

  /** Keeps `verdict` by `rater`, given `at`, on the prompt posted at `address`, replacing the
    * label standing; `false`, writing nothing, when no prompt was posted there.
    */
  def reacted(address: String, rater: PrincipalId, verdict: Verdict, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Withdraws the label standing on the prompt posted at `address` when it is `verdict` by
    * `rater`; `false`, writing nothing, otherwise.
    */
  def unreacted(address: String, rater: PrincipalId, verdict: Verdict)(using
      Tx^
  ): Either[StoreError, Boolean]
}

/** Reviews as the engine keeps them: an edge's part, and which messages are considered for a
  * prompt, which a deployment's kit decides ([[Review.pick]]).
  */
trait ReviewStore extends Reviews {

  /** Up to `limit` heard messages said at or after `since` that `shadow` answered as a set,
    * whose live decision is settled and that no review has considered; the earliest said first
    * (ties by entry id).
    */
  def candidates(shadow: ShadowName, since: Instant, limit: Int)(using
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
