package grit.core.speech

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.spend.{Day, Spend}
import grit.core.store.{StoreError, Tx}

/** Where heard messages could be answered, and grit's decisions on each: kept as long as its
  * period's usage is (the ledger), so the rates and the day's speech spend count every one, and
  * each unprompted reply can be stated with how it was approved.
  */
trait SpeechStore {

  /** Keeps `reach` for the heard message that is `turn`'s first entry: where a reply to it
    * could go, and whom it names. Kept once: a later reach for the same turn is ignored.
    * `Invalid` when `turn`'s first entry is not a heard message.
    */
  def heard(turn: TurnRef, reach: Reach)(using Tx^): Either[StoreError, Unit]

  /** The reach kept for the heard message that is `turn`'s first entry; `None` when it is
    * gone or was heard before reach was kept.
    */
  def reach(turn: TurnRef)(using Tx^): Either[StoreError, Option[Reach]]

  /** Every unprompted turn decided at or after `since`, oldest first, at its stage: none
    * answered as said to grit ([[Decision.Answering]]), which, as an addressed turn, counts
    * in no rate.
    */
  def spoken(since: Instant)(using Tx^): Either[StoreError, Vector[Spoken]]

  /** What the unprompted turns' recorded calls cost on `day`: their drafts, queries, topics,
    * summaries and judges ([[grit.core.store.UsageLedger]]). A turn answered as said to grit
    * ([[Decision.Answering]]) is not among them: as an addressed turn, it counts only in the
    * deployment's budget.
    */
  def spentOn(day: Day)(using Tx^): Either[StoreError, Spend]

  /** Keeps `decision` on the heard message `heard`, made `at`, with what triage said;
    * `false`, writing nothing, when one is kept for it already.
    */
  def decided(heard: Heard, decision: Decision, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Whether `turn` was decided to be answered as said to grit ([[Decision.Answering]]);
    * `false` for any other turn, decided on or not.
    */
  def answering(turn: TurnRef)(using Tx^): Either[StoreError, Boolean]

  /** Keeps what became of the turn `turn`, drafting or answering (`outcome`, settled `at`),
    * with the first [[SpeechStore.Excerpt]] characters of `draft`, its draft or reply. A `Posted` outcome keeps
    * its reply's position ([[TurnRef.replyId]]), which must be written first. `false`,
    * writing nothing, when an outcome is kept already; `Invalid` when `turn` was never
    * decided on, or is posted with no reply written.
    */
  def drafted(turn: TurnRef, outcome: Outcome, draft: Option[String], at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Deletes the decisions kept on `conversation`'s turns `from` to `to`, both included: what
    * goes with those turns' usage ([[grit.core.store.UsageLedger.forget]]).
    */
  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit]
}

object SpeechStore {

  /** How much of a draft is kept with its outcome. */
  val Excerpt = 500
}
